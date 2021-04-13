(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_html

let ( let* ) = Result.bind
let strf = Printf.sprintf

let id = __MODULE__
let name = "Delete rows"
let synopsis = "Delete a table row."
let prefix = "delete-rows"
let description = Example.description [ Ht.txt
  "Each line has a button that sends a delete request for the bookmark.
   The empty response deletes the table row. An effect introduction duration
   allows to fade out the row before it is replaced by nothing." ]

let style = {css|
tr td:nth-child(1) { min-width:10ex; }
tr td:nth-child(2) { min-width:20ex; }
tr.broken td:nth-child(1) { color: rgba(var(--redish)); }
tr.hc-out { opacity: 0; transition: all var(--dur-medium) ease-out; }
|css}

let delete_bookmark urlf b =
  let url = Example.uf urlf "bookmark/%d" b.Bookmark.id in
  let r = Hc.request ~meth:`DELETE url in
  let t = Hc.target "tr:up" and e = Hc.effect `Inplace in
  Example.button ~at:[r; t; e] "Delete"

let deletable_bookmark urlf b =
  let status = if b.Bookmark.broken then "broken" else "alive" in
  Ht.tr ~at:At.[class' status]
    [ Ht.td [Ht.txt (String.capitalize_ascii status)];
      Ht.td [Ht.a ~at:At.[href b.Bookmark.link] [Ht.txt b.Bookmark.name]];
      Ht.td [delete_bookmark urlf b]; ]

let table_headers =
  let cell txt = Ht.th [Ht.txt txt] in
  Ht.tr [cell "Status"; cell "Bookmark"; Ht.th []]

let table_view urlf bs =
  Ht.table [
    Ht.thead [table_headers];
    Ht.tbody (List.map (deletable_bookmark urlf) bs)]

let actions urlf =
  let r = Hc.request ~meth:`POST (Example.uf urlf "?action=restore") in
  let t = Hc.target ":up :up table" in
  let e = Hc.effect `Inplace in
  let restore = Example.button ~at:[r; t; e] "Restore deleted" in
  Ht.div [restore]

let index r =
  let urlf = Example.urlf r in
  let* m = Req.Allow.(meths [get;post] r) in
  match m with
  | `GET ->
      let bookmarks = table_view urlf (Bookmark.all ()) in
      let content = Ht.splice [bookmarks; actions urlf] in
      let page = Example.page ~style ~id ~title:name [description; content] in
      Ok (Resp.html Http.s200_ok page)
  | `POST ->
      let* q = Req.to_query r in
      match Http.Query.find "action" q with
      | Some "restore" ->
          Bookmark.restore_deleted ();
          let part = Example.part [table_view urlf (Bookmark.all ())] in
          Ok (Resp.html Http.s200_ok part)
      | v ->
        let some = strf "unknown action %s" in
        let explain = Option.fold ~none:"missing action" ~some v in
        Error (Resp.v ~explain Http.s400_bad_request)

let bookmark_delete id r = match int_of_string id with
| exception Failure _ ->
    Error (Resp.v ~explain:"illegal id" Http.s400_bad_request)
| id ->
    let* m = Req.Allow.(meths [delete] r) in
    match m with
    | `DELETE ->
        match Bookmark.get id with
        | None -> Ok (Resp.html Http.s200_ok (Example.part []))
        | Some id ->
            Bookmark.delete id;
            Ok (Resp.html Http.s200_ok (Example.part []))

let serve r = match Req.path r with
| [""] -> index r
| ("bookmark" :: [id]) -> bookmark_delete id r
| p -> Ok (Resp.v Http.s404_not_found)

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
