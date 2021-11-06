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
let description = Example.description [ El.txt
  "Each line has a button that sends a delete request for the bookmark.
   The empty response deletes the table row." ]

let style = {css|
tr td:nth-child(1) { min-width:10ex; }
tr td:nth-child(2) { min-width:20ex; }
tr.broken td:nth-child(1) { color: rgba(var(--redish)); }

tr { transition: all var(--dur-medium); }
tr.hc-out { opacity: 0 }
.hc-in tbody tr
{ opacity: 0; transform: translateY(calc(-1 * var(--size-half-line))) }
|css}

let delete_bookmark urlf b =
  let url = Example.uf urlf "bookmark/%d" b.Bookmark.id in
  let r = Hc.request ~meth:`DELETE url in
  let t = Hc.target "tr:up" and e = Hc.effect `Element in
  Example.button ~at:[r; t; e] "Delete"

let deletable_bookmark urlf b =
  let status = if b.Bookmark.broken then "broken" else "alive" in
  El.tr ~at:At.[class' status]
    [ El.td [El.txt (String.capitalize_ascii status)];
      El.td [El.a ~at:At.[href b.Bookmark.link] [El.txt b.Bookmark.name]];
      El.td [delete_bookmark urlf b]; ]

let table_headers =
  let cell txt = El.th [El.txt txt] in
  El.tr [cell "Status"; cell "Bookmark"; El.th []]

let table_view urlf bs =
  let bs = List.map (deletable_bookmark urlf) bs in
  El.table [ El.thead [table_headers]; El.tbody bs]

let actions urlf =
  let r = Hc.request ~meth:`POST (Example.uf urlf "?action=restore") in
  let t = Hc.target ":up :up table" and e = Hc.effect `Element in
  let restore = Example.button ~at:[r; t; e] "Restore deleted" in
  El.div [restore]

let index r =
  let urlf = Example.urlf r in
  let* m = Http.Req.allow Http.Meth.[get;post] r in
  match m with
  | `GET ->
      let bookmarks = table_view urlf (Bookmark.all ()) in
      let content = El.splice [bookmarks; actions urlf] in
      let page = Example.page ~style ~id ~title:name [description; content] in
      Ok (Http.Resp.html Http.ok_200 page)
  | `POST ->
      let* q = Http.Req.to_query r in
      match Http.Query.find "action" q with
      | Some "restore" ->
          Bookmark.restore_deleted ();
          let part = Example.part [table_view urlf (Bookmark.all ())] in
          Ok (Http.Resp.html Http.ok_200 part)
      | v ->
          let some = strf "unknown action %s" in
          let explain = Option.fold ~none:"missing action" ~some v in
          Http.Resp.bad_request_400 ~explain ()

let bookmark_delete id r = match int_of_string_opt id with
| None -> Http.Resp.bad_request_400 ~explain:"illegal id" ()
| Some id ->
    let* `DELETE = Http.Req.allow Http.Meth.[delete] r in
    match Bookmark.get id with
    | None -> Ok (Http.Resp.html Http.ok_200 (Example.part []))
    | Some id ->
        Bookmark.delete id;
        Ok (Http.Resp.html Http.ok_200 (Example.part []))

let serve r = match Http.Req.path r with
| [""] -> index r
| ("bookmark" :: [id]) -> bookmark_delete id r
| p -> Http.Resp.not_found_404 ()

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
