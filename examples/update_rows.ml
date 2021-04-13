(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_html

let ( let* ) = Result.bind
let strf = Printf.sprintf

let id = __MODULE__
let name = "Update rows"
let synopsis = "Update selected table rows."
let prefix = "update-rows"
let description = Example.description [ Ht.txt
  "The table is wrapped in a form. When an action button is clicked
   the selected ids are sent to the server for update. The resulting
   table is sent back in the response." ]

let style = {css|
tr td:nth-child(1) { width:5%; }
tr td:nth-child(2) { width:20%; }
tr td:nth-child(3) { width:75%; }
tr.broken td:nth-child(2) { color: rgb(var(--redish)); }
tr { transition: all 1000ms; }
form.hc-outro tr.updated.broken { background: rgba(var(--redish), 0.1); }
form.hc-outro tr.updated.alive { background: rgba(var(--greenish), 0.1); }
|css}

let bookmark_selector b =
  let bid = string_of_int b.Bookmark.id in
  Ht.input ~at:At.[type' "checkbox"; name "ids"; value bid] ()

let selectable_bookmark ?(updated = fun _ -> false) b =
  let status = if b.Bookmark.broken then "broken" else "alive" in
  Ht.tr ~at:At.(add_if (updated b) (class' "updated") [class' status])
    [ Ht.td [bookmark_selector b];
      Ht.td [Ht.txt (String.capitalize_ascii status)];
      Ht.td [Ht.a ~at:At.[href b.Bookmark.link] [Ht.txt b.Bookmark.name]]]

let table_headers =
  let cell txt = Ht.th [Ht.txt txt] in
  Ht.tr [Ht.th []; cell "Status"; cell "Bookmark";]

let table_view ?updated bs =
  Ht.form [ Ht.table [
      Ht.thead [table_headers];
      Ht.tbody (List.map (selectable_bookmark ?updated) bs)]]

let actions urlf =
  let act a = Hc.request ~meth:`PUT (Example.uf urlf "?action=%s" a) in
  let target = ":up :up form" in
  let t = Hc.target target and q = Hc.query target in
  let e = Hc.effect ~outro_ms:500 `Inplace in
  let alive = Example.button ~at:[act "set-alive"; t; e; q] "Set alive" in
  let broken = Example.button ~at:[act "set-broken"; t; e; q] "Set broken" in
  Ht.div [alive; broken]

let index urlf =
  let content = Ht.splice [table_view (Bookmark.all ()); actions urlf] in
  Example.page ~style ~id ~title:name [description; content]

let update_bookmark_status r =
  let* q = Req.to_query r in
  try
    let broken = match Http.Query.find "action" q with
    | Some "set-alive" -> false
    | Some "set-broken" -> true
    | Some a -> failwith (strf "%S: unknown action" a)
    | None -> failwith (strf "missing action")
    in
    let ids =
      let parse_id id = try int_of_string (String.trim id) with
      | Failure _ -> failwith (strf "%s: not an id" id)
      in
      List.map parse_id (Http.Query.find_all "ids" q)
    in
    let bs = Bookmark.all () in
    let ubs = List.filter (fun b -> List.mem b.Bookmark.id ids) bs in
    let ubs = List.map (fun b -> { b with Bookmark.broken = broken }) ubs in
    let () = List.iter Bookmark.set ubs in
    Ok ids
  with
  | Failure explain -> Error (Resp.v ~explain Http.s400_bad_request)

let bookmark_table r =
  let* m = Req.Allow.(meths [get; put] r) in
  match m with
  | `GET -> Ok (Resp.html Http.s200_ok (index (Example.urlf r)))
  | `PUT ->
      let* updated = update_bookmark_status r in
      let updated b = List.exists (fun id -> b.Bookmark.id = id) updated in
      let table = Example.part [table_view ~updated (Bookmark.all ())] in
      Ok (Resp.html Http.s200_ok table)

let serve r = match Req.path r with
| [""] -> bookmark_table r
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
