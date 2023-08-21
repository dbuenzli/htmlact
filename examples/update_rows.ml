(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

let ( let* ) = Result.bind
let strf = Printf.sprintf

let id = __MODULE__
let name = "Update rows"
let synopsis = "Update selected table rows."
let prefix = "update-rows"
let description = Example.description [ El.txt
  "The table is wrapped in a form. When an action button is clicked
   the selected ids are sent to the server for update. The resulting
   table is sent back in the response." ]

let style = {css|
tr td:nth-child(1) { width:5%; }
tr td:nth-child(2) { width:20%; }
tr td:nth-child(3) { width:75%; }
tr.broken td:nth-child(2) { color: rgb(var(--redish)); }

.htmlact-in tr.updated.broken { background: rgba(var(--redish), 0.1); }
.htmlact-in tr.updated.alive { background: rgba(var(--greenish), 0.1); }
tr { transition: all 1s; }
|css}

let bookmark_selector b =
  let bid = string_of_int b.Bookmark.id in
  El.input ~at:At.[type' "checkbox"; name "ids"; value bid] ()

let selectable_bookmark ?(updated = fun _ -> false) b =
  let status = if b.Bookmark.broken then "broken" else "alive" in
  let at = At.if' (updated b) (At.class' "updated") :: At.class' status :: [] in
  El.tr ~at
    [ El.td [bookmark_selector b];
      El.td [El.txt (String.capitalize_ascii status)];
      El.td [El.a ~at:At.[href b.Bookmark.link] [El.txt b.Bookmark.name]]]

let table_headers =
  let cell txt = El.th [El.txt txt] in
  El.tr [El.th []; cell "Status"; cell "Bookmark";]

let table_view ?updated bs =
  El.form [ El.table [
      El.thead [table_headers];
      El.tbody (List.map (selectable_bookmark ?updated) bs)]]

let actions urlf =
  let act a = Htmlact.request ~meth:`PUT (Example.uf urlf "?action=%s" a) in
  let t = ":up :up form" in
  let t = Htmlact.target t and q = Htmlact.query t in
  let e = Htmlact.effect `Element in
  let alive = Example.button ~at:[act "set-alive"; t; e; q] "Set alive" in
  let broken = Example.button ~at:[act "set-broken"; t; e; q] "Set broken" in
  El.div [alive; broken]

let update_bookmark_status r =
  let* q = Http.Request.to_query r in
  try
    let broken = match Http.Query.find_first "action" q with
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
  | Failure explain -> Http.Response.bad_request_400 ~explain ()

let index urlf =
  let content = El.splice [table_view (Bookmark.all ()); actions urlf] in
  Example.page ~style ~id ~title:name [description; content]

let bookmark_table r =
  let* m = Http.Request.allow Http.Method.[get; put] r in
  match m with
  | `GET -> Ok (Http.Response.html Http.Status.ok_200 (index (Example.urlf r)))
  | `PUT ->
      let* updated = update_bookmark_status r in
      let updated b = List.exists (fun id -> b.Bookmark.id = id) updated in
      let table = Example.part [table_view ~updated (Bookmark.all ())] in
      Ok (Http.Response.html Http.Status.ok_200 table)

let serve r = match Http.Request.path r with
| [""] -> bookmark_table r
| p -> Http.Response.not_found_404 ()
