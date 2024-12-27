(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

let ( let* ) = Result.bind

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
  let act a = Htmlact.request ~method':`PUT (Example.uf urlf "?action=%s" a) in
  let sel = ":up :up form" in
  let target = Htmlact.target sel in
  let query = Htmlact.query sel in
  let effect = Htmlact.effect `Element in
  let at = [target; effect; query] in
  let alive = Example.button ~at:(act "set-alive" :: at) "Set alive" in
  let broken = Example.button ~at:(act "set-broken" :: at) "Set broken" in
  El.div [alive; broken]

let update_bookmark_status request =
  let* query = Http.Request.to_query request in
  try
    let failwithf fmt = Printf.ksprintf failwith fmt in
    let ids =
      let parse_id id = try int_of_string (String.trim id) with
      | Failure _ -> failwithf "%s: not an id" id
      in
      List.map parse_id (Http.Query.find_all "ids" query)
    in
    let select_ids b = List.mem b.Bookmark.id ids in
    let broken = match Http.Query.find_first "action" query with
    | Some "set-alive" -> false
    | Some "set-broken" -> true
    | Some a -> failwithf "%S: unknown action" a
    | None -> failwithf "missing action"
    in
    let update_broken b = { b with Bookmark.broken = broken } in
    let bs = List.filter select_ids (Bookmark.all ()) in
    let bs = List.map update_broken bs in
    let () = List.iter Bookmark.set bs in
    Ok ids
  with
  | Failure e -> Http.Response.bad_request_400 ~log:e ()

let index ~urlf =
  let content = El.splice [table_view (Bookmark.all ()); actions urlf] in
  Example.html_page ~style ~id ~title:name [description; content]

let bookmark_table ~urlf request =
  let* method' = Http.Request.allow Http.Method.[get; put] request in
  match method' with
  | `GET -> Ok (Http.Response.html Http.Status.ok_200 (index ~urlf))
  | `PUT ->
      let* updated = update_bookmark_status request in
      let updated b = List.exists (fun id -> b.Bookmark.id = id) updated in
      let table = Example.html [table_view ~updated (Bookmark.all ())] in
      Ok (Http.Response.html Http.Status.ok_200 table)

let serve request =
  let urlf = Example.urlf request ~prefix in
  match Example.path ~prefix request with
  | [""] -> bookmark_table ~urlf request
  | p -> Http.Response.not_found_404 ()
