(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

let ( let* ) = Result.bind

let id = __MODULE__
let name = "Delete rows"
let synopsis = "Delete a table row."
let prefix = "delete-rows"
let description = Example.description [
    El.txt "Each line has a button that sends a delete request for the \
            bookmark. The empty response deletes the table row." ]

let style = {css|
tr td:nth-child(1) { min-width:10ex; }
tr td:nth-child(2) { min-width:20ex; }
tr.broken td:nth-child(1) { color: rgba(var(--redish)); }

tr { transition: all var(--dur-medium); }
tr.htmlact-out { opacity: 0 }
.htmlact-in tbody tr
{ opacity: 0; transform: translateY(calc(-1 * var(--size-half-line))) }
|css}

let delete_bookmark ~urlf b =
  let url = Example.uf urlf "/bookmark/%d" b.Bookmark.id in
  let request = Htmlact.request ~method':`DELETE url in
  let target = Htmlact.target "tr:up" in
  let effect = Htmlact.effect `Element in
  Example.button ~at:[request; target; effect] "Delete"

let deletable_bookmark ~urlf b =
  let status = if b.Bookmark.broken then "broken" else "alive" in
  El.tr ~at:[At.class' status]
    [ El.td [El.txt (String.capitalize_ascii status)];
      El.td [El.a ~at:At.[href b.Bookmark.link] [El.txt b.Bookmark.name]];
      El.td [delete_bookmark ~urlf b]; ]

let table_headers =
  let cell txt = El.th [El.txt txt] in
  El.tr [cell "Status"; cell "Bookmark"; El.th []]

let table_view ~urlf bs =
  let bs = List.map (deletable_bookmark ~urlf) bs in
  El.table [ El.thead [table_headers]; El.tbody bs]

let actions ~urlf =
  let url = Example.uf urlf "?action=restore" in
  let request = Htmlact.request ~method':`POST url in
  let target = Htmlact.target ":up :up table" in
  let effect = Htmlact.effect `Element in
  let at = [request; target; effect] in
  let restore = Example.button ~at "Restore deleted" in
  El.div [restore]

let index ~urlf request =
  let* method' = Http.Request.allow Http.Method.[get; post] request in
  match method' with
  | `GET ->
      let bookmarks = table_view ~urlf (Bookmark.all ()) in
      let content = El.splice [bookmarks; actions ~urlf] in
      let body = [description; content] in
      let html_page = Example.html_page ~style ~id ~title:name body in
      Ok (Http.Response.html Http.Status.ok_200 html_page)
  | `POST ->
      let* query = Http.Request.to_query request in
      match Http.Query.find_first "action" query with
      | Some "restore" ->
          Bookmark.restore_deleted ();
          let html = Example.html [table_view ~urlf (Bookmark.all ())] in
          Ok (Http.Response.html Http.Status.ok_200 html)
      | v ->
          let some = Printf.sprintf "unknown action %s" in
          let log = Option.fold ~none:"missing action" ~some v in
          Http.Response.bad_request_400 ~log ()

let bookmark_delete id request = match int_of_string_opt id with
| None -> Http.Response.bad_request_400 ~log:"illegal id" ()
| Some id ->
    let* `DELETE = Http.Request.allow Http.Method.[delete] request in
    match Bookmark.find ~id with
    | None -> Ok (Http.Response.html Http.Status.ok_200 (Example.html []))
    | Some id ->
        Bookmark.delete id;
        Ok (Http.Response.html Http.Status.ok_200 (Example.html []))

let serve request =
  let urlf = Example.urlf request ~prefix in
  match Example.path ~prefix request with
  | [""] -> index ~urlf request
  | "bookmark" :: [id] -> bookmark_delete id request
  | p -> Http.Response.not_found_404 ()
