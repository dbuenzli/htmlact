(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

let ( let* ) = Result.bind

let paginate_list ?(per_page = 2) page_num (* one-based *) l =
  if per_page < 0 || page_num < 0 then None else
  let all = Array.of_list l in
  let len = Array.length all in
  let max = len - 1 in
  let first = (page_num - 1) * per_page in
  let last = min (page_num * per_page - 1) max in
  if first > max then None else
  let page = Array.to_list (Array.sub all first (last - first + 1)) in
  Some (page, last = max)

let id = __MODULE__
let name = "Click to load"
let synopsis = "Click to display more table rows."
let prefix = "click-to-load"
let description = Example.description [ El.txt
  "The last row table has a button. When the button is clicked
   the row is replaced by a sequence of rows requested from the server.
   The last row of that sequence has a button. When the button etc." ]

let style = {css|
tr td:nth-child(1) { width:15%; }
tr td:nth-child(2) { width:85%; }
tr.broken td:nth-child(1) { color: rgb(var(--redish)); }

tr.htmlact-in
{ opacity: 0; transform: translateY(calc(-1 * var(--size-half-line))) }

tr { transition: all var(--dur-short); }
|css}

let bookmark_row b =
  let status = if b.Bookmark.broken then "broken" else "alive" in
  El.tr ~at:At.[class' status]
    [ El.td [El.txt (String.capitalize_ascii status)];
      El.td [El.a ~at:At.[href b.Bookmark.link] [El.txt b.Bookmark.name]]]

let load_next urlf n =
  let url = Example.uf urlf "?page=%d" n in
  let request = Htmlact.request ~method':`GET url in
  let target = Htmlact.target "tr:up" in
  let effect = Htmlact.effect `Element in
  let more = Example.button ~at:[request; target; effect] "More…" in
  El.tr [El.td ~at:At.[int "colspan" 3] [more]]

let bookmark_page_rows ?per_page urlf n =
  match paginate_list n (Bookmark.all ()) with
  | None -> []
  | Some (bs, last_page) ->
      let rows = List.rev_map bookmark_row bs in
      let rows = if last_page then rows else (load_next urlf (n + 1)) :: rows in
      List.rev rows

let table_headers =
  let cell txt = El.th [El.txt txt] in
  El.tr [cell "Status"; cell "Bookmark";]

let table_view ?updated rows =
  El.table [ El.thead [table_headers]; El.tbody rows]

let index ~urlf =
  let content = El.splice [table_view (bookmark_page_rows urlf 1)] in
  Example.html_page ~style ~id ~title:name [description; content]

let show_bookmark_table ~urlf request =
  let* `GET = Http.Request.allow Http.Method.[get] request in
  let* query = Http.Request.to_query request in
  let* page = match Http.Query.find_first "page" query with
  | None -> Ok None
  | Some page ->
      try Ok (Some (int_of_string page)) with
      | Failure e ->
          Error (Http.Response.empty ~explain:e Http.Status.bad_request_400)
  in
  match page with
  | None -> Ok (Http.Response.html Http.Status.ok_200 (index ~urlf))
  | Some page_num ->
      match bookmark_page_rows urlf page_num with
      | [] -> Http.Response.not_found_404 ()
      | ps -> Ok (Http.Response.html Http.Status.ok_200 (Example.html ps))

let serve request =
  let urlf = Example.urlf request ~prefix in
  match Example.path ~prefix request with
  | [""] -> show_bookmark_table ~urlf request
  | p -> Http.Response.not_found_404 ()
