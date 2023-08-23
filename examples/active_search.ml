(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

let ( let* ) = Result.bind

let id = __MODULE__
let name = "Active search"
let synopsis = "Search records on user input."
let prefix = "active-search"
let description = Example.description [
    El.p [ El.txt
             "On input the text field value is sent to server to ask for \
              a list of bookmarks whose names are prefixed by the value."];
    El.p [ El.txt
             "The input event is debounced by 500ms to avoid sending too \
              many requests."]]

let style = {css|
@keyframes spin { from { visibility: visible; opacity:0 } to { opacity:1 }}
.spinner { visibility: hidden; }
.spinner.htmlact-request
{ animation: spin var(--dur-medium) ease var(--dur-short) infinite alternate; }
|css}

let var_name = "bookmark-name"
let results = "results"

let bookmark_view b =
  let link b = El.a ~at:At.[href b.Bookmark.link] [El.txt b.Bookmark.name] in
  let description b = El.txt b.Bookmark.description in
  El.tr [ El.td [link b]; El.td [description b]]

let results_view bs =
  El.table [ El.tbody ~at:At.[id results] (List.map bookmark_view bs)]

let search_bookmarks request =
  let* `GET = Http.Request.allow Http.Method.[get] request in
  let* query = Http.Request.to_query request in
  let name = Option.value ~default:"" (Http.Query.find_first var_name query) in
  let name = String.trim name in
  let bookmarks = match name with
  | "" -> [] | "*" -> Bookmark.all ()
  | name -> Bookmark.search_name ~prefix:name
  in
  let html = Example.html (List.map bookmark_view bookmarks) in
  Ok (Http.Response.html Http.Status.ok_200 html)

let bookmark_search_view urlf =
  let request = Htmlact.request ~method':`GET (Example.uf urlf "/search") in
  let target = Htmlact.target (":up #" ^ results) in
  let feedback = Htmlact.feedback ":up .spinner" in
  let event = Htmlact.event ~debounce_ms:500 "input" in
  let at = [
      request; target; event; feedback;
      At.class' "field"; At.name var_name; At.type' "search";
      At.int "size" 25; At.autofocus; At.autocomplete "off" ]
  in
  let input = El.input ~at () in
  let label = El.label ~at:[At.for' var_name] [El.txt "Bookmark name search"] in
  El.splice
    [ label; El.span ~at:At.[class' "spinner"] [El.txt "…"]; El.br ();
      input; results_view []]

let bookmark_search urlf request =
  let* `GET = Http.Request.allow Http.Method.[get] request in
  let content = bookmark_search_view urlf in
  let body = [description; content] in
  let html_page = Example.html_page ~style ~id ~title:name body in
  Ok (Http.Response.html Http.Status.ok_200 html_page)

let serve request =
  let urlf = Example.urlf request ~prefix in
  match Example.path ~prefix request with
  | [""] -> bookmark_search urlf request
  | ["search"] -> search_bookmarks request
  | _ -> Http.Response.not_found_404 ()
