(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

let ( let* ) = Result.bind

let id = __MODULE__
let name = "Autocomplete"
let synopsis = "Adaptative autocomplete list."
let prefix = "autocomplete"
let description = Example.description [ El.p [
    El.txt "On input the text field value is sent to server to ask for a list \
            of autocompletions. The input event is debounced by 500ms to \
            avoid sending too many requests."];
    El.p [
      El.small [
        El.txt "FIXME the Safari experience it completely unreliable."]]]

let style = {css|
@keyframes spin { from { visibility: visible; opacity:0 } to { opacity:1 }}
.spinner { visibility: hidden; }
.spinner.htmlact-request
{ animation: spin var(--dur-medium) ease var(--dur-short) infinite alternate; }
|css}

let var_name = "name"

let bookmark_complete request =
  let el_option b = El.option ~at:At.[value b.Bookmark.name] [] in
  let* `GET = Http.Request.allow Http.Method.[get] request in
  let* query = Http.Request.to_query request in
  let name = Option.value ~default:"" (Http.Query.find_first var_name query) in
  let name = String.trim name in
  let bookmarks = if name = "" then [] else Bookmark.search_name ~prefix:name in
  let html = Example.html (List.map el_option bookmarks) in
  Ok (Http.Response.html Http.Status.ok_200 html)

let bookmark_name_input urlf =
  let list_id = "names" in
  let request = Htmlact.request ~method':`GET (Example.uf urlf "/complete") in
  let target = Htmlact.target (":up #" ^ list_id) in
  let feedback = Htmlact.feedback ":up .spinner" in
  let effect = Htmlact.event ~debounce_ms:500 "input" in
  let at =
    At.[ class' "field"; name var_name; type' "search"; int "size" 25;
         list list_id; autocomplete "off"; autofocus;
         At.true' "incremental" ]
  in
  let input = El.input ~at:(request :: target :: effect :: feedback :: at) () in
  let label = El.label [El.txt "Bookmark name"] in
  El.splice
    [ label; El.span ~at:At.[class' "spinner"] [El.txt "…"]; El.br ();
      input; El.datalist ~at:At.[id list_id] []]

let bookmark_name urlf request =
  let* `GET = Http.Request.allow Http.Method.[get] request in
  let content = bookmark_name_input urlf in
  let body = [description; content] in
  let html_page = Example.html_page ~style ~id ~title:name body in
  Ok (Http.Response.html Http.Status.ok_200 html_page)

let serve request =
  let urlf = Example.urlf request ~prefix in
  match Example.path ~prefix request with
  | [""] -> bookmark_name urlf request
  | ["complete"] -> bookmark_complete request
  | _ -> Http.Response.not_found_404 ()
