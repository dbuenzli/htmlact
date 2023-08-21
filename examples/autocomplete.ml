(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

let ( let* ) = Result.bind
let strf = Printf.sprintf

let id = __MODULE__
let name = "Autocomplete"
let synopsis = "Adaptative autocomplete list."
let prefix = "autocomplete"
let description = Example.description [ El.div [El.txt
  "On input the text field value is sent to server to ask for a list
   of autocompletions. The input event is debounced by 500ms to avoid sending
   too many requests to the server."]; El.div [
   El.small [El.txt "FIXME the Safari experience it completely unreliable."]]]

let style = {css|
@keyframes spin { from { visibility: visible; opacity:0 } to { opacity:1 }}
.spinner { visibility: hidden; }
.spinner.htmlact-request
{ animation: spin var(--dur-medium) ease var(--dur-short) infinite alternate; }
|css}

let autocomplete_bookmarks ~prefix =
  let prefix = String.lowercase_ascii prefix in
  let has_prefix b =
    Example.starts_with ~prefix (String.lowercase_ascii b.Bookmark.name)
  in
  List.filter has_prefix (Bookmark.all ())

let autocomplete_options r =
  let* q = Http.Request.to_query r in
  let part =
    let t = Option.value ~default:"" (Http.Query.find_first "title" q) in
    let t = String.trim t in
    if t = "" then [] else
    let option b = El.option ~at:At.[value b.Bookmark.name] [] in
    List.map option (autocomplete_bookmarks ~prefix:t)
  in
  Ok (Http.Response.html Http.Status.ok_200 (Example.part part))

let bookmark_name_input urlf =
  let list_id = "titles" in
  let input =
    let r = Htmlact.request ~meth:`POST (Example.uf urlf "") in
    let t = Htmlact.target (":up #" ^ list_id) in
    let f = Htmlact.feedback ":up .spinner" in
    let e = Htmlact.event ~debounce_ms:500 "input" in
    let at =
      At.[ class' "field"; name "title"; type' "search"; int "size" 25;
           v "list" list_id; autocomplete "off"; autofocus;
           At.true' "incremental" ]
    in
    El.input ~at:(r :: t :: e :: f :: at) ()
  in
  let label = El.label [El.txt "Bookmark name"] in
  [ label;
    El.span ~at:At.[class' "spinner"] [El.txt "…"];
    El.br ();
    (* The form here prevents Safari of trying to complete non-sense.
       It seems it pick-up the name from Bookmark name and tries to complete
       from the address book !? *)
    El.form [input; El.datalist ~at:At.[id list_id] []]]

let bookmark_name r =
  let* m = Http.Request.allow Http.Method.[get; post] r in
  match m with
  | `POST -> autocomplete_options r
  | `GET ->
      let content = El.splice (bookmark_name_input (Example.urlf r)) in
      let page = Example.page ~style ~id ~title:name [description; content] in
      Ok (Http.Response.html Http.Status.ok_200 page)

let serve r = match Http.Request.path r with
| [""] -> bookmark_name r
| p -> Http.Response.not_found_404 ()
