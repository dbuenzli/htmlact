(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_html

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
.spinner.hc-request
{ animation: spin var(--dur-medium) ease var(--dur-short) infinite alternate; }
|css}

let autocomplete_bookmarks ~prefix =
  let prefix = String.lowercase_ascii prefix in
  let has_prefix b =
    Example.starts_with ~prefix (String.lowercase_ascii b.Bookmark.name)
  in
  List.filter has_prefix (Bookmark.all ())

let autocomplete_options r =
  let* q = Req.to_query r in
  let part =
    let t = Option.value ~default:"" (Http.Query.find "title" q) in
    let t = String.trim t in
    if t = "" then [] else
    let option b = El.option ~at:At.[value b.Bookmark.name] [] in
    List.map option (autocomplete_bookmarks ~prefix:t)
  in
  Ok (Resp.html Http.ok_200 (Example.part part))

let bookmark_name_input urlf =
  let list_id = "titles" in
  let input =
    let r = Hc.request ~meth:`POST (Example.uf urlf "") in
    let t = Hc.target (":up #" ^ list_id) and f = Hc.feedback ":up .spinner" in
    let e = Hc.event ~debounce_ms:500 "input" in
    let at =
      At.[ class' "field"; name "title"; type' "text"; int "size" 25;
           v "list" list_id; autocomplete "off"; autofocus ]
    in
    El.input ~at:(r :: t :: e :: f :: at) ()
  in
  [ El.label [El.txt "Bookmark name"];
    El.span ~at:At.[class' "spinner"] [El.txt "…"];
    El.br ();
    input; El.datalist ~at:At.[id list_id] [] ]

let bookmark_name r =
  let* m = Req.Allow.(meths [get; post] r) in
  match m with
  | `POST -> autocomplete_options r
  | `GET ->
      let content = El.splice (bookmark_name_input (Example.urlf r)) in
      let page = Example.page ~style ~id ~title:name [description; content] in
      Ok (Resp.html Http.ok_200 page)

let serve r = match Req.path r with
| [""] -> bookmark_name r
| p -> Resp.not_found_404 ()

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
