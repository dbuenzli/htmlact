(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

let ( let* ) = Result.bind

let id = __MODULE__
let name = "Click to edit"
let synopsis = "Click to edit a record in the page."
let prefix = "click-to-edit"
let description = Example.description [
    El.txt "When the edit button is clicked the bookmark record is replaced \
            by a form returned by the server to edit it." ]

let style = {css|
.record label::after { content: ":" }
.record div + div { margin-top: var(--size-fourth-line); }
.record div:last-child { margin-top: var(--size-half-line); }

form.htmlact-out, .record input { transition: all var(--dur-short); }
.htmlact-in input.field, .htmlact-out input.field { background: white; }
|css}

let field_label n = El.label [El.txt n]
let field v = Example.field [El.txt v]
let field_link l = Example.field [El.a ~at:[At.href l] [El.txt l]]
let input_field = Example.input_field
let edit_button urlf b =
  let url = Example.uf urlf "/bookmark/%d/editor" b.Bookmark.id in
  let request = Htmlact.request url in
  let target = Htmlact.target ".record:up" in
  let effect = Htmlact.effect `Element in
  Example.button ~at:[request; target; effect] "Edit"

let cancel_button urlf b =
  let url = Example.uf urlf "/bookmark/%d" b.Bookmark.id in
  let request = Htmlact.request url in
  let target = Htmlact.target ".record:up" in
  let effect = Htmlact.effect `Element in
  Example.button ~at:[request; target; effect] "Cancel"

let view ~urlf b =
  El.div ~at:[At.class' "record"] [
    El.div [ field_label "Name"; El.sp; field b.Bookmark.name ];
    El.div [ field_label "Link"; El.sp; field_link b.Bookmark.link ];
    El.div [ field_label "Description"; El.sp; field b.Bookmark.description ];
    El.div [ edit_button urlf b ]]

let editor_view ~urlf b =
  let url = Example.uf urlf "/bookmark/%d" b.Bookmark.id in
  let request = Htmlact.request ~method':`PUT url in
  let effect = Htmlact.effect `Element in
  El.form ~at:[At.class' "record"; request; effect] [
    El.div [ field_label "Name"; El.sp;
             input_field ~name:"name" ~type':"text" b.Bookmark.name ];
    El.div [ field_label "Link"; El.sp;
             input_field ~name:"link" ~type':"url" b.Bookmark.link ];
    El.div [ field_label "Description"; El.sp;
             input_field ~name:"description" ~type':"text"
               b.Bookmark.description ];
    El.div [ cancel_button urlf b; Example.submit "Save" ]]

let put_bookmark ~urlf request b =
  let* query = Http.Request.to_query request in
  let get n d = Option.value ~default:d (Http.Query.find_first n query) in
  let name = get "name" b.Bookmark.name in
  let link = get "link" b.Bookmark.link in
  let description = get "description" b.Bookmark.description in
  Bookmark.set { b with Bookmark.name; link; description };
  Ok (view ~urlf (Bookmark.get ~id:b.Bookmark.id))

let bookmark_part ~urlf request id action = match int_of_string_opt id with
| None -> Http.Response.bad_request_400 ~explain:"illegal id" ()
| Some id ->
    match Bookmark.find ~id with
    | None -> Http.Response.not_found_404 ()
    | Some b ->
        match action with
        | [""] | [] ->
            let* m = Http.Request.allow Http.Method.[get; put] request in
            let* view = match m with
            | `GET -> Ok (view ~urlf b)
            | `PUT -> put_bookmark ~urlf request b
            in
            Ok (Http.Response.html Http.Status.ok_200 (Example.html [view]))
        | ["editor"] ->
            let editor = editor_view ~urlf b in
            Ok (Http.Response.html Http.Status.ok_200 (Example.html [editor]))
        | _ ->
            Http.Response.not_found_404 ()

let index urlf =
  let content = view ~urlf (Bookmark.get ~id:1) in
  Example.html_page ~style ~id ~title:name [description; content]

let serve request =
  let urlf = Example.urlf request ~prefix in
  match Example.path ~prefix request with
  | [""] -> Ok (Http.Response.html Http.Status.ok_200 (index urlf))
  | "bookmark" :: id :: action -> bookmark_part ~urlf request id action
  | p -> Http.Response.not_found_404 ()
