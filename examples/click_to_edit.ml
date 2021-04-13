(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_html

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e
let strf = Printf.sprintf

let id = __MODULE__
let name = "Click to edit"
let synopsis = "Click to edit a record in the page."
let prefix = "click-to-edit"
let description = Example.description [ Ht.txt
  "When the edit button is clicked the bookmark record is replaced
   by a form returned by the server to edit it." ]

let style = {css|
.record label::after { content: ":" }
.record div + div { margin-top: var(--size-fourth-line); }
.record div:last-child { margin-top: var(--size-half-line); }
.hc-out, .record input { transition: all var(--dur-short); }
.hc-in input.field { background: white; }
.hc-out input.field { background: white; }
|css}

let field_label n = Ht.splice [Ht.label [Ht.txt n]]
let field v = Example.field [Ht.txt v]
let field_link l = Example.field [Ht.a ~at:At.[href l] [Ht.txt l]]
let input_field = Example.input_field

let view urlf b =
  let edit_button urlf b =
    let r = Hc.request (Example.uf urlf "bookmark/%d/editor" b.Bookmark.id) in
    let t = Hc.target ".record:up" and e = Hc.effect `Inplace in
    Example.button ~at:[r; t; e] "Edit"
  in
  let at = [At.class' "record"] in
  Ht.div ~at [
    Ht.div [ field_label "Name"; Ht.sp; field b.Bookmark.name ];
    Ht.div [ field_label "Link"; Ht.sp; field_link b.Bookmark.link ];
    Ht.div [ field_label "Description"; Ht.sp; field b.Bookmark.description ];
    Ht.div [ edit_button urlf b ]]

let editor_view urlf b =
  let cancel_button urlf b =
    let r = Hc.request (Example.uf urlf "bookmark/%d" b.Bookmark.id) in
    let t = Hc.target ".record:up" and e = Hc.effect `Inplace in
    Example.button ~at:[r; t; e] "Cancel"
  in
  let r = Hc.request ~meth:`PUT (Example.uf urlf "bookmark/%d" b.Bookmark.id) in
  let e = Hc.effect `Inplace in
  Ht.form ~at:[At.class' "record"; r; e] [
    Ht.div [ field_label "Name"; Ht.sp;
             input_field ~name:"name" ~type':"text" b.Bookmark.name ];
    Ht.div [ field_label "Link"; Ht.sp;
             input_field ~name:"link" ~type':"url" b.Bookmark.link ];
    Ht.div [ field_label "Description"; Ht.sp;
             input_field ~name:"description" ~type':"text"
               b.Bookmark.description ];
    Ht.div [ cancel_button urlf b; Example.submit "Save" ]]

let put_bookmark r b =
  let* q = Req.to_query r in
  let get n d = Option.value ~default:d (Http.Query.find n q) in
  let name = get "name" b.Bookmark.name in
  let link = get "link" b.Bookmark.link in
  let description = get "description" b.Bookmark.description in
  let b' = { b with Bookmark.name; link; description } in
  Bookmark.set b';
  Ok (view (Example.urlf r) (Bookmark.get b'.Bookmark.id |> Option.get))

let index urlf =
  let b = Bookmark.get 1 |> Option.get in
  let content = view urlf  b in
  Example.page ~style ~id ~title:name [description; content]

let bookmark_part id act r = match int_of_string id with
| exception Failure _ ->
    Error (Resp.v ~explain:"illegal id" Http.s400_bad_request)
| id ->
    match Bookmark.get id with
    | None -> Result.ok @@ Resp.v Http.s404_not_found
    | Some b ->
        match act with
        | [""] | [] ->
            let* m = Req.Allow.(meths [get; put] r) in
            let* view = match m with
            | `GET -> Ok (view (Example.urlf r) b)
            | `PUT -> put_bookmark r b
            | _ -> assert false
            in
            Result.ok @@ Resp.html Http.s200_ok (Example.part [view])
        | ["editor"] ->
            let editor = editor_view (Example.urlf r) b in
            Result.ok @@ Resp.html Http.s200_ok (Example.part [editor])
        | _ ->
            Result.ok @@ Resp.v Http.s404_not_found

let serve r = match Req.path r with
| [""] -> Ok (Resp.html Http.s200_ok (index (Example.urlf r)))
| ("bookmark" :: id :: act) -> bookmark_part id act r
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
