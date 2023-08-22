(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

module type T = sig
  val id : string
  val name : string
  val synopsis : string
  val prefix : string
  val serve : Http.Request.t -> (Http.Response.t, Http.Response.t) result
end

(* Request deconstruction *)

let path request ~prefix =
  Http.Path.strip_prefix ~prefix:[prefix] (Http.Request.path request)

(* URL formatting *)

type urlf = string
let urlf request ~prefix =
  let service_path = Http.Request.service_path request in
  Http.Path.encode (Http.Path.concat service_path [prefix])

let uf urlf fmt = Printf.sprintf ("%s" ^^ fmt) urlf

(* HTML generation *)

let class_description = "description"
let class_field = "field"
let class_button = "button"
let class_spinner = "spinner"

let _button typ ?(at = []) label =
  El.button ~at:At.(class' class_button :: type' typ :: at)
    [ El.span ~at:At.[class' class_spinner; v "aria-hidden" "true"] [];
      El.span [El.txt label]; ]

let button ?at label = _button "button" ?at label
let description contents = El.div ~at:At.[class' class_description] contents
let field ?(at = []) fv = El.span ~at:At.(class' class_field :: at) fv
let submit ?at label = _button "submit" ?at label
let input_field ?(autocomplete = false) ?(at = []) ~type':t ~name:n fv =
  let size = max 20 (String.length fv + 4) in
  let at = (At.v "size" (string_of_int size)) :: at in
  let at = At.if' (not autocomplete) (At.autocomplete "off") :: at in
  El.input ~at:At.(class' class_field :: type' t :: name n :: value fv :: at) ()

let link ~href:r text = El.a ~at:[At.href r] [El.txt text]
let table ?(headers = []) rows =
  let th txt = El.th [El.txt txt] in
  let td d = El.td [d] in
  let tr r = El.tr (List.map td r) in
  let headers = List.map th headers in
  let rows = List.map tr rows in
  let head = match headers with [] -> El.void | hs -> El.thead [El.tr hs] in
  El.table [ head; El.tbody rows]

let html content = El.to_string ~doctype:false (El.splice content)
let html_page ?style ~id ~title:t content =
  let inline_style s = El.style [El.unsafe_raw s] in
  let example_title s = Printf.sprintf "%s – Htmlact examples" t in
  let example_nav_links id =
    let src_root = "https://erratique.ch/repos/htmlact/tree/examples/" in
    let basename = String.uncapitalize_ascii id in
    let src = String.concat "" [src_root; basename; ".ml"] in
    let src_link = link ~href:src "src" in
    El.splice [El.sp; El.small [src_link; El.sp; link ~href:".." "up"]]
  in
  let root = String.equal t "Htmlact examples" in
  let title = if root then t else example_title t in
  let scripts = ["/htmlact-page.js"] in
  let more_style = Option.fold ~none:El.void ~some:inline_style style in
  let more_head = El.splice [ inline_style Style.base; more_style ] in
  let nav_links = if root then El.void else example_nav_links id in
  let h1 = El.h1 [El.txt t; nav_links] in
  let body = El.body [ h1; El.splice content] in
  let page = El.page ~title ~scripts ~more_head body in
  El.to_string ~doctype:true page
