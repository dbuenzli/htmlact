(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_html

module type T = sig
  val id : string
  val name : string
  val synopsis : string
  val prefix : string
  val serve : Req.t -> (Resp.t, Resp.t) result
end

let src_root = "https://erratique.ch/repos/hc/tree/examples/"

let style = Style.base
let link ~href:r text = Ht.a ~at:At.[href r] [Ht.txt text]
let page ?style:st ~id ~title content =
  let scripts = ["/hc-page.js"] in
  let h1 =
    Ht.txt title ::
    (if title = "Hc examples" then [] else
     let src = src_root ^ (String.uncapitalize_ascii id ^ ".ml") in
     [Ht.txt " ";
      Ht.small [link ~href:src "src"; Ht.txt " ";
                link ~href:"/" "up"]])
  in
  let body = Ht.body [ Ht.h1 h1; Ht.splice content] in
  let more_head =
    let el_style s = Ht.style [Ht.raw s] in
    let more_style = match st with None -> Ht.void | Some t -> el_style t in
    Ht.splice [ el_style style; more_style ]
  in
  let title = Printf.sprintf "%s – Hc examples" title in
  Ht.to_string ~doc_type:true @@
  Ht.page ~more_head ~scripts ~title body

let part content = Ht.to_string ~doc_type:false (Ht.splice content)

let table ?(headers = []) rows =
  let th txt = Ht.th [Ht.txt txt] in
  let td d = Ht.td [d] in
  let tr r = Ht.tr (List.map td r) in
  let headers = List.map th headers in
  let rows = List.map tr rows in
  let head = match headers with [] -> Ht.void | hs -> Ht.thead [Ht.tr hs] in
  Ht.table [ head; Ht.tbody rows]

let description contents = Ht.div ~at:At.[class' "description"] contents

let c_field = "field"
let c_button = "button"

let _button typ ?(at = []) label =
  Ht.button ~at:At.(class' c_button :: type' typ :: at)
    [ Ht.span ~at:At.[class' "spinner"; v "aria-hidden" "true"] [];
      Ht.span [Ht.txt label]; ]

let submit ?at label = _button "submit" ?at label
let button ?at label = _button "button" ?at label

let input_field ?(autocomplete = false) ?(at = []) ~type':t ~name:n fv =
  let size = max 20 (String.length fv + 4) in
  let at = (At.v "size" (string_of_int size)) :: at in
  let at = At.add_if (not autocomplete) (At.autocomplete "off") at in
  Ht.input ~at:At.(class' c_field :: type' t :: name n :: value fv :: at) ()

let field ?(at = []) fv =
  Ht.span ~at:At.(class' c_field :: at) fv

type urlf = string
let urlf r =
  let root = Http.Path.encode (Req.service_root r) in
  String.concat "/" [root; ""]

let uf urlf fmt = Printf.sprintf ("%s" ^^ fmt) urlf


let req_decode req dec = match dec req with
| exception Failure explain -> Error (Resp.v ~explain Http.s400_bad_request)
| v -> Ok v

let req_decode_query req dec = match Req.to_query req with
| Error _ as e -> e
| Ok q ->
    match dec req q with
    | exception Failure explain -> Error (Resp.v ~explain Http.s400_bad_request)
    | v -> Ok v

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
