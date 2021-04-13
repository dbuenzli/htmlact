(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_html

(* Serve the examples *)

let ( let* ) = Result.bind

let examples =
  [ (module Click_to_edit : Example.T);
    (module Update_rows : Example.T);
    (module Click_to_load : Example.T);
    (module Delete_row : Example.T); ]

let index_page =
  let intro =
    let link = Example.link in
    let hc_link = link ~href:"https://erratique.ch/software/hc" "Hc" in
    let webs_link = link ~href:"https://erratique.ch/software/webs" "Webs" in
    let htmx_link = link ~href:"https://htmx.org/examples/" "these ones" in
    Ht.p
      [Ht.txt "This is a list of page interaction patterns implemented
               using "; hc_link; Ht.txt " and "; webs_link;
       Ht.small [Ht.txt " (not required, you can use your own)."]; Ht.txt
         " Most of these examples are a remix of "; htmx_link; Ht.txt "."]
  in
  let examples =
    let li (module E : Example.T) =
      let href = At.href (Http.Path.encode [E.prefix; ""]) in
      let name = Ht.span ~at:At.[class' "link"] [Ht.txt E.name] in
      let text = [name; Ht.small [Ht.sp; Ht.txt E.synopsis]] in
      Ht.li [ Ht.a ~at:[href] text ]
    in
    Ht.ol ~at:At.[class' "examples"] (List.map li examples)
  in
  let content = [intro; examples] in
  Example.page ~id:"" ~title:"Hc examples" content

module Smap = Map.Make (String)

let example_map =
  let add acc ((module E : Example.T) as e) = Smap.add E.prefix e acc in
  List.fold_left add Smap.empty examples

let serve_examples r =
  let prefix = List.hd (Req.path r) in
  match Smap.find_opt prefix example_map with
  | None -> Ok (Resp.v ~explain:"No such example" Http.s404_not_found)
  | Some (module E : Example.T) ->
      let* r = Req.forward_service ~strip:[prefix] r in
      E.serve r

let service file_root r =
  Resp.result @@ match Req.path r with
  | ["hc-page.js" | "hc-page.map"] ->
      let* _m = Req.Allow.(meths [get] r) in
      let* file = Req.to_absolute_filepath ~root:file_root r in
      Webs_unix.send_file r file
  | [""] ->
      let* _m = Req.Allow.(meths [get] r) in
      Ok (Resp.html Http.s200_ok index_page)
  | _ ->
      serve_examples r

let find_root () = (* very hackish *)
  let bin_dir = Filename.(dirname Sys.executable_name) in
  match Filename.basename bin_dir with
  | "bin" -> Webs_unix.realpath (Filename.concat bin_dir "../share/hc")
  | _ -> Webs_unix.realpath (Filename.concat bin_dir "../hc-page-js")

let main () =
  let root = find_root () in
  Webs_cli.quick_serve ~name:"hc" (service root)

let () = if !Sys.interactive then () else main ()

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
