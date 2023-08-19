(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

(* Serves the examples *)

let ( let* ) = Result.bind

let examples =
  [ (module Click_to_edit : Example.T);
    (module Update_rows : Example.T);
    (module Click_to_load : Example.T);
    (module Delete_rows : Example.T);
    (module Autocomplete : Example.T); ]

let index_page =
  let intro =
    let link = Example.link in
    let htmlact_link =
      link ~href:"https://erratique.ch/software/htmlact" "Htmlact"
    in
    let webs_link = link ~href:"https://erratique.ch/software/webs" "Webs" in
    let htmx_link = link ~href:"https://htmx.org/examples/" "these ones" in
    El.p
      [El.txt "This is a list of page interaction patterns implemented
               using "; htmlact_link; El.txt " and "; webs_link;
       El.small [El.txt " (not required, you can use your own)."]; El.txt
         " Most of these examples are a remix of "; htmx_link; El.txt "."]
  in
  let examples =
    let li (module E : Example.T) =
      let href = At.href (Http.Path.encode [E.prefix; ""]) in
      let name = El.span ~at:At.[class' "link"] [El.txt E.name] in
      let text = [name; El.small [El.sp; El.txt E.synopsis]] in
      El.li [ El.a ~at:[href] text ]
    in
    El.ol ~at:At.[class' "examples"] (List.map li examples)
  in
  let content = [intro; examples] in
  Example.page ~id:"" ~title:"Htmlact examples" content

module Smap = Map.Make (String)

let example_map =
  let add acc ((module E : Example.T) as e) = Smap.add E.prefix e acc in
  List.fold_left add Smap.empty examples

let serve_examples r =
  let prefix = List.hd (Http.Request.path r) in
  match Smap.find_opt prefix example_map with
  | None -> Http.Response.not_found_404 ~explain:"No such example" ()
  | Some (module E : Example.T) ->
      let* r = Http.Request.forward_service ~strip:[prefix] r in
      E.serve r

let service file_root r =
  Http.Response.result @@ match Http.Request.path r with
  | ["htmlact-page.js" | "htmlact-page.map"] ->
      let* `GET = Http.Request.allow Http.Method.[get] r in
      let* file = Http.Request.to_absolute_filepath ~file_root r in
      Webs_fs.send_file r file
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] r in
      Ok (Http.Response.html Http.Status.ok_200 index_page)
  | _ ->
      serve_examples r

let find_root () = (* very hackish *)
  let bin_dir = Filename.(dirname Sys.executable_name) in
  match Filename.basename bin_dir with
  | "bin" -> Unix.realpath (Filename.concat bin_dir "../share/htmlact")
  | _ -> Unix.realpath (Filename.concat bin_dir "../htmlact-page-js")

let main () =
  let root = find_root () in
  Webs_quick.serve ~name:"htmlact" (service root)

let () = if !Sys.interactive then () else exit (main ())

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers

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
