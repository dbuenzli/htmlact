(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
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
    (module Active_search : Example.T); ]

let index_page =
  let li_example (module E : Example.T) =
    let href = At.href (Http.Path.encode [E.prefix]) in
    let name = El.span ~at:At.[class' "link"] [El.txt E.name] in
    let text = [name; El.small [El.sp; El.txt E.synopsis]] in
    El.li [ El.a ~at:[href] text ]
  in
  let link = Example.link in
  let htmlact = link ~href:"https://erratique.ch/software/htmlact" "Htmlact" in
  let htmlit = link ~href:"https://erratique.ch/software/htmlit" "Htmlit" in
  let webs = link ~href:"https://erratique.ch/software/webs" "Webs" in
  let htmx = link ~href:"https://htmx.org/examples/" "these ones" in
  let intro = El.splice [
      El.p [
        El.txt "This is a list of page interaction patterns \
                implemented using "; htmlact; El.txt ". ";
        htmlit; El.txt " and "; webs;
        El.txt " are also used but not required."];
      El.p [
        El.txt "Most of these examples are a remix of "; htmx; El.txt "."]]
  in
  let examples = List.map li_example examples in
  let examples = El.ol ~at:At.[class' "examples"] examples  in
  let content = [intro; examples] in
  Example.html_page ~id:"" ~title:"Htmlact examples" content

module String_map = Map.Make (String)

let example_map =
  let add acc ((module E : Example.T) as e) = String_map.add E.prefix e acc in
  List.fold_left add String_map.empty examples

let serve_example ~example request =
  match String_map.find_opt example example_map with
  | None -> Http.Response.not_found_404 ~explain:"No such example" ()
  | Some (module E : Example.T) -> E.serve request

let service ~file_root request =
  Http.Response.result @@
  let* () = Http.Request.clean_path request in
  match Http.Request.path request with
  | ["htmlact-page.js" | "htmlact-page.map"] ->
      let* file = Http.Request.to_absolute_filepath ~file_root request in
      Webs_fs.send_file request file
  | [""] ->
      let* `GET = Http.Request.allow Http.Method.[get] request in
      Ok (Http.Response.html Http.Status.ok_200 index_page)
  | example :: _ ->
      serve_example ~example request
  | [] ->
      Http.Response.not_found_404 ()

let find_file_root () = (* extremely hackish *)
  let bin_dir = Filename.(dirname Sys.executable_name) in
  match Filename.basename bin_dir with
  | "bin" -> Unix.realpath (Filename.concat bin_dir "../share/htmlact")
  | _ -> (* dev *) Unix.realpath (Filename.concat bin_dir "../htmlact-page-js")

let main () = Webs_quick.serve (service ~file_root:(find_file_root ()))
let () = if !Sys.interactive then () else exit (main ())
