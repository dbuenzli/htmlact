#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let webs = Conf.with_pkg "webs"
let htmlit = Conf.with_pkg "htmlit"
let () =
  Pkg.describe "htmlact" @@ fun c ->
  let webs = Conf.value c webs in
  let htmlit = Conf.value c htmlit in
  let depopts = webs && htmlit in
  Ok [ Pkg.mllib ~cond:depopts "src/htmlact.mllib";
       Pkg.mllib "src/page/htmlact_page.mllib" ~dst_dir:"page";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld";
       Pkg.bin ~cond:depopts "examples/htmlact_examples" ~dst:"htmlact-examples";
       Pkg.share "src/standalone/htmlact_page_init.js" ~dst:"htmlact-page.js"
     ]
