#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let webs = Conf.with_pkg "webs"
let () =
  Pkg.describe "hc" @@ fun c ->
  let webs = Conf.value c webs in
  Ok [ Pkg.mllib "src/hc.mllib";
       Pkg.mllib "src/hc_page.mllib" ~dst_dir:"page";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld";
       Pkg.bin ~cond:webs "examples/hc_examples" ~dst:"hc-examples";
       Pkg.share "src/hc_page_init.js" ~dst:"hc-page.js"
     ]
