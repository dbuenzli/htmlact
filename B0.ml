open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let brr = B0_ocaml.libname "brr"
let webs = B0_ocaml.libname "webs"
let webs_cli = B0_ocaml.libname "webs.cli"
let webs_unix = B0_ocaml.libname "webs.unix"
let htmlit = B0_ocaml.libname "htmlit"

let htmlact = B0_ocaml.libname "htmlact"
let htmlact_page = B0_ocaml.libname "htmlact.page"

(* Libraries *)

let htmlact_lib =
  let srcs = [ `Dir ~/"src" ] in
  let requires = [webs; htmlit] in
  B0_ocaml.lib htmlact ~doc:"htmlact library" ~srcs ~requires

let htmlact_page_lib =
  let srcs = [ `Dir ~/"src/page"] in
  let requires = [brr] in
  B0_ocaml.lib htmlact_page ~doc:"Webpage driver library" ~srcs ~requires

let htmlact_page_js =
  let doc = "Self-contained webpage driver" in
  let srcs = [`Dir ~/"src/standalone"] in
  let requires = [htmlact_page] in
  B0_jsoo.exe "htmlact-page.js" ~requires ~doc ~srcs

(* Examples *)

let htmlact_examples =
  let doc = "Htmlact examples" in
  let requires = [unix; htmlact; webs; webs_cli; webs_unix; htmlit] in
  let srcs = [ `Dir ~/"examples" ] in
  let exec_env env u =
    let driver_dir = Fpath.to_string (B0_env.unit_dir env htmlact_page_js) in
    let env = B0_env.build_env env in
    Ok (Os.Env.add "HTMLACT_FILE_ROOT" driver_dir env)
  in
  let meta =
    B0_meta.empty
    |> ~~ B0_ocaml.Code.needs `Native
    |> ~~ B0_unit.Exec.cwd `Scope_dir
    |> ~~ B0_unit.Exec.env (`Fun ("Adds .js location in env", exec_env))
  in
  let wrap proc b = B0_build.require_unit b htmlact_page_js; proc b in
  B0_ocaml.exe "htmlact-examples" ~wrap ~doc ~srcs ~requires ~meta

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The htmlact programmers"]
    |> ~~ B0_meta.maintainers
      ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/htmlact"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/htmlact/doc"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/htmlact.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/htmlact/issues"
    |> ~~ B0_meta.description_tags
      ["front-end"; "ui"; "html"; "web"; "org:erratique"; ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
           "--with-webs" "%{webs:installed}%"
           "--with-htmlit" "%{htmlit:installed}%"]]|}
    |> ~~ B0_opam.depends [
      "ocaml", {|>= "4.14.0"|};
      "ocamlfind", {|build|};
      "ocamlbuild", {|build|};
      "topkg", {|build & >= "1.0.3"|};
      "brr", "";
      "js_of_ocaml-compiler", {|>= "3.7.1"|};
    ]
    |> ~~ B0_opam.depopts [ "webs", ""; "htmlit", ""]
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"htmlact package" ~meta ~locked:true @@
  B0_unit.list ()
