open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let brr = B0_ocaml.libname "brr"
let webs = B0_ocaml.libname "webs"
let webs_cli = B0_ocaml.libname "webs.cli"
let webs_unix = B0_ocaml.libname "webs.unix"
let htmlit = B0_ocaml.libname "htmlit"

let htmlact = B0_ocaml.libname "htmlact"
let htmlact_page = B0_ocaml.libname "htmlact.page"

(* Libraries *)

let htmlact_lib =
  let srcs = [ `Dir (Fpath.v "src") ] in
  let requires = [webs; htmlit] in
  B0_ocaml.lib htmlact ~doc:"htmlact library" ~srcs ~requires

let htmlact_page_lib =
  let srcs = [ `Dir (Fpath.v "src/page")] in
  let requires = [brr] in
  B0_ocaml.lib htmlact_page ~doc:"Webpage driver library" ~srcs ~requires

let htmlact_page_js =
  let srcs = [`Dir (Fpath.v "src/standalone")] in
  let requires = [htmlact_page] in
  let meta = B0_jsoo.meta ~requires () in
  let doc = "Self-contained webpage driver" in
  B0_jsoo.exe "htmlact-page.js" ~doc ~meta ~srcs

(* Examples *)

let serve_reload b u ~args = match B0_unit.get_meta B0_meta.exe_file u with
| Error _ as e -> Log.if_error e ~use:(Fut.return B0_cli.Exit.some_error)
| Ok exe ->
    let open Result.Syntax in
    Fut.bind exe @@ fun exe ->
    Log.if_error ~use:(Fut.return B0_cli.Exit.some_error) @@
    Result.map Fut.return @@
    let cwd = B0_build.scope_dir b u in
    let* server = Os.Cmd.spawn ~cwd Cmd.(path exe %% list args) in
    let* show_uri = Os.Cmd.get_tool (Fpath.v "show-uri") (* todo in build *)in
    let show_uri = Cmd.(path show_uri % "-p" % "http://localhost:8000") in
    let* () = Os.Cmd.run show_uri in
    let* st = Os.Cmd.spawn_wait_status server in
    let code = match st with `Exited c -> c | `Signaled c -> 128 + c in
    Ok (Os.Exit.code code)

let htmlact_examples =
  let doc = "Htmlact examples" in
  let requires = [ htmlact; webs; webs_cli; webs_unix; htmlit] in
  let srcs = Fpath.[ `Dir (v "examples"); ] in
  let meta = B0_meta.(empty |> add B0_ocaml.Meta.supported_code `Native) in
  let wrap proc b = B0_build.require b htmlact_page_js; proc b in
  let action = serve_reload in
  B0_ocaml.exe "htmlact-examples" ~wrap ~doc ~srcs ~requires ~action ~meta

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The htmlact programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/htmlact"
    |> add online_doc "https://erratique.ch/software/htmlact/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/htmlact.git"
    |> add issues "https://github.com/dbuenzli/htmlact/issues"
    |> add description_tags
      ["front-end"; "ui"; "html"; "web"; "org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
           "--with-webs" "%{webs:installed}%"
           "--with-htmlit" "%{htmlit:installed}%"]]|}
    |> add B0_opam.Meta.depends [
      "ocaml", {|>= "4.14.0"|};
      "ocamlfind", {|build|};
      "ocamlbuild", {|build|};
      "topkg", {|build & >= "1.0.3"|};
      "brr", "";
      "js_of_ocaml-compiler", {|>= "3.7.1"|};
    ]
    |> add B0_opam.Meta.depopts [ "webs", ""; "htmlit", ""]
    |> tag B0_opam.tag
  in
  B0_pack.v "default" ~doc:"htmlact package" ~meta ~locked:true @@
  B0_unit.list ()
