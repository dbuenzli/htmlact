open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let brr = B0_ocaml.libname "brr"
let webs = B0_ocaml.libname "webs"
let webs_cli = B0_ocaml.libname "webs.cli"
let webs_unix = B0_ocaml.libname "webs.unix"
let webs_httpc = B0_ocaml.libname "webs.httpc"
let webs_html = B0_ocaml.libname "webs.html"

let hc = B0_ocaml.libname "hc"
let hc_page = B0_ocaml.libname "hc.page"

(* Libraries *)

let hc_lib =
  let srcs = Fpath.[ `File (v "src/hc.mli"); `File (v "src/hc.ml") ] in
  let requires = [webs; webs_html] in
  B0_ocaml.lib hc ~doc:"Hc library" ~srcs ~requires

let hc_page_lib =
  let srcs = Fpath.[ `File (v "src/hc_page.mli"); `File (v "src/hc_page.ml")] in
  let requires = [brr] in
  B0_ocaml.lib hc_page ~doc:"Web page driver library" ~srcs ~requires

let hc_page_js =
  let srcs = Fpath.[ `File (v "src/hc_page_init.ml"); ] in
  let requires = [hc_page] in
  let meta = B0_jsoo.meta ~requires () in
  B0_jsoo.exe "hc-page.js" ~meta ~srcs

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

let hc_examples =
  let doc = "Hc examples" in
  let requires = [ hc; webs; webs_cli; webs_unix; webs_httpc; webs_html] in
  let srcs = Fpath.[ `Dir (v "examples"); ] in
  let meta = B0_meta.(empty |> add B0_ocaml.Meta.supported_code `Native) in
  let wrap proc b = B0_build.require b hc_page_js; proc b in
  let action = serve_reload in
  B0_ocaml.exe "hc-examples" ~wrap ~doc ~srcs ~requires ~action ~meta

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The hc programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/hc"
    |> add online_doc "https://erratique.ch/software/hc/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/hc.git"
    |> add issues "https://github.com/dbuenzli/hc/issues"
    |> add description_tags ["front-end"; "ui"; "html"; "org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
           "--with-webs" "%{webs:installed}%"]]|}
    |> add B0_opam.Meta.depends [
      "ocaml", {|>= "4.08.0"|};
      "ocamlfind", {|build|};
      "ocamlbuild", {|build|};
      "topkg", {|build & >= "1.0.3"|};
      "brr", "";
      "webs", "";
      "js_of_ocaml-compiler", {|>= "3.7.1"|};
    ]
    |> tag B0_opam.tag
  in
  B0_pack.v "default" ~doc:"hc package" ~meta ~locked:true @@
  B0_unit.list ()
