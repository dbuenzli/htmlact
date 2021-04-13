(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr
open Brr_io

module Mutation_observer = struct
  let mutation_observer = Jv.get Jv.global "MutationObserver"
  let create obs = Jv.new' mutation_observer [| Jv.repr obs |]
  let disconnect o = ignore @@ Jv.call o "disconnect" [||]
  let observe o el opts = ignore @@ Jv.call o "observe" [| El.to_jv el; opts |]
end

module Intersection_observer = struct
  let intersection_observer = Jv.get Jv.global "IntersectionObserver"
  let create obs opts = Jv.new' intersection_observer [| Jv.repr obs; opts |]
  let disconnect o = ignore @@ Jv.call o "disconnect" [||]
  let observe o el = ignore @@ Jv.call o "observe" [| El.to_jv el |]
end

(* Error management

   TODO trigger error event and allow to disable logging *)

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e
let error str = Error (Jv.Error.v str)
let reword_error f e = error (f (Jv.Error.message e))
let el_log_if_error el = function
| Ok v -> v | Error e -> Console.(error [str "hc: element "; el; e ])

(* Attributes, classes and headers *)

module At = struct
  let query = Jstr.v "data-query"
  let event = Jstr.v "data-event"
  let event_src = Jstr.v "data-event-src"
  let target = Jstr.v "data-target"
  let effect = Jstr.v "data-effect"
  let feedback = Jstr.v "data-feedback"
  let request = Jstr.v "data-request"
  let sel_request = Jstr.v "[data-request]"
end

module Class = struct
  let request = Jstr.v "hc-request"
  let intro = Jstr.v "hc-intro"
  let outro = Jstr.v "hc-outro"
  let error = Jstr.v "hc-error"
end

(* Parsing helpers *)

module Parse = struct
  let tokenize s = Jstr.cuts ~sep:Jstr.sp s
  let error err = Jv.throw err
  let error_key_unknown k = Jstr.(v "unknown key: " + k)
  let rec kv = function
  | [] -> None
  | t :: ts ->
      let k = Jstr.trim t in
      if Jstr.is_empty k then kv ts else
      match Jstr.find_sub ~sub:(Jstr.v ":") k with
      | None -> error Jstr.(v "not a key: " + k)
      | Some i ->
          match i = Jstr.length k - 1 with
          | false ->
              Some (Jstr.slice ~stop:i k, Jstr.slice ~start:(i + 1) k, ts)
          | true ->
              match ts with
              | [] -> error Jstr.(v "key " + k + v ": missing value")
              | v :: ts ->
                  Some (Jstr.slice ~stop:(-1) (Jstr.trim k), Jstr.trim v, ts)
end

(* Selectors *)

module Sel = struct
  type up = Jstr.t * Jstr.t list (* el name (or empty string) and classes *)
  type t = up list * Jstr.t (* up selectors and CSS selector *)

  let rec find_up el (n, classes as up) = match El.parent el with
  | None -> None
  | Some el ->
      let n_match = Jstr.is_empty n || Jstr.equal n (El.tag_name el) in
      let c_match = List.for_all (fun c -> El.class' c el) classes in
      if n_match && c_match then Some el else find_up el up

  let rec find_ups el = function
  | [] -> Some el
  | u :: us -> match find_up el u with None -> None | Some el -> find_ups el us

  let find_first ~start (ups, css_sel) = match find_ups start ups with
  | None -> None
  | Some el when Jstr.is_empty css_sel -> Some el
  | Some el -> El.find_first_by_selector ~root:el css_sel

  let fold_find ~start:s (ups, css_sel) f acc = match find_ups s ups with
  | None -> acc
  | Some el when Jstr.is_empty css_sel -> f el acc
  | Some el -> El.fold_find_by_selector ~root:el f css_sel acc

  let up_str = Jstr.v ":up"
  let dot = Jstr.v "."
  let of_jstr s =
    let rec loop ups i s =
      let next = match Jstr.find_sub ~start:i ~sub:up_str s with
      | None -> None
      | Some j ->
          let n = j + 3 in
          if n = Jstr.length s || Uchar.to_int (Jstr.get s n) = 0x20
          then Some j else None
      in
      match next with
      | None -> List.rev ups, Jstr.trim (Jstr.slice ~start:i s)
      | Some j ->
          let pre = Jstr.trim (Jstr.slice ~start:i ~stop:j s) in
          let ups = match Jstr.cuts ~sep:dot pre with
          | [] -> (Jstr.empty, []) :: ups
          | t :: classes -> (t, classes) :: ups
          in
          loop ups (j + 3) s
    in
    loop [] 0 s

  let to_jstr (ups, css_sel) =
    let up_to_jstr (t, cs) = Jstr.(concat ~sep:dot (t :: cs) + up_str) in
    let ups_to_jstr ups = Jstr.concat ~sep:Jstr.sp (List.map up_to_jstr ups) in
    Jstr.(ups_to_jstr ups + sp + css_sel)
end

(* Durations *)

module Dur_ms = struct
  type t = int
  let to_jstr d = Jstr.(of_int d + Jstr.v "ms")
  let of_jstr s =
    let len = Jstr.length s in
    if len < 2 then None else
    let max = len - 1 in
    if Uchar.to_int (Jstr.get s max) <> 0x0073 (* 's' *) then None else
    let mul, stop =
      if Uchar.to_int (Jstr.get s (max - 1)) = 0x006D (* 'm' *)
      then 1., max - 1 else 1000., max
    in
    let dur = Jstr.to_float (Jstr.slice ~stop s) in
    if Float.is_nan dur || dur < 0. then None else Some (truncate (dur *. mul))

  let parse_value k kv = match of_jstr kv with
  | None -> Parse.error Jstr.(v "key " + k + v ": invalid duration: " + kv)
  | Some dur -> dur
end

(* Events *)

module Event = struct
  type name = Jstr.t
  type modifier =
  | Once | Debounce of Dur_ms.t | Throttle of Dur_ms.t | Filter of Jstr.t

  type t = name * modifier list
  let name (n, _) = n
  let mods (_, mods) = mods

  let parse_name = function
  | [] -> Parse.error (Jstr.v "missing event name")
  | t :: ts -> Jstr.trim t, ts

  let rec parse_mod = function
  | [] -> None
  | t :: rest as ts ->
      let t = Jstr.trim t in
      if Jstr.is_empty t then parse_mod rest else
      if Jstr.(equal t (v "once")) then Some (Once, rest) else
      match Parse.kv ts with
      | None -> None
      | Some (k, v, ts) ->
          if Jstr.(equal k (v "debounce"))
          then Some (Debounce (Dur_ms.parse_value k v), ts) else
          if Jstr.(equal k (v "throttle"))
          then Some (Throttle (Dur_ms.parse_value k v), ts) else
          if Jstr.(equal k (v "filter"))
          then Some (Filter v, ts) else
          Parse.error (Parse.error_key_unknown k )

  let of_jstr s =
    try
      let name, ts = parse_name (Parse.tokenize s) in
      let mods =
        let rec loop acc = function
        | [] -> acc
        | ts ->
            match parse_mod ts with
            | None -> acc | Some (m, ts) -> loop (m :: acc) ts
        in
        loop [] ts
      in
      Ok (name, mods)
    with Jv.Error e -> reword_error Jstr.(append (v "event: ")) e

  let of_el el = match El.at At.event el with
  | Some s ->
      begin match of_jstr s with
      | Error e -> reword_error Jstr.(append (At.event + v ": ")) e
      | Ok _ as v -> v
      end
  | None ->
      let t = El.tag_name el in
      let n =
        if Jstr.(equal t (v "form")) then Ev.Type.name Form.Ev.submit else
        if Jstr.(equal t (v "input") || equal t (v "textarea") ||
                 equal t (v "select"))
        then Ev.Type.name Ev.change
        else Ev.Type.name Ev.click
      in
      Ok (n, [])

  let connect_el cb el () =
    el_log_if_error el @@
    let* ev = of_el el in
    let name = name ev in
    let ev = Ev.Type.create name in
    Ok (Ev.listen ev cb (El.as_target el))

  let connect_descendents cb el =
    El.fold_find_by_selector ~root:el (connect_el cb) At.sel_request ()

  let connect_tree cb el =
    if Option.is_some (El.at At.request el) then connect_el cb el ();
    connect_descendents cb el
end

(* Target *)

module Target = struct
  type t = El.t
  let of_el el = match El.at At.target el with
  | None -> Ok el
  | Some sel ->
      match Sel.find_first ~start:el (Sel.of_jstr sel) with
      | None -> error Jstr.(sel + v " no such target")
      | Some el -> Ok el
end

(* Feedback *)

module Feedback = struct
  type t = El.t list
  let set = El.set_class

  let start_request ~requestel:r fbs =
    let feedback el = set Class.error false el; set Class.request true el in
    List.iter feedback (r :: fbs)

  let error ~requestel:r fbs =
    let feedback el = set Class.error true el; set Class.request false el in
    List.iter feedback (r :: fbs)

  let skip_effect ~requestel:r fbs =
    List.iter (set Class.request false) (r :: fbs)

  let effect_no_intro = skip_effect
  let effect_intro ~requestel:r fbs =
    let feedback el = set Class.request false el; set Class.intro true el in
    El.set_class Class.request false r;
    List.iter feedback fbs

  let effect_no_outro fbs = List.iter (El.set_class Class.intro false) fbs
  let effect_end_outro fbs = List.iter (El.set_class Class.outro false) fbs
  let effect_start_outro fbs =
    let feedback el = set Class.intro false el; set Class.outro true el in
    List.iter feedback fbs

  let of_el el ~target = match El.at At.feedback el with
  | None -> [target]
  | Some sel -> Sel.fold_find ~start:el (Sel.of_jstr sel) List.cons []
end

(* Effects *)

module Effect = struct
  open Fut.Syntax

  type kind = Inner | Inplace | Insert of Jstr.t | None' | Event of Jstr.t
  type t = kind * Dur_ms.t * Dur_ms.t

  let do_intro ~requestel ~feedback intro =
    if intro = 0
    then (Feedback.effect_no_intro ~requestel feedback; Fut.return ())
    else (Feedback.effect_intro ~requestel feedback; Fut.tick ~ms:intro)

  let do_outro ~feedback outro =
    if outro = 0 then (Feedback.effect_no_outro feedback; Fut.return ()) else
    let* () = Feedback.effect_start_outro feedback; Fut.tick ~ms:outro in
    Fut.return (Feedback.effect_end_outro feedback)

  let do_effect k ~target ~feedback html_part ~outro = match k with
  | Inplace ->
        (* XXX maybe we should do this for all adds. *)
        let target_is_feedback = List.exists (( == ) target) feedback in
        if target_is_feedback then begin
          let el_parent = El.parent target |> Option.get in
          let observe records o =
            let feedback = ref [] in
            for i = 0 to (Jv.Int.get records "length") - 1 do
              let r = Jv.Jarray.get records i in
              let adds = Jv.get r "addedNodes" in
              for i = 0 to (Jv.Int.get adds "length") - 1 do
                let n = El.of_jv @@ Jv.call adds "item" [|Jv.of_int i|] in
                if El.is_el n
                then (feedback := n :: !feedback)
              done;
            done;
            do_outro ~feedback:!feedback outro
          in
          let obs = Mutation_observer.create observe in
          let opts = Jv.obj [| "childList", Jv.true' |] in
          Mutation_observer.observe obs el_parent opts;
          Jv.Jstr.set (El.to_jv target) "outerHTML" html_part;
          Fut.return ()
        end else begin
          Jv.Jstr.set (El.to_jv target) "outerHTML" html_part;
          do_outro ~feedback outro
        end
  | Inner ->
      Jv.Jstr.set (El.to_jv target) "innerHTML" html_part;
      do_outro ~feedback outro
  | Insert pos ->
      let args = Jv.[|of_jstr pos; of_jstr html_part|] in
      ignore @@ Jv.call (El.to_jv target) "insertAdjacentHTML" args;
      do_outro ~feedback outro
  | Event ev ->
      let ev = Ev.create (Ev.Type.create ev) in
      ignore (Ev.dispatch ev (El.as_target target));
      do_outro ~feedback outro
  | None' ->
      do_outro ~feedback outro

  let apply ~requestel ~target ~feedback (k, intro, outro) html_part =
    let* () = do_intro ~requestel ~feedback intro in
    do_effect k ~target ~feedback html_part ~outro

  let rec parse_kind = function
  | [] -> Parse.error (Jstr.v "missing effect")
  | t :: ts ->
      let t = Jstr.trim t in
      if Jstr.(equal t (v "inner")) then Inner, ts else
      if Jstr.(equal t (v "inplace")) then Inplace, ts else
      if Jstr.(equal t (v "event")) then match ts with
      | e :: ts -> Event e, ts | _ -> Parse.error (Jstr.v "missing event name")
      else
      if Jstr.(equal t (v "beforebegin") || equal t (v "afterbegin") ||
               equal t (v "beforeend") || equal t (v "afterend"))
      then Insert t, ts
      else Parse.error Jstr.(v "unknown effect: " + t)

  let of_jstr s =
    try
      let kind, ts = parse_kind (Parse.tokenize s) in
      let intro, outro =
        let rec loop intro outro = function
        | [] -> intro, outro
        | t :: _ as ts ->
            match Parse.kv ts with
            | None -> intro, outro
            | Some (k, v, ts) ->
                if Jstr.(equal k (v "intro"))
                then loop (Dur_ms.parse_value k v) outro ts else
                if Jstr.(equal k (v "outro"))
                then loop intro (Dur_ms.parse_value k v) ts else
                Parse.error Jstr.(v "unknown key: " + k)
        in
        loop 0 0 ts
      in
      Ok (kind, intro, outro)
    with Jv.Error e -> reword_error Jstr.(append (v "effect: ")) e

  let of_el el = match El.at At.effect el with
  | None -> Ok (Inner, 0, 0)
  | Some s ->
      match of_jstr s with
      | Error e -> reword_error Jstr.(append (At.effect + v ": ")) e
      | Ok _ as v -> v
end

(* Headers *)

module Header = struct
  let redirect = Jstr.v "hc-redirect"
  let reload = Jstr.v "hc-reload"

  let header_error h msg = Jstr.(v "header " + h + v ": " + msg)

  let response_redirect ~requestel feedback hs =
    match Fetch.Headers.find redirect hs with
    | None -> Ok ()
    | Some url ->
        let base = Uri.to_jstr (Window.location G.window) in
        match Uri.of_jstr ~base url with
        | Error e -> reword_error (header_error redirect) e
        | Ok url ->
            Feedback.skip_effect ~requestel feedback;
            Window.set_location G.window url; (* goodbye *) Ok ()

  let response_reload ~requestel feedback hs =
    match Fetch.Headers.find reload hs with
    | None -> Ok ()
    | Some bv ->
        match Jstr.equal (Jstr.v "true") bv with
        | true ->
            Feedback.skip_effect ~requestel feedback;
            Window.reload G.window; (* goodbye *) Ok ()
        | false ->
            if Jstr.equal (Jstr.v "false") bv then Ok () else
            error (header_error reload Jstr.(v ": invalid value: " + bv))

  let handle_response ~requestel feedback hs =
    let* () = response_redirect ~requestel feedback hs in
    let* () = response_reload ~requestel feedback hs in
    Ok ()
end

(* Query *)

module Query = struct
  let append_form_data fd0 fd1 =
    if Form.Data.is_empty fd0 then (* avoid append dance *) fd1 else
    let append_entry k v fd = match v with
    | `String v -> Form.Data.append fd k v; fd
    | `File f -> Form.Data.append_blob fd k (File.as_blob f); fd
    in
    Form.Data.fold append_entry fd1 fd0

  let append_el_query_data el fd =
    match Jstr.(equal (El.tag_name el) (Jstr.v "form")) with
    | true -> append_form_data fd (Form.Data.of_form (Form.of_el el))
    | false ->
        match El.at Brr.At.Name.value el with
        | None -> fd
        | Some v -> Form.Data.append fd (Jstr.v "value") v; fd

  let of_el el ~url =
    let ps = Uri.Params.of_jstr (Uri.query url) in
    let fd = Form.Data.of_uri_params ps in
    match El.at At.query el with
    | None -> append_el_query_data el fd
    | Some sel ->
        let sel = Sel.of_jstr sel in
        Sel.fold_find ~start:el sel append_el_query_data fd
end

(* Requests *)

module Request = struct
  type t = Uri.t * [ `Websocket | `Sse | `Http of Jstr.t ]
  let nobase = Jstr.v "nobase:/" (* Js URL can't be relative *)
  let nobase_scheme = Jstr.v "nobase"
  let of_jstr s =
    let m, url = match Jstr.find_sub ~sub:Jstr.sp s with
    | None -> Jstr.empty, s
    | Some i -> Jstr.slice ~stop:i s, Jstr.slice ~start:(i + 1) s
    in
    let* url = Uri.of_jstr ~base:nobase url in
    let s = Uri.scheme url in
    if Jstr.(is_empty m && (equal s (v "ws") || equal s (v "wss")))
    then Ok (url, `Websocket) else
    if Jstr.(equal m (v "sse") || equal m (v "SSE"))
    then Ok (url, `Sse)
    else Ok (url, `Http (if Jstr.is_empty m then Jstr.v "GET" else m))

  let real_url u =
    if Jstr.equal (Uri.scheme u) nobase_scheme
    then Uri.path u else Uri.to_jstr u

  let to_fetch_request url meth query =
    match Jstr.(equal meth (v "GET") || equal meth (v "HEAD")) with
    | true ->
        let url = real_url url in
        let q = Uri.Params.to_jstr (Form.Data.to_uri_params query) in
        let url = if Jstr.is_empty q then url else Jstr.(url + v "?" + q) in
        let init = Fetch.Request.init ~method':meth () in
        Fetch.Request.v ~init url
    | false ->
        let url = real_url url in
        let body = match Form.Data.has_file_entry query with
        | true -> Fetch.Body.of_form_data query
        | false -> Fetch.Body.of_uri_params (Form.Data.to_uri_params query)
        in
        let init = Fetch.Request.init ~method':meth ~body () in
        Fetch.Request.v ~init url
end

let http_request requestel meth url query target effect feedback =
  let open Fut.Syntax in
  Feedback.start_request ~requestel feedback;
  let resp = Fetch.request (Request.to_fetch_request url meth query) in
  let resp = Fut.bind resp @@ function
  | Error _ as e -> Feedback.error ~requestel feedback; Fut.return e
  | Ok resp ->
      let hs = Fetch.Response.headers resp in
      (* TODO do stuff with status ? *)
      match Header.handle_response ~requestel feedback hs with
      | Error _ as e -> Feedback.error ~requestel feedback; Fut.return e
      | Ok () ->
          let* html = Fetch.Body.text (Fetch.Response.as_body resp) in
          match html with
          | Error _ as e -> Feedback.error ~requestel feedback; Fut.return e
          | Ok html ->
              let* () =
                Effect.apply ~requestel ~target ~feedback effect html
              in
              Fut.ok ()
  in
  Fut.await resp (el_log_if_error requestel);
  Ok ()

let sse_request el url query target effect feedback =
  error (Jstr.v "SSE unimplemented")

let websocket_request el url query target effect feedback =
  error (Jstr.v "Websockets unimplemented")

let prevent_some_default ev = (* TODO unconditional ? *)
  let n = Ev.Type.name (Ev.type' ev) in
  if Jstr.(equal n (v "submit")) then Ev.prevent_default ev else ()

let do_request ev =
  prevent_some_default ev;
  let el = El.of_jv @@ Ev.target_to_jv (Ev.target ev) in
  el_log_if_error el @@
  match El.at At.request el with
  | None -> Ok () (* data-request attribute removed meanwhile ignore *)
  | Some req ->
      let* url, kind = Request.of_jstr req in
      let query = Query.of_el el ~url in
      let* target = Target.of_el el in
      let* effect = Effect.of_el el in
      let feedback = Feedback.of_el el ~target in
      match kind with
      | `Http meth ->
          http_request el meth url query target effect feedback
      | `Sse ->
          sse_request el url query target effect feedback
      | `Websocket ->
          websocket_request el url query target effect feedback

let install_observer () = (* Observe DOM additions and removals *)
  let obs records _obs =
    let in_html_dom n =
      Jv.call (El.to_jv n) "getRootNode" [||] == Document.to_jv @@ G.document
    in
    for i = 0 to (Jv.Int.get records "length") - 1 do
      let r = Jv.Jarray.get records i in
      let adds = Jv.get r "addedNodes" in
      for i = 0 to (Jv.Int.get adds "length") - 1 do
        let n = El.of_jv @@ Jv.call adds "item" [|Jv.of_int i|] in
        if in_html_dom n && El.is_el n
        then (Event.connect_tree do_request n)
      done;
      let rems = Jv.get r "removedNodes" in
      for i = 0 to (Jv.Int.get rems "length") - 1 do
        let n = El.of_jv @@ Jv.call rems "item" [|Jv.of_int i|] in
        if not (in_html_dom n) then () (* TODO gc data-event-src listeners. *)
      done
    done
  in
  let o = Mutation_observer.create obs in
  let opts = Jv.obj [| "childList", Jv.true'; "subtree", Jv.true' |] in
  Mutation_observer.observe o (Document.root G.document) opts

let init () =
  Event.connect_descendents do_request (Document.root G.document);
  install_observer ()

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
