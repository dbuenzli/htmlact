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

let ev_cycle_start = Ev.Type.create (Jstr.v "hc-cycle-start")
let ev_cycle_end = Ev.Type.create (Jstr.v "hc-cycle-end")
let ev_cycle_error = Ev.Type.create (Jstr.v "hc-cycle-error")
let ev_hc_in = Ev.Type.create (Jstr.v "hc-in")

let send_cycle_ev ev =
  (* FIXME maybe we coud honour the Js cancellation stuff. *)
  ignore (Ev.dispatch (Ev.create ev) (Document.as_target G.document))

(* Error management

   TODO trigger error event and allow to disable logging *)

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e
let error str = Error (Jv.Error.v str)
let reword_error f e = error (f (Jv.Error.message e))
let el_log_if_error el = function
| Ok v -> v
| Error e ->
    send_cycle_ev ev_cycle_error;
    Console.(error [str "hc: element "; el; e ])

(* Attributes, classes and headers *)

module At = struct
  let query = Jstr.v "data-query"
  let query_rescue = Jstr.v "data-query-rescue"
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
  let error = Jstr.v "hc-error"
  let in' = Jstr.v "hc-in"
  let in_parent = Jstr.v "hc-in-parent"
  let out = Jstr.v "hc-out"
  let out_parent = Jstr.v "hc-out-parent"
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
        let v = El.prop El.Prop.value el in
        if Jstr.is_empty v then fd else
        let n = El.prop El.Prop.name el in
        let n = if Jstr.is_empty n then Jstr.v "value" else n in
        Form.Data.append fd n v; fd

  let of_el ?url el =
    let fd = match url with
    | None -> Form.Data.create ()
    | Some url ->
        let ps = Uri.Params.of_jstr (Uri.query url) in
        Form.Data.of_uri_params ps
    in
    match El.at At.query el with
    | None -> append_el_query_data el fd
    | Some sel ->
        let sel = Sel.of_jstr sel in
        Sel.fold_find ~start:el sel append_el_query_data fd

  (* Rescue *)

  let hc_rescue_stamp = Jstr.v "data-hc-rescue-stamp"
  let hc_rescue_stamp_prop = El.Prop.jstr hc_rescue_stamp

  let rescue_stamp el =
    Uri.Params.to_jstr (Brr_io.Form.Data.to_uri_params (of_el el))

  let stamp_if_needed el = match El.at At.query_rescue el with
  | None -> ()
  | Some rescue ->
      if not (Jstr.equal (Jstr.v "true") rescue) then () else
      El.set_prop hc_rescue_stamp_prop (rescue_stamp el) el

  let stamp_changed el =
    not (Jstr.equal (rescue_stamp el) (El.prop hc_rescue_stamp_prop el))

  let cancel_rescue_if_needed el = El.set_at At.query_rescue None el

  let rescue_on_beforeunload ev =
    let check_els = Jstr.(v "[" + At.query_rescue + v "='true']") in
    let check el rescue = rescue || stamp_changed el in
    let rescue = El.fold_find_by_selector check check_els false in
    if not rescue then () else
    begin
      Ev.prevent_default ev;
      Jv.set (Ev.to_jv ev)
        "returnValue" (Jv.of_jstr Jstr.empty) (* for chrome *)
    end
end

(* Events *)

module Event = struct
  type name = Jstr.t
  type t =
    { name : name;
      once : bool;
      debounce_ms : Dur_ms.t;
      throttle_ms : Dur_ms.t;
      filter : Jstr.t option;  }

  let name e = e.name
  let mods (_, mods) = mods

  let parse_name = function
  | [] -> Parse.error (Jstr.v "missing event name")
  | t :: ts -> Jstr.trim t, ts

  let rec parse_mods o d t f = function
  | [] -> o, d, t, f
  | tok :: rest as ts ->
      let tok = Jstr.trim tok in
      if Jstr.is_empty tok then parse_mods o d t f ts else
      if Jstr.(equal tok (v "once")) then parse_mods true d t f ts else
      match Parse.kv ts with
      | None -> o, d, t, f
      | Some (k, v, ts) ->
          if Jstr.(equal k (v "debounce"))
          then parse_mods o (Dur_ms.parse_value k v) t f ts else
          if Jstr.(equal k (v "throttle"))
          then parse_mods o d (Dur_ms.parse_value k v) f ts else
          if Jstr.(equal k (v "filter"))
          then parse_mods o d t (Some v) ts else
          Parse.error (Parse.error_key_unknown k )

  let of_jstr s =
    try
      let name, ts = parse_name (Parse.tokenize s) in
      let once, debounce_ms, throttle_ms, filter = parse_mods false 0 0 None ts
      in
      Ok {name; once; debounce_ms; throttle_ms; filter}
    with Jv.Error e -> reword_error Jstr.(append (v "event: ")) e

  let of_el el = match El.at At.event el with
  | Some s ->
      begin match of_jstr s with
      | Error e -> reword_error Jstr.(append (At.event + v ": ")) e
      | Ok _ as v -> v
      end
  | None ->
      let t = El.tag_name el in
      let name =
        if Jstr.(equal t (v "form")) then Ev.Type.name Form.Ev.submit else
        if Jstr.(equal t (v "input") || equal t (v "textarea") ||
                 equal t (v "select"))
        then Ev.Type.name Ev.change
        else Ev.Type.name Ev.click
      in
      Ok { name; once = false; debounce_ms = 0; throttle_ms = 0; filter = None }

  let filter ev e = match ev.filter with
  | None -> true
  | Some f -> Jv.to_bool (Jv.apply (Jv.get' Jv.global f) [| Ev.to_jv e |])

  let prevent_some_default ev = (* TODO unconditional ? *)
    let n = Ev.Type.name (Ev.type' ev) in
    if Jstr.(equal n (v "submit")) then Ev.prevent_default ev else ()

  let once ev el cb =
    let open Fut.Syntax in
    let etype = Ev.Type.create ev.name in
    let target = El.as_target el in
    let* e = Ev.next etype target in
    prevent_some_default e;
    if not (filter ev e) then Fut.return () else
    if ev.debounce_ms <= 0 then Fut.return (cb e) else
    let* () = Fut.tick ~ms:ev.debounce_ms in
    Fut.return (cb e)

  let debounce ev cb =
    let tid = ref 0 in
    fun e ->
      prevent_some_default e;
      if not (filter ev e) then () else
      (G.stop_timer !tid;
       tid := G.set_timeout ~ms:ev.debounce_ms (fun () -> cb e))

  let throttle ev cb =
    let suppress = ref false in
    fun e ->
      prevent_some_default e;
      if not (filter ev e) then () else
      if !suppress then () else
      (cb e; suppress := true;
       ignore (G.set_timeout ~ms:ev.throttle_ms (fun () -> suppress := false)))

  let listen ev el cb =
    let etype = Ev.Type.create ev.name in
    let target = El.as_target el in
    let cb =
      if ev.debounce_ms <> 0 then debounce ev cb else
      if ev.throttle_ms <> 0 then throttle ev cb else
      fun e ->
        prevent_some_default e;
        if filter ev e then cb e else ()
    in
    Ok (Ev.listen etype cb target)

  let connect_el cb el () =
    Query.stamp_if_needed el;
    el_log_if_error el @@
    let* ev = of_el el in
    if ev.once then Ok (ignore (once ev el cb)) else
    listen ev el cb

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
      | None -> error Jstr.(v "\"" + sel + v "\" no such target")
      | Some el -> Ok el
end

(* Feedback *)

module Feedback = struct
  type t = El.t list
  let set = El.set_class

  let start_request ~requestel:r fbs =
    let feedback el = set Class.error false el; set Class.request true el in
    List.iter feedback (r :: fbs)

  let error_request ~requestel:r fbs =
    let feedback el = set Class.error true el; set Class.request false el in
    List.iter feedback (r :: fbs)

  let end_request ~requestel:r fbs =
    List.iter (set Class.request false) (r :: fbs)

  let remove ~parent ~removes b =
    El.set_class Class.out_parent b parent;
    List.iter (El.set_class Class.out b) removes

  let insert ~parent ~inserts b =
    El.set_class Class.in_parent b parent;
    List.iter (El.set_class Class.in' b) inserts

  let of_el el ~target = match El.at At.feedback el with
  | None -> [target]
  | Some sel -> Sel.fold_find ~start:el (Sel.of_jstr sel) List.cons []
end

(* Effects *)

module Effect = struct
  open Fut.Syntax

  type kind = Element | Children | Insert of Jstr.t | None' | Event of Jstr.t
  type t = kind

  let remove_duration el =
    let style_duration k el =
      let dur = El.computed_style k el in
      if Jstr.is_empty dur then 0 else
      match Dur_ms.of_jstr dur with
      | Some d -> d
      | None ->
          let err = Jstr.(k + v ": parse error: " + dur) in
          el_log_if_error el (error err); 0
    in
    let dur = style_duration (Jstr.v "--hc-out-duration") el in
    let dur1 = style_duration (Jstr.v "animation-duration") el in
    let dur2 = style_duration (Jstr.v "transition-duration") el in
    let dur = if dur > dur1 then dur else dur1 in
    let dur = if dur > dur2 then dur else dur2 in
    dur

  let remove_duration ~parent ~removes =
    let dur acc el =
      let dur = remove_duration el in
      if dur > acc then dur else acc
    in
    List.fold_left dur 0 (parent :: removes)

  let feedback_remove ~target kind =
    let rem = match kind with
    | Element -> Some (El.parent target |> Option.get, [target])
    | Children -> Some (target, El.children target)
    | _ -> None
    in
    match rem with
    | None -> Fut.return ()
    | Some (parent, removes) ->
        Feedback.remove ~parent ~removes true;
        let* () = Fut.tick ~ms:(remove_duration ~parent ~removes) in
        Feedback.remove ~parent ~removes false;
        Fut.return ()

  let feedback_insert ~parent ~inserts =
    if (Ev.dispatch (Ev.create ev_hc_in) (El.as_target parent)) then begin
      Feedback.insert ~parent ~inserts true;
      (* Make sure we had a render of [true]. *)
      ignore (G.request_animation_frame @@ fun _ ->
      ignore (G.request_animation_frame @@ fun _ ->
              Feedback.insert ~parent ~inserts false))
    end

  let apply_element ~target html_part =
    let parent = El.parent target |> Option.get in
    let observe records o =
      let inserts = ref [] in
      for i = 0 to (Jv.Int.get records "length") - 1 do
        let r = Jv.Jarray.get records i in
        let adds = Jv.get r "addedNodes" in
        for i = 0 to (Jv.Int.get adds "length") - 1 do
          let n = El.of_jv @@ Jv.call adds "item" [|Jv.of_int i|] in
          if El.is_el n
          then (inserts := n :: !inserts)
        done;
      done;
      feedback_insert ~parent ~inserts:!inserts
    in
    let obs = Mutation_observer.create observe in
    let opts = Jv.obj [| "childList", Jv.true' |] in
    Mutation_observer.observe obs parent opts;
    Jv.Jstr.set (El.to_jv target) "outerHTML" html_part

  let apply_children ~target html_part =
    Jv.Jstr.set (El.to_jv target) "innerHTML" html_part;
    feedback_insert ~parent:target ~inserts:(El.children target)

  let apply_insert ~target pos html_part =
    let before el = El.of_jv @@ Jv.call (El.to_jv el) "previousSibling" [||] in
    let after el = El.of_jv @@ Jv.call (El.to_jv el) "nextSibling" [||] in
    let args = Jv.[|of_jstr pos; of_jstr html_part|] in
    ignore @@ Jv.call (El.to_jv target) "insertAdjacentHTML" args;
    let parent, insert =
      if Jstr.(equal pos (v "beforebegin"))
      then El.parent target |> Option.get, before target else
      if Jstr.(equal pos (v "afterbegin"))
      then target, List.hd (El.children target) else
      if Jstr.(equal pos (v "beforeend"))
      then target, List.hd (List.rev (El.children target)) else
      if Jstr.(equal pos (v "afterend"))
      then El.parent target |> Option.get, after target else
      assert false
    in
    feedback_insert ~parent ~inserts:[insert]

  let apply_event ~target ev =
    let ev = Ev.create (Ev.Type.create ev) in
    ignore (Ev.dispatch ev (El.as_target target))

  let apply ~target kind html_part =
    let* () = feedback_remove ~target kind in
    begin match kind with
    | Element -> apply_element ~target html_part
    | Children -> apply_children ~target html_part
    | Insert pos -> apply_insert ~target pos html_part
    | Event ev -> apply_event ~target ev
    | None' -> ()
    end;
    Fut.return ()

  let rec parse_kind = function
  | [] -> Parse.error (Jstr.v "missing effect")
  | t :: ts ->
      let t = Jstr.trim t in
      if Jstr.(equal t (v "element")) then Element, ts else
      if Jstr.(equal t (v "children")) then Children, ts else
      if Jstr.(equal t (v "none")) then None', ts else
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
      if ts <> [] then Parse.error Jstr.(v "unexpected token: " + List.hd ts);
      Ok kind
    with Jv.Error e -> reword_error Jstr.(append (v "effect: ")) e

  let of_el el = match El.at At.effect el with
  | None -> Ok Children
  | Some s ->
      match of_jstr s with
      | Error e -> reword_error Jstr.(append (At.effect + v ": ")) e
      | Ok _ as v -> v
end

(* Headers *)

module Header = struct
  let hc = Jstr.v "hc"
  let redirect = Jstr.v "hc-redirect"
  let reload = Jstr.v "hc-reload"
  let location_push = Jstr.v "hc-location-push"
  let location_replace = Jstr.v "hc-location-replace"
  let location_title = Jstr.v "hc-location-title"

  let header_error h msg = Jstr.(v "header " + h + v ": " + msg)

  let response_redirect ~requestel feedback hs =
    match Fetch.Headers.find redirect hs with
    | None -> Ok ()
    | Some url ->
        let base = Uri.to_jstr (Window.location G.window) in
        match Uri.of_jstr ~base url with
        | Error e -> reword_error (header_error redirect) e
        | Ok url ->
            Feedback.end_request ~requestel feedback;
            Window.set_location G.window url; (* goodbye *) Ok ()

  let response_reload ~requestel feedback hs =
    match Fetch.Headers.find reload hs with
    | None -> Ok ()
    | Some bv ->
        match Jstr.equal (Jstr.v "true") bv with
        | true ->
            Feedback.end_request ~requestel feedback;
            Window.reload G.window; (* goodbye *) Ok ()
        | false ->
            if Jstr.equal (Jstr.v "false") bv then Ok () else
            error (header_error reload Jstr.(v ": invalid value: " + bv))

  let response_location_title ~requestel feedback hs =
    match Fetch.Headers.find location_title hs with
    | None -> Ok ()
    | Some title' ->
        let* title' = match Uri.decode title' (* pct-decode *) with
        | Error e -> reword_error (header_error location_title) e
        | Ok _ as v -> v
        in
        let title = Document.title G.document in
        if Jstr.equal title title' then Ok () else
        Ok (Document.set_title G.document title')

  let response_location header action ~requestel feedback hs =
    match Fetch.Headers.find header hs with
    | None -> Ok false
    | Some url ->
        let h = Window.history G.window in
        let base = Uri.to_jstr (Window.location G.window) in
        match Uri.of_jstr ~base url with
        | Error e -> reword_error (header_error header) e
        | Ok uri ->
            let uri_str = Uri.to_jstr uri in
            if Jstr.equal uri_str base
            then Ok false
            else Ok (action ?state:None ?title:None ?uri:(Some uri) h; true)

  let response_location_replace ~requestel feedback hs =
    response_location location_replace Window.History.replace_state ~requestel
      feedback hs

  let response_location_push ~requestel feedback hs =
    response_location location_push Window.History.push_state ~requestel
      feedback hs

  let handle_response ~requestel feedback hs =
    let* did_push = response_location_push ~requestel feedback hs in
    let* _did_rep =
      if did_push then Ok false else
      response_location_replace ~requestel feedback hs
    in
    let* () = response_location_title ~requestel feedback hs in
    let* () = response_redirect ~requestel feedback hs in
    let* () = response_reload ~requestel feedback hs in
    Ok ()
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

  let headers = Fetch.Headers.of_assoc [Header.hc, Jstr.v "true"]
  let referrer_policy = Jstr.v "same-origin"

  let to_fetch_request url meth query =
    match Jstr.(equal meth (v "GET") || equal meth (v "HEAD")) with
    | true ->
        let url = real_url url in
        let q = Uri.Params.to_jstr (Form.Data.to_uri_params query) in
        let url = if Jstr.is_empty q then url else Jstr.(url + v "?" + q) in
        let redirect = Fetch.Request.Redirect.follow in
        let method' = meth in
        let init =
          Fetch.Request.init ~referrer_policy ~headers ~redirect ~method' ()
        in
        Fetch.Request.v ~init url
    | false ->
        let url = real_url url in
        let body = match Form.Data.has_file_entry query with
        | true -> Fetch.Body.of_form_data query
        | false -> Fetch.Body.of_uri_params (Form.Data.to_uri_params query)
        in
        let redirect = Fetch.Request.Redirect.follow in
        let method' = meth in
        let init =
          Fetch.Request.init
            ~referrer_policy ~headers ~redirect ~method' ~body ()
        in
        Fetch.Request.v ~init url
end

let http_request requestel meth url query target effect feedback =
  let open Fut.Syntax in
  Feedback.start_request ~requestel feedback;
  let resp = Fetch.request (Request.to_fetch_request url meth query) in
  let resp = Fut.bind resp @@ function
  | Error _ as e -> Feedback.error_request ~requestel feedback; Fut.return e
  | Ok resp ->
      let hs = Fetch.Response.headers resp in
      let () = Query.cancel_rescue_if_needed requestel in
      (* TODO do stuff with status ? *)
      match Header.handle_response ~requestel feedback hs with
      | Error _ as e -> Feedback.error_request ~requestel feedback; Fut.return e
      | Ok () ->
          let* html = Fetch.Body.text (Fetch.Response.as_body resp) in
          match html with
          | Error _ as e ->
              Feedback.error_request ~requestel feedback; Fut.return e
          | Ok html ->
              Feedback.end_request ~requestel feedback;
              let* () = Effect.apply ~target effect html in
              send_cycle_ev ev_cycle_end;
              Fut.ok ()
  in
  Fut.await resp (el_log_if_error requestel);
  Ok ()

let sse_request el url query target effect feedback =
  error (Jstr.v "SSE unimplemented")

let websocket_request el url query target effect feedback =
  error (Jstr.v "Websockets unimplemented")

let do_request ev =
  send_cycle_ev ev_cycle_start;
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

let install_query_rescue () = (* Rescues changed query data *)
  let window = Window.as_target G.window in
  Ev.listen Ev.beforeunload Query.rescue_on_beforeunload window

let init () =
  Event.connect_descendents do_request (Document.root G.document);
  install_observer ();
  install_query_rescue ()

module Ev = struct
  let cycle_start = ev_cycle_start
  let cycle_end = ev_cycle_end
  let cycle_error = ev_cycle_error
  let hc_in = ev_hc_in
end

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
