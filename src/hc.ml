(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_html

let strf = Printf.sprintf

let request ?meth url =
  let req = match meth with
  | None -> url
  | Some `Sse -> strf "SSE %s" url
  | Some (#Http.meth as m) -> strf "%s %s" (Http.Meth.encode m) url
  in
  At.v "data-request" req

let request_path ?meth p = request ?meth (Http.Path.encode p)
let query v = At.v "data-query" v
let query_rescue v = At.v "data-query-rescue" @@ match v with
| `Force -> "force"
| `Bool b -> (string_of_bool b)

let target v = At.v "data-target" v
let event_src v = At.v "data-event-src" v

type dur_ms = int

let event ?(once = false) ?debounce_ms ?throttle_ms ?filter event =
  let dur k = function None -> "" | Some d -> strf " %s:%dms" k d in
  let once = if once then " once" else "" in
  let debounce = dur "debounce" debounce_ms in
  let throttle = dur "throttle" throttle_ms in
  let filter = match filter with None -> "" | Some f -> " filter:" ^ f in
  let v = String.concat "" [event; once; debounce; throttle; filter] in
  At.v "data-event" v

type effect_kind =
[ `Element | `Children | `Beforebegin | `Afterbegin | `Beforeend | `Afterend
| `None | `Event of string ]

let effect_kind_to_string = function
| `Element -> "element" | `Children -> "children"
| `Beforebegin -> "beforebegin" | `Afterbegin -> "afterbegin"
| `Beforeend -> "beforeend" | `Afterend -> "afterend"
| `None -> "none" | `Event ev -> "event " ^ ev

let effect k = At.v "data-effect" (effect_kind_to_string k)
let feedback v = At.v "data-feedback" v

(* Durations. *)

module Dur = struct
  let notice = 100
  let short = 250
  let short_outro = 200
  let medium = 500
  let medium_outro = 400
  let long = 1000
  let long_outro = 750
end

(* Headers *)

let hc = Http.Name.v "hc"
let redirect = Http.Name.v "hc-redirect"
let reload = Http.Name.v "hc-reload"
let location_push = Http.Name.v "hc-location-push"
let location_replace = Http.Name.v "hc-location-replace"
let location_title = Http.Name.v "hc-location-title"
let encode_location_title = Http.Pct.encode `Uri

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
