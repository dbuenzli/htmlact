(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

let strf = Printf.sprintf

let request ?method' url =
  let req = match method' with
  | None -> url
  | Some `Sse -> strf "SSE %s" url
  | Some (#Http.Method.t as m) -> strf "%s %s" (Http.Method.encode m) url
  in
  At.v "data-request" req

let request_path ?method' p = request ?method' (Http.Path.encode p)
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

type referrer_policy =
[ `No_referrer | `No_referrer_when_downgrade | `Origin
| `Origin_when_cross_origin | `Same_origin | `Strict_origin
| `Strict_origin_when_cross_origin | `Unsafe_url | `Other of string ]

let referrer_policy_to_string = function
| `No_referrer -> "no-referrer"
| `No_referrer_when_downgrade -> "no-referrer-when-downgrade"
| `Origin -> "origin"
| `Origin_when_cross_origin -> "origin-when-cross-origin"
| `Same_origin -> "same-origin"
| `Strict_origin -> "strict-origin"
| `Strict_origin_when_cross_origin -> "strict-origin-when-cross-origin"
| `Unsafe_url -> "unsafe-url"
| `Other o -> o

let referrer_policy p =
  At.v "data-referrer-policy" (referrer_policy_to_string p)

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

let htmlact = Http.Headers.name "htmlact"
let redirect = Http.Headers.name "htmlact-redirect"
let reload = Http.Headers.name "htmlact-reload"
let location_push = Http.Headers.name "htmlact-location-push"
let location_replace = Http.Headers.name "htmlact-location-replace"
let location_title = Http.Headers.name "htmlact-location-title"
let encode_location_title = Url.Percent.encode `Uri
