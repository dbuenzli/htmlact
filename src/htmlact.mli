(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [Htmlit] HTML generation and [Webs] HTTP headers.

    These functions help generate [htmlact] {{!page-manual.attref}data
    attributes} and {{!page-manual.response_headers}HTTP headers} with
    {!Htmlit} and {!Webs}. Ignore if you are using something else to
    generate your HTML or interact with HTTP. *)

open Webs
open Htmlit

(** {1:atts Attributes} *)

val request : ?method':[< Http.Method.t | `Sse] -> string -> At.t
(** [request ~method' r] is a {{!page-manual.data_request}[data-request]}
    attribute for URL [r] using method [method'] (defaults to [`GET]). *)

val request_path : ?method':[< Http.Method.t | `Sse] -> Http.Path.t -> At.t
(** [request ~meth p] is a {{!page-manual.data_request}[data-request]}
    attribute for URL [p] using method [method'] (defaults to [`GET]). *)

val query : string -> At.t
(** [query sel] is the {{!page-manual.data_target}[data-query]} attribute
    [sel]. *)

val query_rescue : [`Bool of bool | `Force] -> At.t
(** [query_rescue b]
    is the {{!page-manual.data_query_rescue}[data-query-rescue]}
    attribute [b]. *)

val event_src : string -> At.t
(** [event_src sel] is the {{!page-manual.data_event_src}[data-event-src]}
    attribute [sel]. *)

type dur_ms = int
(** The type for millisecond durations. *)

val event : ?once:bool -> ?debounce_ms:dur_ms -> ?throttle_ms:dur_ms ->
  ?filter:string -> string -> At.t
(** [event ()] is the {{!page-manual.data_event}data-event} attribute
    [ev]. *)

val target : string -> At.t
(** [target sel] is the {{!page-manual.data_target}[data-target]} attribute
    [sel]. *)

type effect_kind =
[ `Element | `Children | `Beforebegin | `Afterbegin | `Beforeend | `Afterend
| `None | `Event of string ]
(** The type for kind of {{!page-manual.data_effect}request effects}. *)

val effect : effect_kind -> At.t
(** [effect e] is the {{!page-manual.data_effect}[data-effect]} attribute
    [e]. *)

val feedback : string -> At.t
(** [feedback sel] is the {{!page-manual.data_feedback}[data-feedback]}
    attribute [sel]. *)

type referrer_policy =
[ `No_referrer
| `No_referrer_when_downgrade
| `Origin
| `Origin_when_cross_origin
| `Same_origin
| `Strict_origin
| `Strict_origin_when_cross_origin
| `Unsafe_url
| `Other of string ]
(** The type for {{!page-manual.data_referrer_policy}referrer policies}. *)

val referrer_policy : referrer_policy -> At.t
(** [referrer_policy policy] is the {{!page-manual.data_referrer_policy}
    [data-referrer-policy]} attribute [policy]. *)

(** {1:dur Durations} *)

(** Named durations.

    These are for convenience. Most animations should simply use
    {!Dur.short}. {!Dur.notice} is the time after which you want to start
    to show spinners on requests.

    Useful to match with corresponding CSS declaration:
{[
:root
{ --dur-notice: 100ms;
  --dur-short: 250ms;
  --dur-short-outro: 200ms;
  --dur-medium: 500ms;
  --dur-medium-outro: 400ms;
  --dur-long: 1000ms;
  --dur-long-outro: 750ms; }
]} *)
module Dur : sig

  val notice : dur_ms
  (** [notice] is [100ms]. Below this duration, users won't notice. *)

  val short : dur_ms
  (** [short] is [250ms]. Most animations should simply use this timing. *)

  val short_outro : dur_ms
  (** [short_outro] is [200ms]. For short outroduction, fade outs, collapses. *)

  val medium : dur_ms
  (** [medium] is [500ms]. *)

  val medium_outro : dur_ms
  (** [medium] is [400ms]. *)

  val long : dur_ms
  (** [long] is [1000ms]. *)

  val long_outro : dur_ms
  (** [long_outro] is [750ms]. *)
end

(** {1:headers HTTP headers} *)

(** {2:requests Requests} *)

val htmlact : Webs.Http.Headers.Name.t
(** [htmlact] is the {{!page-manual.header_htmlact}[htmlact]} header. *)

(** {2:responses Responses} *)

val redirect : Webs.Http.Headers.Name.t
(** [redirect] is the {{!page-manual.header_redirect}[htmlact-redirect]}
    header. *)

val reload : Webs.Http.Headers.Name.t
(** [reload] is the {{!page-manual.header_reload}[htmlact-reload]} header. *)

val location_push : Webs.Http.Headers.Name.t
(** [location_push] is the {{!page-manual.header_location_push}
    [htmlact-location-push]} header. *)

val location_replace : Webs.Http.Headers.Name.t
(** [location_replace] is the
    {{!page-manual.header_location_replace}[htmlact-location-replace]}
    header. *)

val location_title : Webs.Http.Headers.Name.t
(** [location_title] is the
    {{!page-manual.header_location_title}[htmlact-location-title]} header. *)

val encode_location_title : string -> string
(** [encode_location_title s] encodes [s] for the {!location_title}
    headers. *)
