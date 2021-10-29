(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Hc HTML generation and HTTP headers for {!Webs}.

    These function help generate Hc {{!page-manual.attref}data
    attributes} and {{!page-manual.responses}HTTP headers} with
    {!Webs} and {!Webs_html}. Ignore if you are using something else
    to generate your HTML. *)

open Webs
open Webs_html

(** {1:atts Attributes} *)

val request : ?meth:[< Http.meth | `Sse] -> string -> At.t
(** [request ~meth r] is a {{!page-manual.data_request}[data-request]}
    attribute for URL [r] using method [meth] (defaults to [`GET]). *)

val request_path : ?meth:[< Http.meth | `Sse] -> Http.path -> At.t
(** [request ~meth p] is a {{!page-manual.data_request}[data-request]}
    attribute for URL [p] using method [meth] (defaults to [`GET]). *)

val query : string -> At.t
(** [query sel] is the {{!page-manual.data_target}[data-query]} attribute
    [sel]. *)

val query_rescue : bool -> At.t
(** [query_rescue b]is the {{!page-manual.query_rescue}[data-query-rescue]}
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

val hc : Webs.Http.name
(** [hc] is the {{!page-manual.header_hc}[hc]} header. *)

(** {2:responses Responses} *)

val redirect : Webs.Http.name
(** [redirect] is the {{!page-manual.header_redirect}[hc-redirect]}
    header. *)

val reload : Webs.Http.name
(** [reload] is the {{!page-manual.header_reload}[hc-reload]} header. *)

val location_push : Webs.Http.name
(** [location_push] is the {{!page-manual.header_location_push}
    [hc-location-push]} header. *)

val location_replace : Webs.Http.name
(** [location_replace] is the
    {{!page-manual.header_location_replace}[hc-location-replace]}
    header. *)

val location_title : Webs.Http.name
(** [location_title] is the
    {{!page-manual.header_location_title}[hc-location-title]} header. *)

val encode_location_title : string -> string
(** [encode_locationt_title s] encodes [s] for the {!location_title}
    headers. *)

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
