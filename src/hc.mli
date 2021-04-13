(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Hc HTML generation using {!Ht}.

    These function help generate {{:page-manual.attref}Hc data attributes}
    with {!Ht}. Ignore if you are using something else to generate
    your HTML. *)

open Webs
open Webs_html

(** {1:dur Durations} *)

type dur_ms = int

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

(** {1:atts Attributes} *)

val request : ?meth:[< Http.meth | `Sse] -> string -> At.t
val request_path : ?meth:[< Http.meth | `Sse] -> Http.path -> At.t
val target : string -> At.t
val query : string -> At.t

type effect_kind =
[ `Inner | `Inplace | `Beforebegin | `Afterbegin | `Beforeend
| `Afterend | `None | `Event of string ]

val effect : effect_kind -> At.t


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
