(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Hc web page driver. *)

(** {1:install Installing} *)

val init : unit -> unit
(** [init ()] installs [hc] on the page. *)

(** {1:events Events} *)

(** Hc connection cycle events.

    {b FIXME.} Expand on that. Also end with error attribute
    would likely be nicer. Ensures start/end comes in pair. *)
module Ev : sig

  val cycle_start : unit Brr.Ev.type'
  (** [cycle_start] is sent on {!G.document} when a connection cycle start. *)

  val cycle_end : unit Brr.Ev.type'
  (** [cycle_end] is sent on {!G.document} when a connection cycle ends. *)

  val cycle_error : unit Brr.Ev.type'
  (** [cycle_end] is sent on {!G.document} when a connection cycle errors. *)
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
