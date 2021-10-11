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

    {b FIXME.} Document in the manual.

    {b FIXME.} Expand on that. Also end with error attribute
    would likely be nicer. Ensures start/end comes in pair. *)
module Ev : sig

  val cycle_start : unit Brr.Ev.type'
  (** [cycle_start] is sent on {!G.document} when a connection cycle start. *)

  val cycle_end : unit Brr.Ev.type'
  (** [cycle_end] is sent on {!G.document} when a connection cycle ends. *)

  val cycle_error : unit Brr.Ev.type'
  (** [cycle_end] is sent on {!G.document} when a connection cycle errors. *)

  val hc_in : unit Brr.Ev.type'
  (** [hc_in] is sent on the element that gets classifed by
      {{!page-manual.class_hc_in_parent}[hc-in-parent]}, just before they
      get classified. This is the parent element of what gets
      newly inserted in the DOM. {!Ev.prevent_default} is called on
      the event, the classification dance does not occur.

      {b FIXME.} Wouldn't it be better to expose MutationObserver
      hooks ?  *)
end

(**/**)

module Effect : sig
  type kind = Element | Children | Insert of Jstr.t | None' | Event of Jstr.t
  val feedback_remove : target:Brr.El.t -> kind -> unit Fut.t
end
(**/**)

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
