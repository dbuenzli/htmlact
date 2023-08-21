(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [htmlact] webpage driver. *)

(** {1:install Installing} *)

val init : unit -> unit
(** [init ()] installs [htmlact] on the page. *)

(** {1:events Events} *)

(** Htmlact connection cycle events.

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

  val htmlact_in : unit Brr.Ev.type'
  (** [hthmlact_in] is sent on the element that gets classifed by
      {{!page-manual.class_htmlact_in}[htmlact-in-parent]}, just before
      they get classified. This is the parent element of what gets
      newly inserted in the DOM. {!Brr.Ev.prevent_default} is called on
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
