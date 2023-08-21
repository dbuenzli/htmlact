(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(** A bookmark database. *)

type t =
  { id : int;
    name : string;
    link : string;
    description : string;
    broken : bool; }

val all : unit -> t list
val get : int -> t option
val set : t -> unit
val delete : t -> unit
val restore_deleted : unit -> unit
