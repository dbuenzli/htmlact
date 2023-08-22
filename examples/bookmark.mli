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
(** The type for bookmarks. *)

val all : unit -> t list
(** [all ()] are all the bookmarks in the database. *)

val search_name : prefix:string -> t list
(** [search_name ~prefix] are all the bookmarks whose lowercase
    name is prefixed by [prefix]. *)

val find : id:int -> t option
(** [find ~id] is the bookmark identified by [id] (if any). *)

val get : id:int -> t
(** [get ~id] is the bookmark identified by [id]. Raises [Invalid_argument]
    if there is no such bookmark. *)

val set : t -> unit
(** [set b] sets bookmark [b] in the database. *)

val delete : t -> unit
(** [detele b] deletes bookmark [b] from the database. *)

val restore_deleted : unit -> unit
(** [restore_deleted ()] restores the deleted bookmarks from the database. *)
