(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

module type T = sig
  val id : string
  val name : string
  val synopsis : string
  val prefix : string
  val serve : Http.Request.t -> (Http.Response.t, Http.Response.t) result
end

val page : ?style:string -> id:string -> title:string -> El.html list -> string
val part : El.html list -> string

val table : ?headers:string list -> El.html list list -> El.html
val link : href:string -> string -> El.html
val description : El.html list -> El.html

val submit : ?at:At.t list -> string -> El.html
val button : ?at:At.t list -> string -> El.html
val input_field :
  ?autocomplete:bool -> ?at:At.t list -> type':string -> name:string ->
  string -> El.html

val field : ?at:At.t list -> El.html list -> El.html

type urlf
val urlf : Http.Request.t -> urlf
val uf : urlf -> ('a, unit, string, string) format4 -> 'a

val starts_with : prefix:string -> string -> bool
