(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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

val req_decode :
  Http.Request.t -> (Http.Request.t -> 'a) -> ('a, Http.Response.t) result

val req_decode_query :
  Http.Request.t -> (Http.Request.t -> Http.Query.t -> 'a) ->
  ('a, Http.Response.t) result

val starts_with : prefix:string -> string -> bool

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers

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
