(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_html

module type T = sig
  val id : string
  val name : string
  val synopsis : string
  val prefix : string
  val serve : Req.t -> (Resp.t, Resp.t) result
end

val page : ?style:string -> id:string -> title:string -> Ht.part list -> string
val part : Ht.part list -> string

val table : ?headers:string list -> Ht.part list list -> Ht.part
val link : href:string -> string -> Ht.part
val description : Ht.part list -> Ht.part

val submit : ?at:At.t list -> string -> Ht.part
val button : ?at:At.t list -> string -> Ht.part
val input_field :
  ?autocomplete:bool -> ?at:At.t list -> type':string -> name:string ->
  string -> Ht.part

val field : ?at:At.t list -> Ht.part list -> Ht.part

type urlf
val urlf : Req.t -> urlf
val uf : urlf -> ('a, unit, string, string) format4 -> 'a


val req_decode : Req.t -> (Req.t -> 'a) -> ('a, Resp.t) result
val req_decode_query :
  Req.t -> (Req.t -> Http.Query.t -> 'a) -> ('a, Resp.t) result

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
