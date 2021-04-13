(*---------------------------------------------------------------------------
   Copyright (c) 2021 The hc programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Webs
open Webs_html

let strf = Printf.sprintf

type dur_ms = int
module Dur = struct
  let notice = 100
  let short = 250
  let short_outro = 200
  let medium = 500
  let medium_outro = 400
  let long = 1000
  let long_outro = 750
end

let request ?meth url =
  let req = match meth with
  | None -> url
  | Some `Sse -> strf "SSE %s" url
  | Some (#Http.meth as m) -> strf "%s %s" (Http.Meth.encode m) url
  in
  At.v "data-request" req

let request_path ?meth p = request ?meth (Http.Path.encode p)
let target v = At.v "data-target" v
let query v = At.v "data-query" v

type effect_kind =
[ `Inner | `Inplace | `Beforebegin | `Afterbegin | `Beforeend
| `Afterend | `None | `Event of string ]

let effect_kind_to_string = function
| `Inner -> "inner" | `Inplace -> "inplace"
| `Beforebegin -> "beforebegin" | `Afterbegin -> "afterbegin"
| `Beforeend -> "beforeend" | `Afterend -> "afterend"
| `None -> "none" | `Event ev -> "event " ^ ev

let effect k = At.v "data-effect" (effect_kind_to_string k)

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
