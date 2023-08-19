(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type t =
  { id : int;
    name : string;
    link : string;
    description : string;
    broken : bool; }

let trash = ref []
let table = ref []
let all () = List.map snd !table
let get id = List.assoc_opt id !table
let set b =
  table := List.sort compare ((b.id, b) :: (List.remove_assoc b.id !table))

let delete b =
  table := List.remove_assoc b.id ! table;
  trash := (b.id, b) :: !trash

let restore_deleted () = List.iter (fun (_, b) -> set b) !trash; trash := []

let init () =
  set { id = 1; name = "OCaml"; link = "https://ocaml.org";
        description = "The OCaml website"; broken = false; };
  set { id = 2; name = "Haskell"; link = "https://haskell.org";
        description = "The Haskell language website"; broken = false; };
  set { id = 3; name = "Wikipedia"; link = "https://wikipedia.org";
        description ="The Free Encyclopedia"; broken = false; };
  set { id = 4; name = "Old Caml site"; link = "https://caml.inria.fr";
        description = "The old OCaml website"; broken = false; };
  set { id = 5; name = "caml-list"; link = "https://inbox.ocaml.org/";
        description = "The OCaml mailing list"; broken = false; };
  set { id = 6; name = "OCaml Weekly news";
        link = "http://alan.petitepomme.net/cwn/";
        description = "The OCaml weekly news"; broken = false; };
  set { id = 7; name = "MDN Web Docs"; link = "https://developer.mozilla.org/";
        description = "Web dev docs"; broken = false };
  ()

let () = init ()

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
