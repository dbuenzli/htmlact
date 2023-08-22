(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
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
let search_name ~prefix =
  let prefix = String.lowercase_ascii prefix in
  let lowercase_name b = String.lowercase_ascii b.name in
  let has_prefix b = String.starts_with ~prefix (lowercase_name b) in
  List.filter has_prefix (all ())

let find ~id = List.assoc_opt id !table
let get ~id = Option.get (find ~id)
let set b =
  table := List.sort compare ((b.id, b) :: (List.remove_assoc b.id !table))

let delete b =
  table := List.remove_assoc b.id !table;
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
