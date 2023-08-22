(*---------------------------------------------------------------------------
   Copyright (c) 2021 The htmlact programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Webs
open Htmlit

(** The type for examples. *)
module type T = sig
  val id : string
  (** [id] is a unique identifier for the example. *)

  val name : string
  (** [name] is the human name of the example. *)

  val synopsis : string
  (** [synopsis] is a one line description for the example. *)

  val prefix : string
  (** [prefix] is the URL segment prefix of the example. *)

  val serve : Http.Request.t -> (Http.Response.t, Http.Response.t) result
  (** [serve request] implements the example. *)
end

(** {1:req Request deconstruction} *)

val path : Http.Request.t -> prefix:string -> Http.Path.t
(** [path request ~prefix] extracts from [request] the path requested
    for the example with URL prefix [prefix]. *)

(** {1:urlfmt URL formatting} *)

type urlf
(** The type for URL formatters. These abstract away the request service path
    and the example URL prefix {!T.prefix}. *)

val urlf : Http.Request.t -> prefix:string -> urlf
(** [urlf request ~prefix] is an URL formatter for responding to [request]
    for the example with URL prefix [prefix]. *)

val uf : urlf -> ('a, unit, string, string) format4 -> 'a
(** [uf urlf fmt …] formats an URL with [urlf] and format [fmt]. *)

(** {1:html_frags HTML fragments} *)

val button : ?at:At.t list -> string -> El.html
(** [button text] is a button with text [text]. *)

val description : El.html list -> El.html
(** [description html] is a div for [html] with class [description]. *)

val field : ?at:At.t list -> El.html list -> El.html
(** [field html] is a span with html [html] and class [field]. *)

val input_field :
  ?autocomplete:bool -> ?at:At.t list -> type':string -> name:string ->
  string -> El.html
(** [input_field value] is an input text field with value [value]. *)

val link : href:string -> string -> El.html
(** [link ~href text] hyperlinks [text] with [href]. *)

val submit : ?at:At.t list -> string -> El.html
(** [submit text] is a submit button with text [text]. *)

val table : ?headers:string list -> El.html list list -> El.html
(** [table rows] is a table from the given [rows] *)

(** {1:html_rendering HTML rendering} *)

val html : El.html list -> string
(** [html h] is HTML for [h] without a doctype.y *)

val html_page :
  ?style:string -> id:string -> title:string -> El.html list -> string
(** [html_page ~id ~title h] is an HTML page with title [title] and body
    [h] for example [id]. [style] is raw CSS added to the page. *)
