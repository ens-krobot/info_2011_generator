(*
 * kgen_message.mli
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Message description. *)

(** Type of arguments of messages. *)
type typ =
  | Sint8
  | Uint8
  | Sint16
  | Uint16
  | Sint32
  | Uint32

(** Description of a message. *)
type t = {
  name : string;
  (** The name of the message. *)

  id : int;
  (** The numeric identifier of the message. *)

  args : (string * typ) list;
  (** The arguments of the message, each argument is a pair of the
      argument name with its type. *)
}

(** {6 Projections} *)

val name : t -> string
val id : t -> int
val args : t -> (string * typ) list

(** {6 Informations} *)

val size_of_type : typ -> int
  (** Returns the size in bytes of values of the given type. *)

(** {6 Parsing} *)

exception Parse_error of string
  (** Excpetion raised when a source file cannot be parsed. *)

val parse : string -> t list
  (** [parse file] parses [file] and returns the list of messages it
      contains. *)
