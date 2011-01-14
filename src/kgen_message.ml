(*
 * kgen_message.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type typ =
  | Sint8
  | Uint8
  | Sint16
  | Uint16
  | Sint32
  | Uint32

type t = {
  name : string;
  id : int;
  args : (string * typ) list;
}

let name msg = msg.name
let id msg = msg.id
let args msg = msg.args

exception Parse_error of string

(* +-----------------------------------------------------------------+
   | Checking                                                        |
   +-----------------------------------------------------------------+ *)

let parse_error fmt =
  Printf.ksprintf (fun str -> raise (Parse_error str)) fmt

let size_of_type = function
  | Sint8 -> 1
  | Uint8 -> 1
  | Sint16 -> 2
  | Uint16 -> 2
  | Sint32 -> 4
  | Uint32 -> 4

let check_uniq_name messages =
  let module Name_set = Set.Make(String) in
  let rec loop set = function
    | [] ->
        ()
    | msg :: rest ->
        if Name_set.mem msg.name set then
          parse_error "message %S is defined more than once.\n" msg.name
        else
          loop (Name_set.add msg.name set) rest
  in
  loop Name_set.empty messages

let check_uniq_id messages =
  let module Id_map = Map.Make(struct type t = int let compare x y = x - y end) in
  let rec loop map = function
    | [] ->
        ()
    | msg :: rest ->
        match try Some(Id_map.find msg.id map) with Not_found -> None with
          | Some name ->
              parse_error "messages %S and %S have the same id.\n" name msg.name
          | None ->
              loop (Id_map.add msg.id msg.name map) rest
  in
  loop Id_map.empty messages

let check_size msg =
  let size = List.fold_left (fun acc (name, typ) -> acc + size_of_type typ) 0 msg.args in
  if size > 6 then
    parse_error "size exceeded for message %S: %d, must be < 6.\n" msg.name size

let check messages =
  check_uniq_name messages;
  check_uniq_id messages;
  List.iter check_size messages

(* +-----------------------------------------------------------------+
   | Parsing                                                         |
   +-----------------------------------------------------------------+ *)

open Camlp4.PreCast
open Syntax

let messages = Gram.Entry.mk "messages"

EXTEND Gram
  GLOBAL: messages;

  messages:
    [ [ l = LIST0 message; EOI -> l ] ];

  message:
    [ [ "message"; name = LIDENT; "="; id = INT; "{"; args = LIST1 argument; "}" ->
          { name; id = int_of_string id; args }
      ] ];

  argument:
    [ [ name = LIDENT; ":"; typ = integer_type; ";" -> (name, typ) ] ];

  integer_type:
    [ [ str = LIDENT ->
          match str with
            | "int8" | "sint8" -> Sint8
            | "uint8" -> Uint8
            | "int16" | "sint16" -> Sint16
            | "uint16" -> Uint16
            | "int32" | "sint32" -> Sint32
            | "uint32" -> Uint32
            | _ -> Loc.raise _loc (Failure "invalid integer type")
      ] ];
END

let parse file_name =
  let ic = open_in file_name in
  let messages =
    try
      let messages = Gram.parse messages (Loc.mk file_name) (Stream.of_channel ic) in
      close_in ic;
      messages
    with exn ->
      close_in ic;
      raise (Parse_error(Camlp4.ErrorHandler.to_string exn))
  in
  check messages;
  messages
