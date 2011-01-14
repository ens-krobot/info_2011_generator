(*
 * krobot_proto2c.ml
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Printf

(* +-----------------------------------------------------------------+
   | Helpers                                                         |
   +-----------------------------------------------------------------+ *)

(* Return the name of the C type associated to a protocol type. *)
let c_type_name = function
  | Kgen_message.Sint8 -> "int8_t"
  | Kgen_message.Uint8 -> "uint8_t"
  | Kgen_message.Sint16 -> "int16_t"
  | Kgen_message.Uint16 -> "uint16_t"
  | Kgen_message.Sint32 -> "int32_t"
  | Kgen_message.Uint32 -> "uint32_t"

(* Return the expression for reading a value of type [typ] at offset
   [ofs]. *)
let reader typ ofs =
  match typ with
    | Kgen_message.Sint8 ->
        sprintf "((char*)msg)[%d]" ofs
    | Kgen_message.Uint8 ->
        sprintf "msg[%d]" ofs
    | Kgen_message.Sint16 ->
        sprintf "msg[%d] | (((char*)msg)[%d] << 8)" ofs (ofs + 1)
    | Kgen_message.Uint16 ->
        sprintf "msg[%d] | (msg[%d] << 8)" ofs (ofs + 1)
    | Kgen_message.Sint32 ->
        sprintf "msg[%d] | (msg[%d] << 8) | (msg[%d] << 16) | (((char*)msg)[%d] << 24)" ofs (ofs + 1) (ofs + 2) (ofs + 3)
    | Kgen_message.Uint32 ->
        sprintf "msg[%d] | (msg[%d] << 8) | (msg[%d] << 16) | (msg [%d] << 24)" ofs (ofs + 1) (ofs + 2) (ofs + 3)

(* +-----------------------------------------------------------------+
   | Code generation                                                 |
   +-----------------------------------------------------------------+ *)

(* Generate the C interface file for the dispatcher. *)
let gen_dispatcher_intf file_name messages =
  let oc = open_out file_name in
  fprintf oc "void krobot_dispatch(unsigned char *msg);\n";
  close_out oc

(* Generate the C implementation file for the dispatcher. *)
let gen_dispatcher_impl file_name messages =
  let oc = open_out file_name in

  fprintf oc "#include <stdint.h>\n\n";

  (* Generate prototypes. *)
  List.iter
    (fun msg ->
       fprintf oc "void krobot_%s(%s);\n"
         (Kgen_message.name msg)
         (String.concat ", "
            (List.map
               (fun (name, typ) -> sprintf "%s %s" (c_type_name typ) name)
               (Kgen_message.args msg))))
    messages;

  (* Generate the dispatch function. *)
  fprintf oc "\nvoid krobot_dispatch(unsigned char *msg)\n{\n  switch (msg[0] | (msg[1] << 8)) {\n";
  List.iter
    (fun msg ->
       fprintf oc "  case %d:\n" (Kgen_message.id msg);
       match Kgen_message.args msg with
         | [] ->
             fprintf oc "    %s();\n    return;\n" (Kgen_message.name msg)
         | (name, typ) :: args ->
             let indent = String.make (String.length (Kgen_message.name msg) + 12) ' ' in
             fprintf oc "    krobot_%s(%s" (Kgen_message.name msg) (reader typ 2);
             ignore
               (List.fold_left
                  (fun ofs (name, typ) ->
                     fprintf oc ",\n%s%s" indent (reader typ ofs);
                     ofs + Kgen_message.size_of_type typ)
                  (2 + Kgen_message.size_of_type typ)
                  args);
             fprintf oc ");\n    return;\n")
    messages;
  fprintf oc "  }\n}\n";

  close_out oc

(* Generate the C interface file for message emitters. *)
let gen_emitters_intf file_name messages =
  let oc = open_out file_name in

  fprintf oc "#include <stdint.h>\n\n";

  List.iter
    (fun msg ->
       fprintf oc "void krobot_emit_%s(%s);\n"
         (Kgen_message.name msg)
         (String.concat ", "
            (List.map
               (fun (name, typ) -> sprintf "%s %s" (c_type_name typ) name)
               (Kgen_message.args msg))))
    messages;

  close_out oc

(* Generate the C implementation file for message emitters. *)
let gen_emitters_impl file_name messages =
  let oc = open_out file_name in

  fprintf oc "#include <stdint.h>\n";

  List.iter
    (fun msg ->
       fprintf oc "\nvoid krobot_emit_%s(%s)\n{\n"
         (Kgen_message.name msg)
         (String.concat ", "
            (List.map
               (fun (name, typ) -> sprintf "%s %s" (c_type_name typ) name)
               (Kgen_message.args msg)));
       fprintf oc "  unsigned char msg[8];\n";
       fprintf oc "  msg[0] = %d;\n" (Kgen_message.id msg land 0xff);
       fprintf oc "  msg[1] = %d;\n" ((Kgen_message.id msg lsr 8) land 0xff);
       let ofs =
         List.fold_left
           (fun ofs (name, typ) ->
              begin
                match typ with
                  | Kgen_message.Sint8
                  | Kgen_message.Uint8 ->
                      fprintf oc "  msg[%d] = %s;\n" ofs name
                  | Kgen_message.Sint16
                  | Kgen_message.Uint16 ->
                      fprintf oc "  msg[%d] = %s;\n" ofs name;
                      fprintf oc "  msg[%d] = %s >> 8;\n" (ofs + 1) name
                  | Kgen_message.Sint32
                  | Kgen_message.Uint32 ->
                      fprintf oc "  msg[%d] = %s;\n" ofs name;
                      fprintf oc "  msg[%d] = %s >> 8;\n" (ofs + 1) name;
                      fprintf oc "  msg[%d] = %s >> 16;\n" (ofs + 1) name;
                      fprintf oc "  msg[%d] = %s >> 24;\n" (ofs + 1) name
              end;
              ofs + Kgen_message.size_of_type typ)
           2 (Kgen_message.args msg)
       in
       for ofs = ofs + 1 to 7 do
         fprintf oc "  msg[%d] = 0;\n" ofs
       done;
       fprintf oc "  can_send(msg);\n";
       fprintf oc "}\n")
    messages;

  close_out oc

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "usage: krobot-proto2c <file.krobot>";
    exit 2;
  end;

  let messages = Kgen_message.parse Sys.argv.(1) in

  gen_dispatcher_intf "krobot-dispatcher.h" messages;
  gen_dispatcher_impl "krobot-dispatcher.c" messages;
  gen_emitters_intf "krobot-emitters.h" messages;
  gen_emitters_impl "krobot-emitters.c" messages
