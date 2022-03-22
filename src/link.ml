(* Copyright (C) 2021 Nunuhara Cabbage <nunuhara@haniwa.technology>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://gnu.org/licenses/>.
 *)

open Core
open Alice.Ain

type reloc_t = {
  globals : (int, int) Hashtbl.t;
  functions : (int, int) Hashtbl.t;
  structs : (int, int) Hashtbl.t;
  functypes : (int, int) Hashtbl.t
}

let make_reloc_tables () =
  { globals = Hashtbl.create (module Int);
    functions = Hashtbl.create (module Int);
    structs = Hashtbl.create (module Int);
    functypes = Hashtbl.create (module Int)
  }

let check_hll_defs a b =
  let n = nr_libraries a in
  if not (phys_equal (nr_libraries b) n) then
    failwith "nr_libraries not equal";
  let check_lib i =
    if not (Library.equal (Library.of_int a i) (Library.of_int b i)) then
      failwith "library not equal"
  in
  List.iter (List.init n ~f:(~+)) ~f:check_lib

let link_global reloc ain (g:Variable.t) =
  let no =
    match Alice.Ain.get_global ain g.name with
    | Some existing_g ->
        (* ensure definitions match *)
        if not (Variable.equal g existing_g) then
          failwith "globals not equal"; (* TODO: exception *)
        existing_g.index
    | None ->
        (* add global to ain file *)
        write_new_global ain g
  in
  (* add relocation to table *)
  Hashtbl.add_exn reloc ~key:g.index ~data:no

let link_function reloc ain (f:Function.t) =
  let no =
    match Alice.Ain.get_function ain f.name with
    | Some existing_f ->
        (* ensure definitions match *)
        if not (Function.equal f existing_f) then
          failwith "functions not equal"; (* TODO: exception *)
        existing_f.index
    | None ->
        (* add function to ain file *)
        write_new_function ain f
  in
  (* add relocation to table *)
  Hashtbl.add_exn reloc ~key:f.index ~data:no

let rec link_type reloc ain (v:Type.t) =
  match v.data with
  | Struct no ->
      begin match Hashtbl.find reloc.structs no with
      | Some reloc_no -> {v with data=(Struct reloc_no)}
      | None -> failwith "struct type not found"
      end
  | FuncType no ->
      begin match Hashtbl.find reloc.functypes no with
      | Some reloc_no -> {v with data=(FuncType reloc_no)}
      | None -> failwith "functype not found"
      end
  | Delegate _ ->
      failwith "delegate linking not implemented"
  | Array t ->
      {v with data=(Array (link_type reloc ain t))}
  | Wrap t ->
      {v with data=(Wrap (link_type reloc ain t))}
  | Option t ->
      {v with data=(Option (link_type reloc ain t))}
  | Unknown87 t ->
      {v with data=(Unknown87 (link_type reloc ain t))}
  | Enum _ | Enum2 _ ->
      failwith "enum linking not implemented"
  | Void | Int | Float | String | IMainSystem | Bool | LongInt | HLLParam
  | IFace | HLLFunc | IFaceWrap | Function _ ->
      v

let link_variable_type reloc ain (v:Variable.t) =
  { v with value_type=link_type reloc ain v.value_type }

let link_struct reloc ain (s:Struct.t) =
  let no =
    match Alice.Ain.get_struct ain s.name with
    | Some existing_s ->
        (* ensure definitions match *)
        if not (Struct.equal s existing_s) then
          failwith "structs not equal"; (* TODO: exception *)
        existing_s.index
    | None ->
        (* add struct to ain file *)
        write_new_struct ain s
  in
  (* add relocation to table *)
  Hashtbl.add_exn reloc ~key:s.index ~data:no

let link_struct_types reloc ain (s:Struct.t) =
  let reloc_members = List.map s.members ~f:(link_variable_type reloc ain) in
  Struct.write ain {s with members=reloc_members}

let link_struct_ctor_dtor func_reloc (s:Struct.t) =
  if s.constructor >= 0 then
    begin match Hashtbl.find func_reloc s.constructor with
    | Some i -> s.constructor <- i
    | None -> failwith "oops"
    end;
  if s.destructor >= 0 then
    begin match Hashtbl.find func_reloc s.destructor with
    | Some i -> s.destructor <- i
    | None -> failwith "oops"
    end

let link_functype reloc ain (f:FunctionType.t) =
  let no =
    match Alice.Ain.get_functype ain f.name with
    | Some existing_f ->
        (* ensure definitions match *)
        if not (FunctionType.equal f existing_f) then
          failwith "functypes not equal"; (* TODO: exception *)
        existing_f.index
    | None ->
        (* add functype to ain file *)
        write_new_functype ain f
  in
  (* add relocation to table *)
  Hashtbl.add_exn reloc ~key:f.index ~data:no

let link_functype_types reloc ain (f:FunctionType.t) =
  let reloc_vars = List.map f.variables ~f:(link_variable_type reloc ain) in
  FunctionType.write ain {f with variables=reloc_vars}

let link_code reloc a b =
  let code_offset = code_size a in
  let buffer = Alice.CBuffer.create 2048 in
  let instruction_iter dasm =
    let map_argument t v =
      match Bytecode.argtype_of_int t with
      | Address -> v + code_offset
      | String ->
          begin match get_string b v with
          | Some str -> add_string a str
          | None -> failwith "invalid string index"
          end
      | Message ->
          begin match get_message b v with
          | Some msg -> add_message a msg
          | None -> failwith "invalid message index"
          end
      | Function ->
          begin match Hashtbl.find reloc.functions v with
          | Some fno -> fno
          | None -> failwith "invalid function index"
          end
      | Global ->
          begin match Hashtbl.find reloc.globals v with
          | Some gno -> gno
          | None -> failwith "invalid global index"
          end
      | Struct ->
          begin match Hashtbl.find reloc.structs v with
          | Some sno -> sno
          | None -> failwith "invalid struct index"
          end
      | File -> failwith "files not implemented"
      | Delegate -> failwith "delegates not implemented"
      | Switch -> failwith "switch not implemented"
      | Int | Float | Local | Syscall | Library | LibraryFunction -> v
    in
    (* map arguments from b-indices to a-indices via relocation tables *)
    let args = List.map2_exn (DASM.argument_types dasm) (DASM.arguments dasm) ~f:map_argument in
    (* write remapped instruction to output buffer *)
    Alice.CBuffer.write_int16 buffer (DASM.opcode dasm);
    List.iter args ~f:(fun v -> Alice.CBuffer.write_int32 buffer v)
  in
  Alice.Ain.foreach_instruction b ~f:instruction_iter;
  Alice.Ain.append_bytecode a buffer

let link a b =
  check_hll_defs a b;
  (* create hash tables mapping relocations of objects from b to a *)
  let reloc = make_reloc_tables () in
  (* get the starting point for added objects *)
  (* let global_start = nr_globals a in *)
  (* let function_start = nr_functions a in *)
  let struct_start = nr_structs a in
  let functype_start = nr_functypes a in
  (* transfer type objects from b to a *)
  Alice.Ain.struct_iter b ~f:(link_struct reloc.structs a);
  functype_iter b ~f:(link_functype reloc.functypes a);
  (* 2nd pass over type objects to update nested types *)
  struct_iter a ~f:(link_struct_types reloc a) ~from:struct_start;
  functype_iter b ~f:(link_functype_types reloc a) ~from:functype_start;
  (* transfer remaining objects from b to a *)
  global_iter b ~f:(link_global reloc.globals a);
  function_iter b ~f:(link_function reloc.functions a);
  (* 3rd pass to update constructor/destructor indices in struct objects *)
  struct_iter a ~f:(link_struct_ctor_dtor reloc.functions) ~from:struct_start;
  (* append code from b to a, updating indices per relocation tables *)
  if (Alice.Ain.code_size b) > 0 then
    link_code reloc a b

