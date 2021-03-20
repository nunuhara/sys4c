(* XXX: Work around braindamage in dune/ctypes (force linking of ain.o) *)
external strangle_linker : unit -> unit = "ain_new"
external strangle_dune : unit -> unit = "_ain_global"

open Ctypes
open Foreign
open Printf

type ain = unit ptr
let ain : ain typ = ptr void

module Type = struct
  type t_c
  let t_c : t_c structure typ = structure "ain_type"
  let data = field t_c "data" int
  let struc = field t_c "struc" int32_t
  let rank = field t_c "rank" int32_t
  let array_type = field t_c "array_type" (ptr t_c)
  let () = seal t_c

  type data =
    | Void
    | Int
    | Float
    | String
    | Struct of int
    | IMainSystem
    | FuncType of int
    | Bool
    | LongInt
    | Delegate of int
    | HLLParam
    | Array of t
    | Wrap of t
    | Option of t
    | Unknown87 of t
    | IFace
    | Enum2 of int
    | Enum of int
    | HLLFunc
    | IFaceWrap
  and t = {
    data : data;
    is_ref : bool;
  }

  let rec data_to_string = function
    | Void -> "void"
    | Int -> "int"
    | Float -> "float"
    | String -> "string"
    | Struct (no) -> sprintf "struct<%d>" no (* FIXME: look up name in ain object *)
    | IMainSystem -> "IMainSystem"
    | FuncType (no) -> sprintf "functype<%d>" no (* FIXME *)
    | Bool -> "bool"
    | LongInt -> "lint"
    | Delegate (no) -> sprintf "delegate<%d>" no (* FIXME *)
    | HLLParam -> "hll_param"
    | Array (t) -> sprintf "array<%s>" (to_string t)
    | Wrap (t) -> sprintf "wrap<%s>" (to_string t)
    | Option (t) -> sprintf "option<%s>" (to_string t)
    | Unknown87 (t) -> sprintf "unknown87<%s>" (to_string t)
    | IFace -> "interface"
    | Enum2 (no) -> sprintf "enum2<%d>" no (* FIXME *)
    | Enum (no) -> sprintf "enum<%d>" no (* FIXME *)
    | HLLFunc -> "hll_func"
    | IFaceWrap -> "interface_wrap"
  and to_string o =
    let prefix = if o.is_ref then "ref " else "" in
    prefix ^ (data_to_string o.data)

  (* Convert a t to a t_c.data value *)
  (* FIXME: need to take version into account... *)
  let to_int o =
    if o.is_ref then
      match o.data with
      | Void -> failwith "tried to create ref void"
      | Int -> 18
      | Float -> 19
      | String -> 20
      | Struct (_) -> 21
      | IMainSystem -> failwith "tried to create ref IMainSystem"
      | FuncType (_) -> 31
      | Bool -> 51
      | LongInt -> 59
      | Delegate (_) -> 67
      | HLLParam -> 75
      | Array (arrtype) ->
          begin match arrtype.data with
          | Void -> failwith "tried to create ref array<void>"
          | Int -> 22
          | Float -> 23
          | String -> 24
          | Struct (_) -> 25
          | IMainSystem -> failwith "tried to create ref array<IMainSystem>"
          | FuncType (_) -> 32
          | Bool -> 52
          | LongInt -> 60
          | Delegate (_) -> 69
          | HLLParam -> failwith "tried to create ref array<hll_param>"
          | Array (_) -> failwith "tried to create ref array<array<...>>"
          | Wrap (_) -> failwith "tried to create ref array<wrap<...>>"
          | Option (_) -> failwith "tried to create ref array<option<...>>"
          | Unknown87 (_) -> failwith "tried to create ref array<unknown_87>"
          | IFace -> failwith "tried to create ref array<interface>"
          | Enum2 (_) -> failwith "tried to create ref array<enum2>"
          | Enum (_) -> failwith "tried to create ref array<enum>"
          | HLLFunc -> failwith "tried to create ref array<hll_func>"
          | IFaceWrap -> failwith "tried to create ref array<iface_wrap<...>>"
          end
      | Wrap (_) -> failwith "tried to create ref wrap<...>"
      | Option (_) -> failwith "tried to create ref option<...>"
      | Unknown87 (_) -> failwith "tried to create ref unknown_87"
      | IFace -> failwith "tried to create ref interface"
      | Enum2 (_) -> failwith "tried to create ref enum2"
      | Enum (_) -> failwith "tried to create ref enum"
      | HLLFunc -> failwith "tried to create ref hll_func"
      | IFaceWrap -> failwith "tried to create ref iface_wrap<...>"
    else
      match o.data with
        | Void -> 0
        | Int -> 10
        | Float -> 11
        | String -> 12
        | Struct (_) -> 13
        | IMainSystem -> 26
        | FuncType (_) -> 27
        | Bool -> 47
        | LongInt -> 55
        | Delegate (_) -> 63
        | HLLParam -> 74
        | Array (arrtype) ->
            begin match arrtype.data with
            | Void -> failwith "tried to create array<void>"
            | Int -> 14
            | Float -> 15
            | String -> 16
            | Struct (_) -> 17
            | IMainSystem -> failwith "tried to create array<IMainSystem>"
            | FuncType (_) -> 30
            | Bool -> 50
            | LongInt -> 58
            | Delegate (_) -> 66
            | HLLParam -> failwith "tried to create array<hll_param>"
            | Array (_) -> failwith "tried to create array<array<...>>"
            | Wrap (_) -> failwith "tried to create array<wrap<...>>"
            | Option (_) -> failwith "tried to create array<option<...>>"
            | Unknown87 (_) -> failwith "tried to create array<unknown_87>"
            | IFace -> failwith "tried to create array<interface>"
            | Enum2 (_) -> failwith "tried to create array<enum2>"
            | Enum (_) -> failwith "tried to create array<enum>"
            | HLLFunc -> failwith "tried to create array<hll_func>"
            | IFaceWrap -> failwith "tried to create array<iface_wrap<...>>"
            end
        | Wrap (_) -> 82
        | Option (_) -> 86
        | Unknown87 (_) -> 87
        | IFace -> 89
        | Enum2 (_) -> 91
        | Enum (_) -> 92
        | HLLFunc -> 95
        | IFaceWrap -> 100

  let make ?(is_ref=false) data =
    (* TODO: sanity check rank here? *)
    { data; is_ref }

  let rec of_ptr p =
    let struc = Signed.Int32.to_int (getf (!@ p) struc) in
    let rank = Signed.Int32.to_int (getf (!@ p) rank) in
    (* t constructor for (old) array types *)
    let rec make_array ?(is_ref=false) data rank =
      if rank == 0 then
        make data
      else
        make (Array (make_array data (rank - 1))) ~is_ref
    in
    match (getf (!@ p) data) with
    | 0 -> make Void
    | 10 -> make Int
    | 11 -> make Float
    | 12 -> make String
    | 13 -> make (Struct struc)
    | 14 -> make_array Int rank
    | 15 -> make_array Float rank
    | 16 -> make_array String rank
    | 17 -> make_array (Struct struc) rank
    | 18 -> make Int ~is_ref:true
    | 19 -> make Float ~is_ref:true
    | 20 -> make String ~is_ref:true
    | 21 -> make (Struct struc) ~is_ref:true
    | 22 -> make_array Int rank ~is_ref:true
    | 23 -> make_array Float rank ~is_ref:true
    | 24 -> make_array String rank ~is_ref:true
    | 25 -> make_array (Struct struc) rank ~is_ref:true
    | 26 -> make IMainSystem
    | 27 -> make (FuncType struc)
    | 30 -> make_array (FuncType struc) rank
    | 31 -> make (FuncType struc) ~is_ref:true
    | 32 -> make_array (FuncType struc) rank ~is_ref:true
    | 47 -> make Bool
    | 50 -> make_array Bool rank
    | 51 -> make Bool ~is_ref:true
    | 52 -> make_array Bool rank ~is_ref:true
    | 55 -> make LongInt
    | 58 -> make_array LongInt rank
    | 59 -> make LongInt ~is_ref:true
    | 60 -> make_array LongInt rank ~is_ref:true
    | 63 -> make (Delegate struc)
    | 66 -> make_array (Delegate struc) rank
    | 67 -> make (Delegate struc) ~is_ref:true
    | 69 -> make_array (Delegate struc) rank ~is_ref:true
    | 74 -> make HLLParam
    | 75 -> make HLLParam ~is_ref:true
    | 79 -> make (Array (of_ptr (getf (!@ p) array_type)))
    | 80 -> make (Array (of_ptr (getf (!@ p) array_type)))
    | 82 -> make (Wrap (of_ptr (getf (!@ p) array_type)))
    | 86 -> make (Option (of_ptr (getf (!@ p) array_type)))
    | 87 -> make (Unknown87 (of_ptr (getf (!@ p) array_type)))
    | 89 -> make IFace
    | 91 -> make (Enum2 struc)
    | 92 -> make (Enum struc)
    | 93 -> make (Enum struc) ~is_ref:true
    | 95 -> make HLLFunc
    | 100 -> make IFaceWrap
    | _ -> failwith "Invalid or unknown data type in ain file"
end

module Variable = struct
  type initval_c
  let initval_c : initval_c union typ = union "initval_data"
  let initval_s = field initval_c "s" string
  let initval_i = field initval_c "i" int32_t
  let initval_f = field initval_c "f" float
  let () = seal initval_c

  type initval =
    | Int of int
    | Float of float
    | String of string

  type t_c
  let t_c : t_c structure typ = structure "ain_variable"
  let name = field t_c "name" string
  let name2 = field t_c "name2" string
  let value_type = field t_c "type" Type.t_c
  let has_initval = field t_c "has_initval" int32_t
  let initval = field t_c "initval" initval_c
  let group_index = field t_c "group_index" int32_t
  let var_type = field t_c "var_type" int
  let () = seal t_c

  type t = {
    index : int;
    name : string;
    name2 : string;
    value_type : Type.t;
    initval : initval option;
    group_index : int;
    var_type : int
  }

  let of_ptr p i =
    { index = i;
      name = getf (!@ p) name;
      name2 = getf (!@ p) name2;
      value_type = Type.of_ptr (addr (getf (!@ p) value_type));
      initval = None; (* FIXME *)
      group_index = Signed.Int32.to_int (getf (!@ p) group_index);
      var_type = getf (!@ p) var_type
    }

end

module Function = struct
  type t_c
  let t_c : t_c structure typ = structure "ain_function"
  let address = field t_c "address" uint32_t
  let name = field t_c "name" string
  let is_label = field t_c "is_label" bool
  let return_type = field t_c "return_type" Type.t_c
  let nr_args = field t_c "nr_args" int32_t
  let nr_vars = field t_c "nr_vars" int32_t
  let is_lambda = field t_c "is_lambda" int32_t
  let crc = field t_c "crc" int32_t
  let struct_type = field t_c "struct_type" int32_t
  let enum_type = field t_c "enum_type" int32_t
  let vars = field t_c "vars" (ptr Variable.t_c)
  let () = seal t_c

  type t = {
    index : int;
    address : int;
    name : string;
    nr_args : int;
    vars : Variable.t list;
    return_type : Type.t;
    is_label : bool;
    is_lambda : int;
    crc : int;
    struct_type : int;
    enum_type : int
  }
end

module Initval = struct
  type initval_c
  let initval_c : initval_c union typ = union "initval_data"
  let initval_s = field initval_c "string_value" string
  let initval_i = field initval_c "int_value" int32_t
  let initval_f = field initval_c "float_value" float
  let () = seal initval_c

  type t_c
  let t_c : t_c structure typ = structure "ain_initval"
  let global_index = field t_c "global_index" int32_t
  let data_type = field t_c "data_type" int32_t
  let value = field t_c "val" initval_c
  let () = seal t_c

  type initval =
    | Int of int
    | Float of float
    | String of string

  type t = {
    index : int;
    global_index : int;
    value : initval
  }
end

module Struct = struct
  type interface_c
  let interface_c : interface_c structure typ = structure "ain_interface"
  let struct_type = field interface_c "struct_type" int32_t
  let unknown = field interface_c "uk" int32_t
  let () = seal interface_c

  type t_c
  let t_c : t_c structure typ = structure "ain_struct"
  let name = field t_c "name" string
  let nr_interfaces = field t_c "nr_interfaces" int32_t
  let interfaces = field t_c "interfaces" (ptr interface_c)
  let constructor = field t_c "constructor" int32_t
  let destructor = field t_c "destructor" int32_t
  let nr_members = field t_c "nr_members" int32_t
  let members = field t_c "members" (ptr Variable.t_c)
  let nr_vmethods = field t_c "nr_vmethods" int32_t
  let vmethods = field t_c "vmethods" (ptr int32_t)
  let () = seal t_c

  type interface = {
    struct_type : int;
    unknown : int
  }

  type t = {
    index : int;
    name : string;
    interfaces : interface list;
    constructor : int;
    destructor : int;
    members : Variable.t list;
    vmethods : int list
  }
end

module Library = struct
  module Argument = struct
    type t
    let t : t structure typ = structure "ain_hll_argument"
    let name = field t "name" string
    let value_type = field t "type" Type.t_c
    let () = seal t
  end
  module Function = struct
    type t
    let t : t structure typ = structure "ain_hll_function"
    let name = field t "name" string
    let return_type = field t "return_type" Type.t_c
    let nr_arguments = field t "nr_arguments" int32_t
    let arguments = field t "arguments" (ptr Argument.t)
    let () = seal t
  end

  type t_c
  let t_c : t_c structure typ = structure "ain_library"
  let name = field t_c "name" string
  let nr_functions = field t_c "nr_functions" int32_t
  let functions = field t_c "functions" (ptr Function.t)
  let () = seal t_c

  type hll_argument = {
    name : string;
    value_type : Type.t
  }

  type hll_function = {
    name : string;
    return_type : Type.t;
    arguments : hll_argument list
  }

  type t = {
    index : int;
    name : string;
    functions : hll_function list
  }
end

module Switch = struct
  type t_c
  type t_case
  let t_c : t_c structure typ = structure "ain_switch"
  let t_case : t_case structure typ = structure "ain_switch_case"

  let case_value = field t_case "value" int32_t
  let case_address = field t_case "address" int32_t
  let case_parent = field t_case "parent" (ptr t_c)
  let () = seal t_case

  let case_type = field t_c "case_type" int
  let default_address = field t_c "default_address" int32_t
  let nr_cases = field t_c "nr_cases" int32_t
  let cases = field t_c "cases" (ptr t_case)
  let () = seal t_c

  type t = {
    index: int;
    case_type : int;
    default_address : int;
    cases : (int * int) list
  }
end

module FunctionType = struct
  type t_c
  let t_c : t_c structure typ = structure "ain_function_type"
  let name = field t_c "name" string
  let return_type = field t_c "return_type" Type.t_c
  let nr_arguments = field t_c "nr_arguments" int32_t
  let nr_variables = field t_c "nr_variables" int32_t
  let variables = field t_c "variables" (ptr Variable.t_c)
  let () = seal t_c

  type t = {
    index : int;
    name : string;
    return_type : Type.t;
    nr_arguments : int;
    variables : Variable.t list
  }
end

module Enum = struct
  type t_c
  let t_c : t_c structure typ = structure "ain_enum"
  let name = field t_c "name" string
  let nr_symbols = field t_c "nr_symbols" int
  let symbols = field t_c "symbols" (ptr string)
  let () = seal t_c

  type t = {
    index : int;
    name : string;
    symbols : string list
  }
end

exception File_error
exception Unrecognized_format
exception Invalid_format

let load name =
  let ain_open = foreign "ain_open" (string @-> ptr int @-> returning ain) in
  let err = allocate int 0 in
  let ainp = ain_open name err in
  begin match !@err with
  | 0 -> ()
  | 1 -> raise File_error
  | 2 -> raise Unrecognized_format
  | 3 -> raise Invalid_format
  | _ -> failwith "Unrecognized error code from ain_open"
  end;
  ainp

let create = foreign "ain_new" (int @-> int @-> returning ain)
let free = foreign "ain_free" (ain @-> returning void)
let get_function' = foreign "ain_get_function" (ain @-> string @-> returning int)
let get_global' = foreign "ain_get_global" (ain @-> string @-> returning int)
let get_struct' = foreign "ain_get_struct" (ain @-> string @-> returning int)
let get_enum' = foreign "ain_get_enum" (ain @-> string @-> returning int)
let get_library' = foreign "ain_get_library" (ain @-> string @-> returning int)
let get_library_function' =
  foreign "ain_get_library_function" (ain @-> int @-> string @-> returning int)
let get_functype' = foreign "ain_get_functype" (ain @-> string @-> returning int)
let get_string_no' = foreign "ain_get_string_no" (ain @-> string @-> returning int)
let add_function = foreign "ain_add_function" (ain @-> string @-> returning int)
let dup_function = foreign "ain_dup_function" (ain @-> int @-> returning int)
let add_global = foreign "ain_add_global" (ain @-> string @-> returning int)
let add_initval = foreign "ain_add_initval" (ain @-> int @-> returning int)
let add_struct = foreign "ain_add_struct" (ain @-> string @-> returning int)
let add_library = foreign "ain_add_library" (ain @-> string @-> returning int)
let add_string = foreign "ain_add_string" (ain @-> string @-> returning int)
let add_message = foreign "ain_add_message" (ain @-> string @-> returning int)
let add_file = foreign "ain_add_file" (ain @-> string @-> returning int)

let return_option i =
  if i < 0 then None else Some i

let get_function p name = get_function' p name |> return_option
(*let get_global p name = get_global' p name |> return_option*)
let get_struct p name = get_struct' p name |> return_option
let get_enum p name = get_enum' p name |> return_option
let get_library p name = get_library' p name |> return_option
let get_library_function p i name = get_library_function' p i name |> return_option
let get_functype p name = get_functype' p name |> return_option
let get_string_no p s = get_string_no' p s |> return_option

let ain_global = foreign "_ain_global" (ain @-> int @-> returning (ptr_opt (Variable.t_c)))

let get_global p name =
  match get_global' p name with
  | -1 -> None
  | i ->
      begin match ain_global p i with
      | None -> failwith "_ain_global returned NULL"
      | Some obj -> Some (Variable.of_ptr obj i)
      end

