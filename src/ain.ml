open Core

module Type = struct
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
    | HLLFunc2
    | HLLParam
    | Array of t
    | Wrap of t
    | Option of t
    | Unknown87 of t
    | IFace of int
    | Enum2 of int
    | Enum of int
    | HLLFunc
    (* only used in delegates, type name has "T" prefix (e.g., TValueType, TResult) *)
    | Unknown98
    | IFaceWrap of int
    (* internal compiler use *)
    | Function of int
    | Method of int
  and t = {
    data : data;
    is_ref : bool;
  }

  let rec int_of_data_type version o =
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
      | HLLFunc2 -> failwith "tried to create ref hll_func2" (* FIXME: 73? *)
      | HLLParam -> 75
      | Array (arrtype) ->
          if version >= 11 then
            80
          else
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
            | HLLFunc2 -> failwith "tried to create ref array<hll_func2>"
            | HLLParam -> failwith "tried to create ref array<hll_param>"
            | Array (t) -> int_of_data_type version { data=Array t; is_ref=true }
            | Wrap (_) -> failwith "tried to create ref array<wrap<...>>"
            | Option (_) -> failwith "tried to create ref array<option<...>>"
            | Unknown87 (_) -> failwith "tried to create ref array<unknown_87>"
            | IFace (_) -> failwith "tried to create ref array<interface>"
            | Enum2 (_) -> failwith "tried to create ref array<enum2>"
            | Enum (_) -> failwith "tried to create ref array<enum>"
            | HLLFunc -> failwith "tried to create ref array<hll_func>"
            | Unknown98 -> failwith "tried to create ref array<unknown_98>"
            | IFaceWrap (_) -> failwith "tried to create ref array<iface_wrap<...>>"
            | Function (_) -> failwith "tried to create ref array<function>"
            | Method (_) -> failwith "tried to create ref array<method>"
            end
      | Wrap (_) -> failwith "tried to create ref wrap<...>"
      | Option (_) -> failwith "tried to create ref option<...>"
      | Unknown87 (_) -> failwith "tried to create ref unknown_87"
      | IFace (_) -> failwith "tried to create ref interface"
      | Enum2 (_) -> failwith "tried to create ref enum2"
      | Enum (_) -> 93
      | HLLFunc -> failwith "tried to create ref hll_func"
      | Unknown98 -> failwith "tried to create ref unknown_98"
      | IFaceWrap (_) -> failwith "tried to create ref iface_wrap<...>"
      | Function (_) -> failwith "tried to create ref function"
      | Method (_) -> failwith "tried to create ref method"
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
        | HLLFunc2 -> 71
        | HLLParam -> 74
        | Array (arrtype) ->
            if version >= 11 then
              79
            else
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
              | HLLFunc2 -> failwith "tried to create array<hll_func2>"
              | HLLParam -> failwith "tried to create array<hll_param>"
              | Array (t) -> int_of_data_type version { data=Array t; is_ref=false }
              | Wrap (_) -> failwith "tried to create array<wrap<...>>"
              | Option (_) -> failwith "tried to create array<option<...>>"
              | Unknown87 (_) -> failwith "tried to create array<unknown_87>"
              | IFace (_) -> failwith "tried to create array<interface>"
              | Enum2 (_) -> failwith "tried to create array<enum2>"
              | Enum (_) -> failwith "tried to create array<enum>"
              | HLLFunc -> failwith "tried to create array<hll_func>"
              | Unknown98 -> failwith "tried to create array<unknown_98>"
              | IFaceWrap (_) -> failwith "tried to create array<iface_wrap<...>>"
              | Function (_) -> failwith "tried to create array<function>"
              | Method (_) -> failwith "tried to create array<method>"
              end
        | Wrap (_) -> 82
        | Option (_) -> 86
        | Unknown87 (_) -> 87
        | IFace (_) -> 89
        | Enum2 (_) -> 91
        | Enum (_) -> 92
        | HLLFunc -> 95
        | Unknown98 -> 98
        | IFaceWrap (_) -> 100
        | Function (_) -> failwith "tried to create function"
        | Method (_) -> failwith "tried to create method"

  let rec int_of_struct_type ?(var = false) version o =
    match o.data with
    | Struct (no) | FuncType (no) | Delegate (no) | Enum2 (no) | Enum (no)
    | IFace (no) | IFaceWrap (no) ->
        no
    | Array t ->
        (* XXX: preserve quirk with enum struct type in ain v12 (Rance 10) *)
        begin match t.data with
        | Enum _ -> if version = 12 then -1 else int_of_struct_type version t ~var
        | _ ->  int_of_struct_type version t ~var
        end
    | Wrap t | Option t | Unknown87 t ->
        int_of_struct_type version t ~var
    | Void ->
        if var && (version < 11) then 0 else -1
    | _ -> -1

  let rec int_of_rank version o =
    match (version >= 11, o.data) with
    | (false, Array t) -> 1 + (int_of_rank version t)
    | (true, Array _) -> 1
    | (_, (Wrap _ | Option _ | Unknown87 _)) -> 1
    | _ -> 0

  (* temporary representation *)
  type parsed = {
    data : int;
    struc : int;
    rank : int;
    subtype : parsed option
  }

  let make ?(is_ref=false) data =
    { data; is_ref }

  let rec of_parsed parsed =
    (* constructor for old array types *)
    let rec make_array ?(is_ref=false) data rank =
      if rank = 0 then
        make data
      else
        make (Array (make_array data (rank - 1))) ~is_ref
    in
    let struc = parsed.struc in
    let rank = parsed.rank in
    let unwrap_subtype = function
      | Some st -> of_parsed st
      | None -> make Void
    in
    match parsed.data with
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
    | 71 -> make HLLFunc2
    | 74 -> make HLLParam
    | 75 -> make HLLParam ~is_ref:true
    (* XXX: HLL function can have null subtype *)
    | 79 -> make (Array (unwrap_subtype parsed.subtype))
    | 80 -> make (Array (unwrap_subtype parsed.subtype)) ~is_ref:true
    | 82 -> make (Wrap (unwrap_subtype parsed.subtype))
    | 86 -> make (Option (unwrap_subtype parsed.subtype))
    | 87 -> make (Unknown87 (unwrap_subtype parsed.subtype))
    | 89 -> make (IFace struc)
    | 91 -> make (Enum2 struc)
    | 92 -> make (Enum struc)
    | 93 -> make (Enum struc) ~is_ref:true
    | 95 -> make HLLFunc
    | 98 -> make Unknown98
    | 100 -> make (IFaceWrap struc)
    | n -> failwith (sprintf "Invalid or unknown data type in ain file: %d" n)

  let rec equal a b =
    let data_type_equal a b =
      match a with
      | Void        -> begin match b with Void        -> true | _ -> false end
      | Int         -> begin match b with Int         -> true | _ -> false end
      | Float       -> begin match b with Float       -> true | _ -> false end
      | String      -> begin match b with String      -> true | _ -> false end
      | IMainSystem -> begin match b with IMainSystem -> true | _ -> false end
      | Bool        -> begin match b with Bool        -> true | _ -> false end
      | LongInt     -> begin match b with LongInt     -> true | _ -> false end
      | HLLFunc2    -> begin match b with HLLFunc2    -> true | _ -> false end
      | HLLParam    -> begin match b with HLLParam    -> true | _ -> false end
      | HLLFunc     -> begin match b with HLLFunc     -> true | _ -> false end
      | Unknown98   -> begin match b with Unknown98   -> true | _ -> false end
      | Struct i_a ->
          begin match b with
          | Struct i_b -> i_a = i_b
          | _ -> false
          end
      | FuncType i_a ->
          begin match b with
          | FuncType i_b -> i_a = i_b
          | _ -> false
          end
      | Delegate i_a ->
          begin match b with
          | Delegate i_b -> i_a = i_b
          | _ -> false
          end
      | Array t_a ->
          begin match b with
          | Array t_b -> equal t_a t_b
          | _ -> false
          end
      | Wrap t_a ->
          begin match b with
          | Wrap t_b -> equal t_a t_b
          | _ -> false
          end
      | Option t_a ->
          begin match b with
          | Option t_b -> equal t_a t_b
          | _ -> false
          end
      | Unknown87 t_a ->
          begin match b with
          | Unknown87 t_b -> equal t_a t_b
          | _ -> false
          end
      | IFace i_a ->
          begin match b with
          | IFace i_b -> i_a = i_b
          | _ -> false
          end
      | Enum2 i_a ->
          begin match b with
          | Enum2 i_b -> i_a = i_b
          | _ -> false
          end
      | Enum i_a ->
          begin match b with
          | Enum i_b -> i_a = i_b
          | _ -> false
          end
      | IFaceWrap i_a ->
          begin match b with
          | IFaceWrap i_b -> i_a = i_b
          | _ -> false
          end
      | Function i_a ->
          begin match b with
          | Function i_b -> i_a = i_b
          | _ -> false
          end
      | Method i_a ->
          begin match b with
          | Function i_b -> i_a = i_b
          | _ -> false
          end
    in
    (Bool.equal a.is_ref b.is_ref) && (data_type_equal a.data b.data)

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
    | HLLFunc2 -> "hll_func2"
    | HLLParam -> "hll_param"
    | Array (t) -> sprintf "array<%s>" (to_string t)
    | Wrap (t) -> sprintf "wrap<%s>" (to_string t)
    | Option (t) -> sprintf "option<%s>" (to_string t)
    | Unknown87 (t) -> sprintf "unknown87<%s>" (to_string t)
    | IFace (no) -> sprintf "interface<%d>" no (* FIXME: look up name in ain object *)
    | Enum2 (no) -> sprintf "enum2<%d>" no (* FIXME *)
    | Enum (no) -> sprintf "enum<%d>" no (* FIXME *)
    | HLLFunc -> "hll_func"
    | Unknown98 -> "unknown_98"
    | IFaceWrap (no) -> sprintf "interface_wrap<%d>" no (* FIXME: look up name in ain object *)
    | Function (no) -> if no >= 0 then sprintf "function<%d>" no else "function"
    | Method (no) -> if no >= 0 then sprintf "method<%d>" no else "method"
  and to_string o =
    let prefix = if o.is_ref then "ref " else "" in
    prefix ^ (data_to_string o.data)

end

module Variable = struct
  type initval =
    | Void
    | Int of int32
    | Float of float
    | String of string

  type t = {
    index : int;
    name : string;
    name2 : string option;
    value_type : Type.t;
    initval : initval option
  }

  let make ?(index=(-1)) name value_type =
    { index; name; name2=Some ""; value_type; initval=None }

  let equal a b =
    (String.equal a.name b.name)
    && (Option.equal String.equal a.name2 b.name2)
    && (Type.equal a.value_type b.value_type)
    (* TODO: initval *)

end

module Global = struct
  type t = {
    variable : Variable.t;
    group_index : int
  }

  let create variable group_index = { variable; group_index }

  let equal a b =
    (Variable.equal a.variable b.variable) && (a.group_index = b.group_index)
end

module Function = struct
  type t = {
    index : int;
    name : string;
    address : int;
    nr_args : int;
    vars : Variable.t list;
    return_type : Type.t;
    is_label : bool;
    is_lambda : bool;
    crc : int32;
    struct_type : int option;
    enum_type : int option
  }

  let create ?(index=(-1)) name =
    { index;
      name;
      address = -1;
      nr_args = 0;
      vars = [];
      return_type = Type.make Void;
      is_label = false;
      is_lambda = false;
      crc = 0l;
      struct_type = None;
      enum_type = None
    }

  let set_undefined f =
    { f with address = (-1); vars = List.take f.vars f.nr_args }

  let is_defined f = f.address > 1

  let logical_parameters f =
    List.filter (List.take f.vars f.nr_args) ~f:(fun (v:Variable.t) ->
      match v.value_type.data with Void -> false | _ -> true
    )

  let equal a b =
    (String.equal a.name b.name)
    && (a.nr_args = b.nr_args)
    && (List.for_all2_exn (List.take a.vars a.nr_args) (List.take b.vars b.nr_args) ~f:Variable.equal)
    && (Bool.equal a.is_label b.is_label)
    && (Bool.equal a.is_lambda b.is_lambda)
end

module Struct = struct
  type interface = {
    struct_type : int;
    vtable_offset : int
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

  let create ?(index=(-1)) name = {
    index;
    name;
    interfaces = [];
    constructor = -1;
    destructor = -1;
    members = [];
    vmethods = []
  }

  let equal a b =
    (String.equal a.name b.name)
    && ((List.length a.interfaces) = (List.length b.interfaces))
    (* TODO: interfaces? only works if structs from same ain file *)
    (* TODO: constructor & destructor? only works if structs from same ain file *)
    && ((List.length a.members) = (List.length b.members))
    && (List.for_all2_exn a.members b.members ~f:Variable.equal)
end

module Library = struct
  module Argument = struct
    type t = {
      name : string;
      value_type : Type.t
    }

    let create name value_type = { name; value_type }

    let equal a b =
      (String.equal a.name b.name) && (Type.equal a.value_type b.value_type)
  end
  module Function = struct
    type t = {
      index : int;
      lib_no : int;
      name : string;
      return_type : Type.t;
      arguments : Argument.t list
    }

    let create name return_type arguments = {
      index = -1;
      lib_no = -1;
      name;
      return_type;
      arguments
    }

    let equal a b =
      (String.equal a.name b.name)
      && (Type.equal a.return_type b.return_type)
      && (List.for_all2_exn a.arguments b.arguments ~f:Argument.equal)
  end
  type t = {
    index : int;
    name : string;
    functions : Function.t list
  }

  let equal a b =
    (String.equal a.name b.name)
    && (List.for_all2_exn a.functions b.functions ~f:Function.equal)
end

module Switch = struct
  type case_type =
    | IntCase
    | StringCase

  type t = {
    index : int;
    case_type : case_type;
    mutable default_address : int;
    mutable cases : (int32 * int) list
  }

  let case_type_of_int = function
    | 2 -> IntCase
    | 4 -> StringCase
    | _ -> failwith "invalid switch case type"

  let int_of_case_type = function
    | IntCase -> 2
    | StringCase -> 4
end

module FunctionType = struct
  type t = {
    index : int;
    name : string;
    return_type : Type.t;
    nr_arguments : int;
    variables : Variable.t list
  }

  let create name = {
    index = -1;
    name;
    return_type = Type.make Void;
    nr_arguments = 0;
    variables = []
  }

  let logical_parameters f =
    let not_void (v:Variable.t) =
      match v.value_type.data with Void -> false | _ -> true
    in
    List.filter f.variables ~f:not_void

  let function_compatible (ft:t) (f:Function.t) =
    let take_types n vars =
      let rec take_types_r n (vars : Variable.t list) result =
        if n = 0 then
          List.rev result
        else
          match vars with
          | [] -> failwith "function_compatible.take_types: n > nr_vars"
          | x::xs -> take_types_r (n - 1) xs ((x.value_type)::result)
      in
      take_types_r n vars []
    in
    (Type.equal ft.return_type f.return_type)
    && (ft.nr_arguments = f.nr_args)
    && (List.for_all2_exn (take_types ft.nr_arguments ft.variables)
                          (take_types f.nr_args f.vars))
                          ~f:Type.equal

  let equal a b =
    (String.equal a.name b.name)
    && (a.nr_arguments = b.nr_arguments)
    && (List.for_all2_exn a.variables b.variables ~f:Variable.equal)
end

module Enum = struct
  type t = {
    index : int;
    name : string;
    symbols : string list
  }
end

type t = {
  mutable major_version : int;
  mutable minor_version : int;
  mutable keyc : int32;
  mutable code : bytes;
  mutable functions : Function.t array;
  mutable globals : Global.t array;
  mutable structures : Struct.t array;
  mutable messages : string array;
  mutable msg1_uk : int32;
  mutable main : int;
  mutable msgf : int;
  mutable libraries : Library.t array;
  mutable switches : Switch.t array;
  mutable game_version : int;
  (* TODO: scenario labels *)
  mutable strings : string array;
  mutable filenames : string array;
  mutable ojmp : int;
  mutable function_types : FunctionType.t array;
  mutable delegates : FunctionType.t array;
  mutable global_group_names : string array;
  mutable enums : Enum.t array;
  mutable use_msg1 : bool;
  string_table : (string, int) Hashtbl.t
}

let create major_version minor_version = {
  major_version;
  minor_version;
  keyc=0l;
  code=Bytes.of_string "";
  functions=[| Function.create ~index:0 "NULL" |];
  globals=[||];
  structures=[||];
  messages=[||];
  msg1_uk=0l;
  main=0;
  msgf=0;
  libraries=[||];
  switches=[||];
  game_version=0;
  strings=[||];
  filenames=[||];
  ojmp = -1;
  function_types=[||];
  delegates=[||];
  global_group_names=[||];
  enums=[||];
  use_msg1=false;
  string_table=Hashtbl.create (module String)
}

let version ain = ain.major_version
let minor_version ain = ain.minor_version

let version_equal ain (major, minor) =
  (ain.major_version = major) && (ain.minor_version = minor)

let version_gte ain (major, minor) =
  if ain.major_version < major then
    false
  else if (phys_equal ain.major_version major) && (ain.minor_version < minor) then
    false
  else
    true

let version_lt ain v = not (version_gte ain v)

let if_version_gte ain version f default buf =
  if version_gte ain version then f buf else default

let if_version_between ain major_low major_high f default buf =
  if (ain.major_version <= major_low) || (ain.major_version >= major_high) then
    default
  else
    f buf

type buf = {
  ain : t;
  data : Stdlib.Bytes.t;
  mutable pos : int
}

let read_int32 buf =
  let i = Stdlib.Bytes.get_int32_le buf.data buf.pos in
  buf.pos <- buf.pos + 4;
  i

let read_int buf =
  Int32.to_int_exn (read_int32 buf)

let read_bool buf =
  not (Int32.equal (read_int32 buf) Int32.zero)

let read_float buf =
  Int32.float_of_bits (read_int32 buf)

let read_cstring buf =
  match Stdlib.Bytes.index_from_opt buf.data buf.pos '\x00' with
  | Some i ->
      let len = i - buf.pos in
      let cstr = Stdlib.Bytes.to_string (Stdlib.Bytes.sub buf.data buf.pos len) in
      (* TODO: convert to UTF-8? *)
      buf.pos <- buf.pos + (len + 1);
      cstr
  | None ->
      failwith "unterminated string"

let read_some_cstring buf =
  Some (read_cstring buf)

let read_variable_type buf =
  let rec read_variable_type' buf =
    let data = read_int buf in
    let struc = read_int buf in
    let rank = read_int buf in
    let read_array_type buf =
      if rank = 0 then
        None
      else
        Some (read_variable_type' buf)
    in
    let subtype = if_version_gte buf.ain (11, 0) read_array_type None buf in
    let (t:Type.parsed) = { data; struc; rank; subtype } in
    t
  in
  Type.of_parsed (read_variable_type' buf)

let read_return_type buf =
  if version_gte buf.ain (11, 0) then
    read_variable_type buf
  else begin
    let data = read_int buf in
    let struc = read_int buf in
    let (t:Type.parsed) = { data; struc; rank=0; subtype=None } in
    Type.of_parsed t
  end

let read_variables buf count =
  let read_initval (t:Type.t) buf =
    if read_bool buf then begin
      match t.is_ref, t.data with
      | (true, _) | (false, (Struct _ | Delegate _ | Array _)) -> Some (Variable.Void)
      | (false, String) ->
          Some (Variable.String (read_cstring buf))
      | (false, Float) ->
          Some (Variable.Float (read_float buf))
      | (false, _) ->
          Some (Variable.Int (read_int32 buf))
    end else
      None
  in
  let rec read_variables' count index result =
    if count > 0 then begin
      let name = read_cstring buf in
      let name2 = if_version_gte buf.ain (12, 0) read_some_cstring None buf in
      let value_type = read_variable_type buf in
      let initval = if_version_gte buf.ain (8, 0) (read_initval value_type) None buf in
      let (v:Variable.t) = { index; name; name2; value_type; initval } in
      read_variables' (count - 1) (index + 1) (v::result)
    end else
      List.rev result
  in
  read_variables' count 0 []

let read_functions buf count =
  let rec read_functions' count result index =
    if count > 0 then begin
      let address = read_int buf in
      let name = read_cstring buf in
      (* detect game (to apply needed quirks) *)
      if version_equal buf.ain (14, 1) then begin
        match name with
        | "C_MedicaMenu@0" (* Evenicle 2 *)
        | "CInvasionHexScene@0" (* Haha Ranman *)
        | "_ALICETOOLS_AINV14_00" ->
            buf.ain.minor_version <- 0
        | _ -> ()
      end;
      let is_label = if_version_between buf.ain 1 7 read_bool false buf in
      let return_type = read_return_type buf in
      let nr_args = read_int buf in
      let nr_vars = read_int buf in
      let is_lambda = if_version_gte buf.ain (11, 0) read_bool false buf in
      let crc = if_version_gte buf.ain (2, 0) read_int32 Int32.zero buf in
      let vars = read_variables buf nr_vars in
      let (f:Function.t) = {
        index;
        name;
        address;
        nr_args;
        vars;
        return_type;
        is_label;
        is_lambda;
        crc;
        struct_type = None;
        enum_type = None
      } in
      read_functions' (count - 1) (f::result) (index + 1)
    end else
      List.rev result
  in
  read_functions' count [] 0

let read_globals buf count =
  let rec read_globals' count index result =
    if count > 0 then begin
      let name = read_cstring buf in
      let name2 = if_version_gte buf.ain (12, 0) read_some_cstring None buf in
      let value_type = read_variable_type buf in
      let group_index = if_version_gte buf.ain (5, 0) read_int 0 buf in
      let (variable:Variable.t) = { index; name; name2; value_type; initval=None } in
      let (global:Global.t) = { variable; group_index } in
      read_globals' (count - 1) (index + 1) (global::result)
    end else
      List.rev result
  in
  read_globals' count 0 []

let read_global_initvals buf count =
  let read_initval = function
    | 11 -> Variable.Float (read_float buf)
    | 12 -> Variable.String (read_cstring buf)
    | _  -> Variable.Int (read_int32 buf)
  in
  let rec read_global_initvals' count =
    if count > 0 then begin
      let index = read_int buf in
      let initval = Some (read_initval (read_int buf)) in
      let g = buf.ain.globals.(index) in
      Array.set buf.ain.globals index { g with variable = { g.variable with initval } };
      read_global_initvals' (count - 1)
    end else
      ()
  in
  read_global_initvals' count

let read_structures buf count =
  let rec read_interfaces count result =
    if count > 0 then begin
      let struct_type = read_int buf in
      let vtable_offset = read_int buf in
      let (i:Struct.interface) = { struct_type; vtable_offset } in
      read_interfaces (count - 1) (i::result)
    end else
      List.rev result
  in
  let rec read_vmethods count result =
    if count > 0 then begin
      let no = read_int buf in
      read_vmethods (count - 1) (no::result)
    end else
      List.rev result
  in
  let rec read_structures' count result index =
    if count > 0 then begin
      let name = read_cstring buf in
      let nr_interfaces = if_version_gte buf.ain (11, 0) read_int 0 buf in
      let interfaces = read_interfaces nr_interfaces [] in
      let constructor = read_int buf in
      let destructor = read_int buf in
      let nr_members = read_int buf in
      let members = read_variables buf nr_members in
      let nr_vmethods = if_version_gte buf.ain (14, 1) read_int 0 buf in
      let vmethods = read_vmethods nr_vmethods [] in
      let (s:Struct.t) = { index; name; interfaces; constructor; destructor; members; vmethods } in
      read_structures' (count - 1) (s::result) (index + 1)
    end else
      List.rev result
  in
  read_structures' count [] 0

let read_cstrings buf count =
  let rec read_cstrings' count result =
    if count > 0 then
      read_cstrings' (count - 1) ((read_cstring buf)::result)
    else
      List.rev result
  in
  read_cstrings' count []

let read_msg1_strings buf count =
  let read_msg1_string () =
    let len = read_int buf in
    let data = Bytes.sub buf.data ~pos:buf.pos ~len in
    buf.pos <- buf.pos + len;
    for i = 0 to (Bytes.length data) - 1 do
      let c = Char.to_int (Bytes.get data i) in
      Bytes.set data i (Char.of_int_exn ((c - 0x60 - i) % 256))
    done;
    Bytes.to_string data
  in
  let rec read_msg1_strings' count result =
    if count > 0 then
      read_msg1_strings' (count - 1) ((read_msg1_string ())::result)
    else
      List.rev result
  in
  read_msg1_strings' count []

let read_libraries buf count =
  let read_library_type buf =
    if version_gte buf.ain (14, 0) then
      read_variable_type buf
    else begin
      let data = read_int buf in
      (* XXX: rank is 1 in case it's an array type *)
      let (t:Type.parsed) = { data; struc = -1; rank = 1; subtype = None } in
      Type.of_parsed t
    end
  in
  let rec read_library_arguments count result =
    if count > 0 then begin
      let name = read_cstring buf in
      let value_type = read_library_type buf in
      let (arg:Library.Argument.t) = { name; value_type } in
      read_library_arguments (count - 1) (arg::result)
    end else
      List.rev result
  in
  let rec read_libraries' count lib_no result =
    let rec read_library_functions count fno result =
      if count > 0 then begin
        let name = read_cstring buf in
        let return_type = read_library_type buf in
        let nr_arguments = read_int buf in
        let arguments = read_library_arguments nr_arguments [] in
        let (f:Library.Function.t) = { index=fno; lib_no; name; return_type; arguments } in
        read_library_functions (count - 1) (fno + 1) (f::result)
      end else
        List.rev result
    in
    if count > 0 then begin
      let name = read_cstring buf in
      let nr_functions = read_int buf in
      let functions = read_library_functions nr_functions 0 [] in
      let (lib:Library.t) = { index=lib_no; name; functions } in
      read_libraries' (count - 1) (lib_no + 1) (lib::result)
    end else
      List.rev result
  in
  read_libraries' count 0 []

let read_switches buf count =
  let rec read_switches' count index result =
    let rec read_switch_cases count result =
      if count > 0 then begin
        let value = read_int32 buf in
        let address = read_int buf in
        read_switch_cases (count - 1) ((value, address)::result)
      end else
        List.rev result
    in
    if count > 0 then begin
      let case_type = Switch.case_type_of_int (read_int buf) in
      let default_address = read_int buf in
      let nr_cases = read_int buf in
      let cases = read_switch_cases nr_cases [] in
      let (switch:Switch.t) = { index; case_type; default_address; cases } in
      read_switches' (count - 1) (index + 1) (switch::result)
    end else
      List.rev result
  in
  read_switches' count 0 []

let read_function_types buf count =
  let rec read_function_types' count index result =
    if count > 0 then begin
      let name = read_cstring buf in
      let return_type = read_return_type buf in
      let nr_arguments = read_int buf in
      let nr_variables = read_int buf in
      let variables = read_variables buf nr_variables in
      let (ft:FunctionType.t) = { index; name; return_type; nr_arguments; variables } in
      read_function_types' (count - 1) (index + 1) (ft::result)
    end else
      List.rev result
  in
  read_function_types' count 0 []

let read_enums buf count =
  read_cstrings buf count
  |> List.mapi ~f:(fun index name ->
      (* TODO: symbols *)
      let e:Enum.t = { index; name; symbols=[] } in
      e
  )

module MT19937 = struct

  (* period parameters *)
  let n = 624
  let m = 397
  let matrix_a = 0x9908b0dfl
  let upper_mask = 0x80000000l
  let lower_mask = 0x7fffffffl
  (* tempering parameters *)
  let tempering_mask_b = 0x9d2c5680l
  let tempering_mask_c = 0xefc60000l
  let tempering_shift_u y = Int32.(y lsr 11)
  let tempering_shift_s y = Int32.(y lsl 7)
  let tempering_shift_t y = Int32.(y lsl 15)
  let tempering_shift_l y = Int32.(y lsr 18)
  (* mag01[x] = x * matrix_a for x=0,1 *)
  let mag01 = [| 0l; matrix_a |]

  type t = {
    st : int32 array;
    mutable i : int
  }

  let init seed =
    let mt = { st = Array.create 0l ~len:624; i = 624 } in
    Array.set mt.st 0 seed;
    for i = 1 to (n - 1) do
      let prev = Array.get mt.st (i - 1) in
      Array.set mt.st i Int32.((of_int_exn 69069) * prev)
    done;
    mt.i <- n;
    mt

  let genrand mt =
    let gen k1 k2 k3 =
      (* y = (mt->st[k1]&UPPER_MASK)|(mt->st[k2]&LOWER_MASK); *)
      let y = Int32.(((Array.get mt.st k1) land upper_mask) lor ((Array.get mt.st k2) land lower_mask)) in
      (* mt->st[k1] = mt->st[k3] ^ (y >> 1) ^ mag01[y & 0x1]; *)
      let v = Int32.((Array.get mt.st k3) lxor (y lsr 1)) in
      Array.set mt.st k1 Int32.(v lxor (Array.get mag01 (to_int_trunc (y land 1l))))
    in
    (* generate n words at one time *)
    if mt.i >= n then begin
      for kk = 0 to ((n - m) - 1) do
        gen kk (kk + 1) (kk + m)
      done;
      for kk = (n - m) to (n - 2) do
        gen kk (kk + 1) (kk + (m - n))
      done;
      gen (n - 1) 0 (m - 1);
      mt.i <- 0
    end;

    (* y = mt->st[mt->i++]; *)
    let y = Array.get mt.st mt.i in
    mt.i <- mt.i + 1;
    (* y ^= TEMPERING_SHIFT_U(y); *)
    let y = Int32.(y lxor (tempering_shift_u y)) in
    (* y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B; *)
    let y = Int32.(y lxor ((tempering_shift_s y) land tempering_mask_b)) in
    (* y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C; *)
    let y = Int32.(y lxor ((tempering_shift_t y) land tempering_mask_c)) in
    (* y ^= TEMPERING_SHIFT_L(y); *)
    let y = Int32.(y lxor (tempering_shift_l y)) in
    y
end

let decrypt buf =
  let mt = MT19937.init 0x5d3e3l in
  let decrypt_byte b =
    let r = MT19937.genrand mt in
    let e = Int32.of_int_trunc (Char.to_int b) in
    Char.of_int_exn Int32.(to_int_trunc ((r lxor e) land 0xffl))
  in
  for i = 0 to (Stdlib.Bytes.length buf) - 1 do
    Stdlib.Bytes.set buf i (decrypt_byte (Stdlib.Bytes.get buf i))
  done

(* symmetric *)
let encrypt = decrypt

let load filename =
  let read_section_magic buf =
    let s = Stdlib.Bytes.sub_string buf.data buf.pos 4 in
    buf.pos <- buf.pos + 4;
    s
  in
  let read_section buf =
    match read_section_magic buf with
    | "VERS" ->
        buf.ain.major_version <- read_int buf;
        if buf.ain.major_version = 14 then
          buf.ain.minor_version <- 1
    | "KEYC" ->
        buf.ain.keyc <- read_int32 buf
    | "CODE" ->
        let len = read_int buf in
        buf.ain.code <- Bytes.sub buf.data ~pos:buf.pos ~len;
        buf.pos <- buf.pos + len
    | "FUNC" ->
        let count = read_int buf in
        buf.ain.functions <- Array.of_list (read_functions buf count)
    | "GLOB" ->
        let count = read_int buf in
        buf.ain.globals <- Array.of_list (read_globals buf count)
    | "GSET" ->
        let count = read_int buf in
        read_global_initvals buf count
    | "STRT" ->
        let count = read_int buf in
        buf.ain.structures <- Array.of_list (read_structures buf count)
    | "MSG0" ->
        let count = read_int buf in
        buf.ain.messages <- Array.of_list (read_cstrings buf count);
        buf.ain.use_msg1 <- false
    | "MSG1" ->
        let count = read_int buf in
        buf.ain.msg1_uk <- read_int32 buf;
        buf.ain.messages <- Array.of_list (read_msg1_strings buf count);
        buf.ain.use_msg1 <- true
    | "MAIN" ->
        buf.ain.main <- read_int buf
    | "MSGF" ->
        buf.ain.msgf <- read_int buf
    | "HLL0" ->
        let count = read_int buf in
        buf.ain.libraries <- Array.of_list (read_libraries buf count)
    | "SWI0" ->
        let count = read_int buf in
        buf.ain.switches <- Array.of_list (read_switches buf count)
    | "SLBL" ->
        failwith "scenario labels not implemented"
    | "STR0" ->
        let count = read_int buf in
        buf.ain.strings <- Array.of_list (read_cstrings buf count)
    | "FNAM" ->
        let count = read_int buf in
        buf.ain.filenames <- Array.of_list (read_cstrings buf count)
    | "OJMP" ->
        buf.ain.ojmp <- read_int buf
    | "GVER" ->
        buf.ain.game_version <- read_int buf
    | "FNCT" ->
        let (_:int32) = read_int32 buf in (* section size *)
        let count = read_int buf in
        buf.ain.function_types <- Array.of_list (read_function_types buf count)
    | "DELG" ->
        let (_:int32) = read_int32 buf in (* section size *)
        let count = read_int buf in
        buf.ain.delegates <- Array.of_list (read_function_types buf count)
    | "OBJG" ->
        let count = read_int buf in
        buf.ain.global_group_names <- Array.of_list (read_cstrings buf count)
    | "ENUM" ->
        let count = read_int buf in
        buf.ain.enums <- Array.of_list (read_enums buf count)
    | s ->
        failwith (Printf.sprintf "unhandled section: %s" s)
  in
  let rec read_sections buf =
    read_section buf;
    if buf.pos >= (Stdlib.Bytes.length buf.data) then
      buf.ain
    else
      read_sections buf
  in
  let load' file =
    let ain = create 4 0 in
    let magic = Buffer.create 4 in
    Option.value_exn (In_channel.input_buffer file magic ~len:4);
    begin match Buffer.contents magic with
    | "AI2\x00" -> (* compressed ain *)
        let read_int ch =
          let buf = Buffer.create 4 in
          Option.value_exn (In_channel.input_buffer ch buf ~len:4);
          Int32.to_int_exn (Stdlib.String.get_int32_le (Buffer.contents buf) 0)
        in
        In_channel.seek file 8L;
        let out_len = read_int file in
        In_channel.seek file 16L;
        let ain_buf = { ain; data=Bytes.create out_len; pos=0 } in
        let refill buf =
          In_channel.input file ~buf ~pos:0 ~len:(Bytes.length buf)
        in
        let flush buf len =
          Bytes.blit ~src:buf ~src_pos:0 ~dst:ain_buf.data ~dst_pos:ain_buf.pos ~len;
          ain_buf.pos <- ain_buf.pos + len
        in
        Zlib.uncompress refill flush;
        ain_buf.pos <- 0;
        read_sections ain_buf
    | "\x7e\xf5\x02\xba" -> (* encrypted "VERS" *)
        In_channel.seek file 0L;
        let data = Bytes.of_string (In_channel.input_all file) in
        let buf = { ain; data; pos=0 } in
        decrypt buf.data;
        read_sections buf
    | "VERS" -> (* raw (as produced by `alice ain dump -d`) *)
        In_channel.seek file 0L;
        let data = Bytes.of_string (In_channel.input_all file) in
        let buf = { ain; data; pos=0 } in
        read_sections buf
    | _ ->
        failwith "unrecognized .ain format"
    end
  in
  In_channel.with_file filename ~f:load'

module BinBuffer = struct
  include Buffer

  let conv_buf32 = Bytes.create 4

  let add_int32 buf i =
    Stdlib.Bytes.set_int32_le conv_buf32 0 i;
    Buffer.add_bytes buf conv_buf32

  let add_int buf i =
    add_int32 buf (Int32.of_int_trunc i)

  let add_float buf f =
    add_int32 buf (Int32.bits_of_float f)

  let add_bool buf b =
    Buffer.add_string buf (if b then "\x01\x00\x00\x00" else "\x00\x00\x00\x00")

  let add_cstring buf s =
    Buffer.add_string buf s;
    Buffer.add_char buf '\x00'

end

let write_msg1_string buf s =
  let tmp = Bytes.of_string s in
  let len = Bytes.length tmp in
  BinBuffer.add_int buf len;
  for i = 0 to len - 1 do
    let c = Char.to_int (Bytes.get tmp i) in
    Bytes.set tmp i (Char.of_int_exn ((c + 0x60 + i) % 256))
  done;
  BinBuffer.add_bytes buf tmp

let rec write_variable_type ?(var=true) buf ain (t:Type.t) =
  BinBuffer.add_int buf (Type.int_of_data_type ain.major_version t);
  BinBuffer.add_int buf (Type.int_of_struct_type ain.major_version t ~var);
  BinBuffer.add_int buf (Type.int_of_rank ain.major_version t);
  if version_gte ain (11, 0) then begin
    match t.data with
    | Array t | Wrap t | Option t | Unknown87 t ->
        write_variable_type ~var buf ain t
    | _ -> ()
  end

let write_return_type buf ain (t:Type.t) =
  if version_gte ain (11, 0) then
    write_variable_type ~var:false buf ain t
  else begin
    BinBuffer.add_int buf (Type.int_of_data_type ain.major_version t);
    BinBuffer.add_int buf (Type.int_of_struct_type ain.major_version t)
  end

let write_string_option buf opt =
  Option.iter opt ~f:(BinBuffer.add_cstring buf)

let write_variable buf ain (v:Variable.t) =
  let module BB = BinBuffer in
  let write_variable_initval (opt:Variable.initval option) =
    BB.add_bool buf (Option.is_some opt);
    begin match opt with
    | Some (Void) -> ()
    | Some (Int i) -> BB.add_int32 buf i
    | Some (Float f) -> BB.add_float buf f
    | Some (String s) -> BB.add_cstring buf s
    | None -> ()
    end
  in
  BB.add_cstring buf v.name;
  if_version_gte ain (12, 0) (write_string_option buf) () v.name2;
  write_variable_type buf ain v.value_type;
  if_version_gte ain (8, 0) write_variable_initval () v.initval

let write_function buf ain (f:Function.t) =
  let module BB = BinBuffer in
  BB.add_int buf f.address;
  BB.add_cstring buf f.name;
  if_version_between ain 1 7 (BB.add_bool buf) () f.is_label;
  write_return_type buf ain f.return_type;
  BB.add_int buf f.nr_args;
  BB.add_int buf (List.length f.vars);
  if_version_gte ain (11, 0) (BB.add_bool buf) () f.is_lambda;
  if_version_gte ain (2, 0) (BB.add_int32 buf) () f.crc;
  List.iter f.vars ~f:(write_variable buf ain)

let write_global buf ain (g:Global.t) =
  BinBuffer.add_cstring buf g.variable.name;
  if_version_gte ain (12, 0) (write_string_option buf) () g.variable.name2;
  write_variable_type buf ain g.variable.value_type;
  if_version_gte ain (5, 0) (BinBuffer.add_int buf) () g.group_index

let write_initval buf (i, data_type, (initval:Variable.initval)) =
  let module BB = BinBuffer in
  BB.add_int buf i;
  BB.add_int buf data_type;
  begin match initval with
  | Void -> ()
  | Int i -> BB.add_int32 buf i
  | Float f -> BB.add_float buf f
  | String s -> BB.add_cstring buf s
  end

let write_structure buf ain (s:Struct.t) =
  let module BB = BinBuffer in
  let write_interfaces interfaces =
    let write_interface (iface:Struct.interface) =
      BB.add_int buf iface.struct_type;
      BB.add_int buf iface.vtable_offset
    in
    BB.add_int buf (List.length interfaces);
    List.iter interfaces ~f:write_interface
  in
  let write_vmethods vmethods =
    BB.add_int buf (List.length vmethods);
    List.iter vmethods ~f:(BB.add_int buf)
  in
  BB.add_cstring buf s.name;
  if_version_gte ain (11, 0) write_interfaces () s.interfaces;
  BB.add_int buf s.constructor;
  BB.add_int buf s.destructor;
  BB.add_int buf (List.length s.members);
  List.iter s.members ~f:(write_variable buf ain);
  if_version_gte ain (14, 1) write_vmethods () s.vmethods

let write_library buf ain (lib:Library.t) =
  let module BB = BinBuffer in
  let write_library_type (t:Type.t) =
    if version_gte ain (14, 0) then
      write_variable_type buf ain t
    else
      BB.add_int buf (Type.int_of_data_type ain.major_version t)
  in
  let write_library_argument (arg:Library.Argument.t) =
    BB.add_cstring buf arg.name;
    write_library_type arg.value_type
  in
  let write_library_function (f:Library.Function.t) =
    BB.add_cstring buf f.name;
    if version_gte ain (14, 0) then
      write_variable_type buf ain f.return_type
    else
      BB.add_int buf (Type.int_of_data_type ain.major_version f.return_type);
    BB.add_int buf (List.length f.arguments);
    List.iter f.arguments ~f:write_library_argument
  in
  BB.add_cstring buf lib.name;
  BB.add_int buf (List.length lib.functions);
  List.iter lib.functions ~f:write_library_function

let write_switch buf (sw:Switch.t) =
  let module BB = BinBuffer in
  let write_switch_case (value, addr) =
    BB.add_int32 buf value;
    BB.add_int buf addr
  in
  BB.add_int buf (Switch.int_of_case_type sw.case_type);
  BB.add_int buf sw.default_address;
  BB.add_int buf (List.length sw.cases);
  List.iter sw.cases ~f:write_switch_case

let write_functype buf ain (ft:FunctionType.t) =
  let module BB = BinBuffer in
  BB.add_cstring buf ft.name;
  write_return_type buf ain ft.return_type;
  BB.add_int buf ft.nr_arguments;
  BB.add_int buf (List.length ft.variables);
  List.iter ft.variables ~f:(write_variable buf ain)

let to_buffer ain =
  let global_initvals =
    let f i acc (g:Global.t) =
      match g.variable.initval with
      | Some initval ->
          let data_type = Type.int_of_data_type ain.major_version g.variable.value_type in
          (i, data_type, initval)::acc
      | None -> acc
    in
    List.rev (Array.foldi ain.globals ~init:[] ~f)
  in
  let buf = BinBuffer.create 1024 in
  let module BB = BinBuffer in
  BB.add_string buf "VERS";
  BB.add_int buf ain.major_version;
  if ain.major_version < 12 then begin
    BB.add_string buf "KEYC";
    BB.add_int32 buf ain.keyc
  end;
  BB.add_string buf "CODE";
  BB.add_int buf (Bytes.length ain.code);
  BB.add_bytes buf ain.code;
  BB.add_string buf "FUNC";
  BB.add_int buf (Array.length ain.functions);
  Array.iter ain.functions ~f:(write_function buf ain);
  BB.add_string buf "GLOB";
  BB.add_int buf (Array.length ain.globals);
  Array.iter ain.globals ~f:(write_global buf ain);
  if ain.major_version < 12 then begin
    BB.add_string buf "GSET";
    BB.add_int buf (List.length global_initvals);
    List.iter global_initvals ~f:(write_initval buf)
  end;
  BB.add_string buf "STRT";
  BB.add_int buf (Array.length ain.structures);
  Array.iter ain.structures ~f:(write_structure buf ain);
  if not ain.use_msg1 then begin
    BB.add_string buf "MSG0";
    BB.add_int buf (Array.length ain.messages);
    Array.iter ain.messages ~f:(BB.add_cstring buf)
  end else begin
    BB.add_string buf "MSG1";
    BB.add_int buf (Array.length ain.messages);
    BB.add_int32 buf ain.msg1_uk;
    Array.iter ain.messages ~f:(write_msg1_string buf)
  end;
  BB.add_string buf "MAIN";
  BB.add_int buf ain.main;
  if ain.major_version < 12 then begin
    BB.add_string buf "MSGF";
    BB.add_int buf ain.msgf
  end;
  BB.add_string buf "HLL0";
  BB.add_int buf (Array.length ain.libraries);
  Array.iter ain.libraries ~f:(write_library buf ain);
  BB.add_string buf "SWI0";
  BB.add_int buf (Array.length ain.switches);
  Array.iter ain.switches ~f:(write_switch buf);
  BB.add_string buf "GVER";
  BB.add_int buf ain.game_version;
  (* TODO: scenario labels *)
  BB.add_string buf "STR0";
  BB.add_int buf (Array.length ain.strings);
  Array.iter ain.strings ~f:(BB.add_cstring buf);
  if ain.major_version < 12 then begin
    BB.add_string buf "FNAM";
    BB.add_int buf (Array.length ain.filenames);
    Array.iter ain.filenames ~f:(BB.add_cstring buf)
  end;
  if ain.major_version < 7 then begin
    BB.add_string buf "OJMP";
    BB.add_int buf ain.ojmp
  end;
  (* XXX: section disappears in Rance IX (mid v6) *)
  if (Array.length ain.function_types) > 0 then begin
    BB.add_string buf "FNCT";
    BB.add_int buf 0; (* FIXME: section size *)
    BB.add_int buf (Array.length ain.function_types);
    Array.iter ain.function_types ~f:(write_functype buf ain)
  end;
  (* XXX: section first appears in Oyako Rankan (mid v6) *)
  if (Array.length ain.delegates) > 0 then begin
    BB.add_string buf "DELG";
    BB.add_int buf 0; (* FIXME: section size *)
    BB.add_int buf (Array.length ain.delegates);
    Array.iter ain.delegates ~f:(write_functype buf ain)
  end;
  if version_gte ain (5, 0) then begin
    BB.add_string buf "OBJG";
    BB.add_int buf (Array.length ain.global_group_names);
    Array.iter ain.global_group_names ~f:(BB.add_cstring buf)
  end;
  if version_gte ain (12, 0) then begin
    BB.add_string buf "ENUM";
    BB.add_int buf (Array.length ain.enums);
    Array.iter ain.enums ~f:(fun e -> BB.add_cstring buf e.name)
  end;
  buf

let write ?(raw = false) ain out =
  let write_buffer buf =
    if raw then
      Out_channel.output_buffer out buf
    else if version_gte ain (5, 0) then begin
      let write_int32 i =
        let tmp = Bytes.create 4 in
        Stdlib.Bytes.set_int32_le tmp 0 i;
        Out_channel.output_bytes out tmp
      in
      (* write header *)
      Out_channel.output_string out "AI2\x00";
      write_int32 0l; (* ??? *)
      write_int32 (Int32.of_int_exn (BinBuffer.length buf)); (* uncompressed size *)
      write_int32 0l; (* compressed size determined later *)
      (* compress *)
      let ain_buf = { ain; data=BinBuffer.contents_bytes buf; pos=0 } in
      let refill zipbuf =
        let in_len = (Bytes.length ain_buf.data) - ain_buf.pos in
        let out_len = Bytes.length zipbuf in
        let len = min in_len out_len in
        Bytes.blit ~src:ain_buf.data ~src_pos:ain_buf.pos ~dst:zipbuf ~dst_pos:0 ~len;
        ain_buf.pos <- ain_buf.pos + len;
        len
      in
      let flush zipbuf len =
        Out_channel.output out ~buf:zipbuf ~pos:0 ~len
      in
      Zlib.compress refill flush;
      let compressed_size = Int64.(to_int32_exn ((Out_channel.pos out) - 16L)) in
      Out_channel.seek out 12L;
      write_int32 compressed_size
    end else begin
      (* encrypt *)
      let data = BinBuffer.contents_bytes buf in
      encrypt data;
      Out_channel.output_bytes out data
    end
  in
  to_buffer ain |> write_buffer

let write_file ain file =
  Out_channel.with_file file ~f:(write ain) ~binary:true

(* globals *)

let get_global ain name =
  Array.find ain.globals ~f:(fun g -> String.equal g.variable.name name)
  |> Option.map ~f:(fun g -> g.variable)

let get_globali ain name =
  Array.findi ain.globals ~f:(fun _ g -> String.equal g.variable.name name)

let get_global_by_index ain no =
  ain.globals.(no).variable

let set_global_type ain name t =
  match get_globali ain name with
  | Some (i, g) ->
      Array.set ain.globals i { g with variable = { g.variable with value_type = t } }
  | None -> failwith (sprintf "No global named '%s' in ain object" name)

(* FIXME: this sucks *)
(*        keep a list of added globals/etc. and append to array only when lookup is OOB *)
(* FIXME? should probably create copy of `g` so that mutations don't alter original *)
let write_new_global ain (v:Variable.t) =
  let index = Array.length ain.globals in
  let g:Global.t = { variable = { v with index }; group_index = 0 } in
  ain.globals <- Array.append ain.globals [| g |];
  index

let add_global ain name =
  let index = Array.length ain.globals in
  let variable = Variable.make ~index name (Type.make Void) in
  let g = Global.create variable (-1) in
  ain.globals <- Array.append ain.globals [| g |];
  index

(* functions *)

let get_function ain name =
  Array.find ain.functions ~f:(fun f -> String.equal f.name name)

let [@warning "-32"] get_function_index ain name =
  match Array.findi ain.functions ~f:(fun _ f -> String.equal f.name name) with
  | Some (i, _) -> Some i
  | None -> None

let get_function_by_index ain no =
  ain.functions.(no)

let write_function ain (f:Function.t) =
  Array.set ain.functions f.index f

let write_new_function ain f =
  let index = Array.length ain.functions in
  ain.functions <- Array.append ain.functions [| { f with index } |];
  index

let add_function ain name =
  let no = Function.create name |> write_new_function ain in
  ain.functions.(no)

let dup_function ain no =
  ain.functions.(no) |> write_new_function ain

(* structures *)

let get_struct ain name =
  Array.find ain.structures ~f:(fun s -> String.equal s.name name)

let get_struct_index ain name =
  match Array.findi ain.structures ~f:(fun _ s -> String.equal s.name name) with
  | Some (i, _) -> Some i
  | None -> None

let get_struct_by_index ain no =
  ain.structures.(no)

let write_struct ain (s:Struct.t) =
  Array.set ain.structures s.index s

let write_new_struct ain s =
  let index = Array.length ain.structures in
  ain.structures <- Array.append ain.structures [| { s with index } |];
  index

let add_struct ain name =
  let no = Struct.create name |> write_new_struct ain in
  ain.structures.(no)

(* switches *)

let write_switch ain (switch:Switch.t) =
  Array.set ain.switches switch.index switch

let add_switch ain =
  let index = Array.length ain.switches in
  let s:Switch.t = { index; case_type=IntCase; default_address=(-1); cases=[] } in
  ain.switches <- Array.append ain.switches [| s |];
  s

(* enums *)

let get_enum ain name =
  match Array.findi ain.enums ~f:(fun _ e -> String.equal e.name name) with
  | Some (i, _) -> Some i
  | None -> None

(* libraries *)

let get_library_index ain name =
  match Array.findi ain.libraries ~f:(fun _ lib -> String.equal lib.name name) with
  | Some (i, _) -> Some i
  | None -> None

let get_library_function_index ain lib_no name =
  match List.findi ain.libraries.(lib_no).functions ~f:(fun _ f -> String.equal f.name name) with
  | Some (i, _) -> Some i
  | None -> None

let get_library_by_index ain no =
  ain.libraries.(no)

let write_library ain (lib:Library.t) =
  Array.set ain.libraries lib.index lib

let write_new_library ain lib =
  let index = Array.length ain.libraries in
  ain.libraries <- Array.append ain.libraries [| { lib with index } |];
  index

let add_library ain name =
  let lib:Library.t = { index=(-1); name; functions=[] } in
  let no = write_new_library ain lib in
  ain.libraries.(no)

let function_of_hll_function_index ain lib_no fun_no : Function.t =
  let lib_fun = List.nth_exn ain.libraries.(lib_no).functions fun_no in
  let var_of_hll_arg index (arg : Library.Argument.t) : Variable.t =
    { index; name=arg.name; name2=None; value_type=arg.value_type; initval=None }
  in
  { index = -1;
    name = lib_fun.name;
    address = -1;
    nr_args = List.length lib_fun.arguments;
    vars = List.mapi lib_fun.arguments ~f:var_of_hll_arg;
    return_type = lib_fun.return_type;
    is_label = false;
    is_lambda = false;
    crc = 0l;
    struct_type = None;
    enum_type = None
  }

(* function types *)

let get_functype ain name =
  Array.find ain.function_types ~f:(fun ft -> String.equal ft.name name)

let get_functype_index ain name =
  match Array.findi ain.function_types ~f:(fun _ ft -> String.equal ft.name name) with
  | Some (i, _) -> Some i
  | None -> None

let get_functype_by_index ain no =
  ain.function_types.(no)

let write_functype ain (ft:FunctionType.t) =
  Array.set ain.function_types ft.index ft

let write_new_functype ain ft =
  let index = Array.length ain.function_types in
  ain.function_types <- Array.append ain.function_types [| { ft with index } |];
  index

let add_functype ain name =
  let no = FunctionType.create name |> write_new_functype ain in
  ain.function_types.(no)

(* TODO: should be FunctionType.to_function *)
let function_of_functype (ft:FunctionType.t) no : Function.t =
  { index = no;
    address = -1;
    name = ft.name;
    nr_args = ft.nr_arguments;
    vars = ft.variables;
    return_type = ft.return_type;
    is_label = false;
    is_lambda = false;
    crc = 0l;
    struct_type = None;
    enum_type = None
  }

let function_of_functype_index ain no =
  function_of_functype ain.function_types.(no) no

(* delegates *)

let get_delegate ain name =
  Array.find ain.delegates ~f:(fun dg -> String.equal dg.name name)

let get_delegate_index ain name =
  match Array.findi ain.delegates ~f:(fun _ dg -> String.equal dg.name name) with
  | Some (i, _) -> Some i
  | None -> None

let get_delegate_by_index ain no =
  ain.delegates.(no)

let write_delegate ain (dg:FunctionType.t) =
  Array.set ain.delegates dg.index dg

let write_new_delegate ain dg =
  let index = Array.length ain.delegates in
  ain.delegates <- Array.append ain.delegates [| { dg with index } |];
  index

let add_delegate ain name =
  let no = FunctionType.create name |> write_new_delegate ain in
  ain.delegates.(no)

let function_of_delegate_index ain no =
  function_of_functype ain.delegates.(no) no

(* strings, messages, files *)

(* FIXME: this shouldn't return an option? *)
let get_string ain no =
  if no >= (Array.length ain.strings) then
    None
  else
    Some ain.strings.(no)

let init_string_table ain =
  if (Hashtbl.length ain.string_table) = 0 then begin
    Array.iteri ain.strings ~f:(fun index str ->
      match Hashtbl.add ain.string_table ~key:str ~data:index with
      | `Duplicate -> ()
      | `Ok -> ()
    )
  end

let add_string ain str =
  init_string_table ain;
  begin match Hashtbl.find ain.string_table str with
  | Some index -> index
  | None ->
      let index = Array.length ain.strings in
      ain.strings <- Array.append ain.strings [| str |];
      Hashtbl.add_exn ain.string_table ~key:str ~data:index;
      index
  end

let get_string_no ain str =
  init_string_table ain;
  Hashtbl.find ain.string_table str

(* FIXME: this shouldn't return an option? *)
let get_message ain no =
  if no >= (Array.length ain.messages) then
    None
  else
    Some ain.messages.(no)

let add_message ain str =
  let index = Array.length ain.messages in
  ain.messages <- Array.append ain.messages [| str |];
  index

let add_file ain name =
  let index = Array.length ain.filenames in
  ain.filenames <- Array.append ain.filenames [| name |];
  index

(* code *)

let get_code ain = ain.code

(* FIXME: should keep a list of bytes objects and defer concatenation *)
let append_bytecode ain (buf:CBuffer.t) =
  let cur_len = Bytes.length ain.code in
  let code = Bytes.create (cur_len + buf.pos) in
  Bytes.blit ~src:ain.code ~src_pos:0 ~dst:code ~dst_pos:0 ~len:cur_len;
  Bytes.blit ~src:buf.buf ~src_pos:0 ~dst:code ~dst_pos:cur_len ~len:buf.pos;
  ain.code <- code

let code_size ain =
  Bytes.length ain.code

let set_main_function ain no =
  ain.main <- no

let set_message_function ain no =
  ain.msgf <- no

let nr_globals ain = Array.length ain.globals
let nr_functions ain = Array.length ain.functions
let nr_structs ain = Array.length ain.structures
let nr_functypes ain = Array.length ain.function_types
let nr_delegates ain = Array.length ain.delegates
let nr_libraries ain = Array.length ain.libraries

let array_iter ?(from=0) a ~f =
  let finish = (Array.length a) - 1 in
  for i=from to finish do
    f a.(i)
  done

let array_for_all ?(from=0) a ~f =
  let finish = Array.length a in
  let rec iter i =
    if i < finish then begin
      if f a.(i) then
        iter (i + 1)
      else
        false
    end else
      true
  in
  iter from

let global_iter ?(from=0) ~f ain = array_iter ~from ain.globals ~f
let global_for_all ?(from=0) ~f ain = array_for_all ~from ain.globals ~f
let function_iter ?(from=0) ~f ain = array_iter ~from ain.functions ~f
let function_for_all ?(from=0) ~f ain = array_for_all ~from ain.functions ~f
let struct_iter ?(from=0) ~f ain = array_iter ~from ain.structures ~f
let struct_for_all ?(from=0) ~f ain = array_for_all ~from ain.structures ~f
let functype_iter ?(from=0) ~f ain = array_iter ~from ain.function_types ~f
let functype_for_all ?(from=0) ~f ain = array_for_all ~from ain.function_types ~f
let delegate_iter ?(from=0) ~f ain = array_iter ~from ain.delegates ~f
let delegate_for_all ?(from=0) ~f ain = array_for_all ~from ain.delegates ~f

exception File_error
exception Unrecognized_format
exception Invalid_format
