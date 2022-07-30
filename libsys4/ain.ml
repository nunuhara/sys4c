(* XXX: Work around braindamage in dune/ctypes (force linking of ain.o) *)
external strangle_linker : unit -> unit = "ain_new"
external strangle_dune : unit -> unit = "_ain_global"
external strangle_dune2 : unit -> unit = "ain_write"

open Core
open Ctypes
open Foreign
open Printf

type t = unit ptr
let ain_ptr : t typ = ptr void

let version = foreign "_ain_version" (ain_ptr @-> returning int)
let minor_version = foreign "_ain_minor_version" (ain_ptr @-> returning int)
let version_gte = foreign "_ain_version_gte" (ain_ptr @-> int @-> int @-> returning bool)
let version_lt ain major minor = not (version_gte ain major minor)

(** Bindings for `struct ain_type` objects *)
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
    (* internal compiler use *)
    | Function of int
  and t = {
    data : data;
    is_ref : bool;
  }

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
      | HLLParam    -> begin match b with HLLParam    -> true | _ -> false end
      | IFace       -> begin match b with IFace       -> true | _ -> false end
      | HLLFunc     -> begin match b with HLLFunc     -> true | _ -> false end
      | IFaceWrap   -> begin match b with IFaceWrap   -> true | _ -> false end
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
      | Function i_a ->
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
    | Function (no) -> if no >= 0 then sprintf "function<%d>" no else "function"
  and to_string o =
    let prefix = if o.is_ref then "ref " else "" in
    prefix ^ (data_to_string o.data)

  (* Convert a t to a t_c.data value *)
  (* FIXME: need to take version into account... *)
  let rec to_c_data ain o =
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
          if version_gte ain 11 0 then
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
            | Function (_) -> failwith "tried to create ref array<function>"
            end
      | Wrap (_) -> failwith "tried to create ref wrap<...>"
      | Option (_) -> failwith "tried to create ref option<...>"
      | Unknown87 (_) -> failwith "tried to create ref unknown_87"
      | IFace -> failwith "tried to create ref interface"
      | Enum2 (_) -> failwith "tried to create ref enum2"
      | Enum (_) -> failwith "tried to create ref enum"
      | HLLFunc -> failwith "tried to create ref hll_func"
      | IFaceWrap -> failwith "tried to create ref iface_wrap<...>"
      | Function (_) -> failwith "tried to create ref function"
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
            if version_gte ain 11 0 then
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
              | Function (_) -> failwith "tried to create array<function>"
              end
        | Wrap (_) -> 82
        | Option (_) -> 86
        | Unknown87 (_) -> 87
        | IFace -> 89
        | Enum2 (_) -> 91
        | Enum (_) -> 92
        | HLLFunc -> 95
        | IFaceWrap -> 100
        | Function (_) -> failwith "tried to create function"
  and to_c_struc _ o =
    match o.data with
    | Struct (no) | FuncType (no) | Delegate (no) | Enum2 (no) | Enum (no) -> no
    | _ -> -1
  and to_c_rank ain o =
    match (version_gte ain 11 0, o.data) with
    | (false, Array t) -> 1 + (to_c_rank ain t)
    | (true, Array _) -> 1
    | (_, (Wrap _ | Option _ | Unknown87 _)) -> 1
    | _ -> 0
  and to_c_array_type ain o =
    match (version_gte ain 11 0, o.data) with
    | (true, Array t) ->
        let rec type_depth = function
          | Array t | Wrap t | Option t | Unknown87 t ->
              1 + (type_depth t.data)
          | _ -> 0
        in
        let rec write_array_types src dst =
          setf !@dst data (to_c_data ain src);
          setf !@dst struc (Signed.Int32.of_int (to_c_struc ain src));
          setf !@dst rank (Signed.Int32.of_int (to_c_rank ain src));
          begin match src.data with
          | Array t | Wrap t | Option t | Unknown87 t ->
              setf !@dst array_type (dst +@ 1);
              write_array_types t (dst +@ 1)
          | _ ->
              setf !@dst array_type (from_voidp t_c null)
          end
        in
        let alloc_type = foreign "_ain_alloc_type" (int @-> returning (ptr t_c)) in
        let dst = alloc_type (1 + (type_depth t.data)) in
        write_array_types t dst;
        dst
    | _ ->
        from_voidp t_c null
  and write_ptr ain src dst =
    setf !@dst data (to_c_data ain src);
    setf !@dst struc (Signed.Int32.of_int (to_c_struc ain src));
    setf !@dst rank (Signed.Int32.of_int (to_c_rank ain src));
    setf !@dst array_type (to_c_array_type ain src)

  let make ?(is_ref=false) data =
    { data; is_ref }

  let rec of_ptr p =
    let struc = Signed.Int32.to_int (getf (!@ p) struc) in
    let rank = Signed.Int32.to_int (getf (!@ p) rank) in
    (* t constructor for (old) array types *)
    let rec make_array ?(is_ref=false) data rank =
      if rank = 0 then
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

(** Bindings for `struct ain_variable` objects *)
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
  let name2 = field t_c "name2" string_opt
  let value_type = field t_c "type" Type.t_c
  let has_initval = field t_c "has_initval" int32_t
  let initval = field t_c "initval" initval_c
  let group_index = field t_c "group_index" int32_t
  let var_type = field t_c "var_type" int
  let () = seal t_c

  type t = {
    index : int;
    name : string;
    name2 : string option;
    value_type : Type.t;
    initval : initval option;
    group_index : int;
    var_type : int
  }

  let make_local name value_type =
    { index=0; name; name2=Some ""; value_type; initval=None; group_index=0; var_type=0 }

  let make_member name value_type =
    { index=0; name; name2=Some ""; value_type; initval=None; group_index=0; var_type=1 }

  let of_ptr p i =
    { index = i;
      name = getf (!@ p) name;
      name2 = getf (!@ p) name2;
      value_type = Type.of_ptr (addr (getf (!@ p) value_type));
      initval = None; (* FIXME *)
      group_index = Signed.Int32.to_int (getf (!@ p) group_index);
      var_type = getf (!@ p) var_type
    }

  let write_ptr ain src dst =
    setf !@dst name src.name;
    setf !@dst name2 src.name2;
    Type.write_ptr ain src.value_type (addr (getf !@dst value_type));
    (* FIXME: should use Option.is_some, but dst.has_initval isn't boolean...? *)
    begin match src.initval with
    | None -> setf !@dst has_initval (Signed.Int32.of_int 0)
    | Some _ -> setf !@dst has_initval (Signed.Int32.of_int 1)
    end;
    begin match src.initval with
    | None -> ()
    | Some Int (i) -> setf (getf !@dst initval) initval_i (Signed.Int32.of_int i)
    | Some Float (f) -> setf (getf !@dst initval) initval_f f
    | Some String (s) -> setf (getf !@dst initval) initval_s s
    end;
    setf !@dst group_index (Signed.Int32.of_int src.group_index);
    setf !@dst var_type src.var_type

  let equal a b =
    (String.equal a.name b.name)
    && (Option.equal String.equal a.name2 b.name2)
    && (Type.equal a.value_type b.value_type)
    (* TODO: initval *)
    && (a.group_index = b.group_index)
    && (a.var_type = b.var_type)
end

(** Bindings for `struct ain_function` objects *)
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
    name : string;
    mutable address : int;
    mutable nr_args : int;
    mutable vars : Variable.t list;
    mutable return_type : Type.t;
    mutable is_label : bool;
    mutable is_lambda : int;
    mutable crc : int;
    mutable struct_type : int;
    mutable enum_type : int
  }

  let create name =
    { index = -1;
      name = name;
      address = 0;
      nr_args = 0;
      vars = [];
      return_type = Type.make Void;
      is_label = false;
      is_lambda = 0;
      crc = 0;
      struct_type = -1;
      enum_type = -1
    }

  (* internal to Ain module *)
  let of_ptr p i =
    let rec vars_of_ptr p n result =
      if n = 0 then
        List.rev result
      else
        vars_of_ptr (p +@ 1) (n - 1) ((Variable.of_ptr p (List.length result))::result)
    in
    { index = i;
      address = Unsigned.UInt32.to_int (getf !@p address);
      name = getf !@p name;
      nr_args = Signed.Int32.to_int (getf !@p nr_args);
      vars = vars_of_ptr (getf !@p vars) (Signed.Int32.to_int (getf !@p nr_vars)) [];
      return_type = Type.of_ptr (addr (getf !@p return_type));
      is_label = getf !@p is_label;
      is_lambda = Signed.Int32.to_int (getf !@p is_lambda);
      crc = Signed.Int32.to_int (getf !@p crc);
      struct_type = Signed.Int32.to_int (getf !@p struct_type);
      enum_type = Signed.Int32.to_int (getf !@p enum_type)
    }

  (* internal to Ain module *)
  let c_of_int = foreign "_ain_function" (ain_ptr @-> int @-> returning (ptr_opt t_c))

  (* internal to Ain module *)
  let c_of_int_checked ain i =
    match c_of_int ain i with
    | None -> failwith "_ain_function returned NULL"
    | Some obj -> obj

  (** Get a function object by index from an ain file. *)
  let of_int ain no =
    of_ptr (c_of_int_checked ain no) no

  (** Commit any changes to a function object to an ain file. *)
  let write ain f =
    let realloc_vars =
      foreign "_ain_function_realloc_vars" ((ptr t_c) @-> int @-> returning void)
    in
    let rec write_vars dst = function
      | [] -> ()
      | x::xs ->
          Variable.write_ptr ain x dst;
          write_vars (dst +@ 1) xs
    in
    let f_c = c_of_int_checked ain f.index in
    (* XXX: name is fixed upon creation and never updated *)
    setf !@f_c address (Unsigned.UInt32.of_int f.address);
    setf !@f_c is_label f.is_label;
    Type.write_ptr ain f.return_type (addr (getf !@f_c return_type));
    setf !@f_c nr_args (Signed.Int32.of_int f.nr_args);
    setf !@f_c is_lambda (Signed.Int32.of_int f.is_lambda);
    setf !@f_c crc (Signed.Int32.of_int f.crc);
    setf !@f_c struct_type (Signed.Int32.of_int f.struct_type);
    setf !@f_c enum_type (Signed.Int32.of_int f.enum_type);
    realloc_vars f_c (List.length f.vars);
    write_vars (getf !@f_c vars) f.vars

  let logical_parameters f =
    let not_void (v:Variable.t) =
      match v.value_type.data with Void -> false | _ -> true
    in
    List.filter (List.take f.vars f.nr_args) ~f:not_void

  let equal a b =
    (String.equal a.name b.name)
    && (a.nr_args = b.nr_args)
    && (List.for_all2_exn (List.take a.vars a.nr_args) (List.take b.vars b.nr_args) ~f:Variable.equal)
    && (Bool.equal a.is_label b.is_label)
    && (a.is_lambda = b.is_lambda)

  let set_undefined f =
    f.address <- 1;
    f.vars <- List.take f.vars f.nr_args

  let is_defined f =
    f.address > 1
end

(** Bindings for `struct ain_initval` objects. *)
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

(** Bindings for `struct ain_struct` objects *)
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
    mutable constructor : int;
    mutable destructor : int;
    mutable members : Variable.t list;
    vmethods : int list
  }

  (* internal to Ain module *)
  let of_ptr p i =
    let interface_of_ptr p =
      { struct_type = Signed.Int32.to_int (getf !@p struct_type);
        unknown = Signed.Int32.to_int (getf !@p unknown)
      }
    in
    let rec interfaces_of_ptr p n result =
      if n = 0 then
        List.rev result
      else
        interfaces_of_ptr (p +@ 1) (n - 1) ((interface_of_ptr p)::result)
    in
    let rec members_of_ptr p n result =
      if n = 0 then
        List.rev result
      else
        members_of_ptr (p +@ 1) (n - 1) ((Variable.of_ptr p (List.length result))::result)
    in
    { index = i;
      name = getf !@p name;
      interfaces = interfaces_of_ptr (getf !@p interfaces) (Signed.Int32.to_int (getf !@p nr_interfaces)) [];
      constructor = Signed.Int32.to_int (getf !@p constructor);
      destructor = Signed.Int32.to_int (getf !@p destructor);
      members = members_of_ptr (getf !@p members) (Signed.Int32.to_int (getf !@p nr_members)) [];
      vmethods = []
    }

  (* internal to Ain module *)
  let c_of_int = foreign "_ain_struct" (ain_ptr @-> int @-> returning (ptr_opt t_c))

  (* internal to Ain module *)
  let c_of_int_checked ain i =
    match c_of_int ain i with
    | None -> failwith "_ain_struct returned NULL"
    | Some obj -> obj

  (** Get a structure object by index from an ain file. *)
  let of_int ain no =
    of_ptr (c_of_int_checked ain no) no

  (** Commit any changes to a function object to an ain file. *)
  let write ain s =
    let realloc_members =
      foreign "_ain_struct_realloc_members" ((ptr t_c) @-> int @-> returning void)
    in
    let rec write_members dst = function
      | [] -> ()
      | x :: xs ->
          Variable.write_ptr ain x dst;
          write_members (dst +@ 1) xs
    in
    let s_c = c_of_int_checked ain s.index in
    (* XXX: name is fixed upon creation and never updated *)
    setf !@s_c constructor (Signed.Int32.of_int s.constructor);
    setf !@s_c destructor (Signed.Int32.of_int s.destructor);
    realloc_members s_c (List.length s.members);
    write_members (getf !@s_c members) s.members

  let equal a b =
    (String.equal a.name b.name)
    && ((List.length a.interfaces) = (List.length b.interfaces))
    (* TODO: interfaces? only works if structs from same ain file *)
    (* TODO: constructor & destructor? only works if structs from same ain file *)
    && ((List.length a.members) = (List.length b.members))
    && (List.for_all2_exn a.members b.members ~f:Variable.equal)
end

(** Bindings for `struct ain_library` objects (and related types). *)
module Library = struct
  module HLLArgument = struct
    type t_c
    let t_c : t_c structure typ = structure "ain_hll_argument"
    let name = field t_c "name" string
    let value_type = field t_c "type" Type.t_c
    let () = seal t_c

    type t = {
      name : string;
      value_type : Type.t
    }

    let of_ptr p =
      { name = getf!@p name;
        value_type = Type.of_ptr (addr (getf !@p value_type))
      }

    let equal a b =
      (String.equal a.name b.name) && (Type.equal a.value_type b.value_type)
  end
  module HLLFunction = struct
    type t_c
    let t_c : t_c structure typ = structure "ain_hll_function"
    let name = field t_c "name" string
    let return_type = field t_c "return_type" Type.t_c
    let nr_arguments = field t_c "nr_arguments" int32_t
    let arguments = field t_c "arguments" (ptr HLLArgument.t_c)
    let () = seal t_c

    type t = {
      index : int;
      lib_no : int;
      name : string;
      return_type : Type.t;
      arguments : HLLArgument.t list
    }

    let of_ptr p lib_no func_no =
      let rec arguments_of_ptr p n result =
        if n = 0 then
          List.rev result
        else
          arguments_of_ptr (p +@ 1) (n - 1) ((HLLArgument.of_ptr p)::result)
      in
      let nr_args = Signed.Int32.to_int (getf !@p nr_arguments) in
      { index = func_no;
        lib_no = lib_no;
        name = getf !@p name;
        return_type = Type.of_ptr (addr (getf !@p return_type));
        arguments = arguments_of_ptr (getf !@p arguments) nr_args []
      }

    let equal a b =
      (String.equal a.name b.name)
      && (Type.equal a.return_type b.return_type)
      && (List.for_all2_exn a.arguments b.arguments ~f:HLLArgument.equal)
  end

  type t_c
  let t_c : t_c structure typ = structure "ain_library"
  let name = field t_c "name" string
  let nr_functions = field t_c "nr_functions" int32_t
  let functions = field t_c "functions" (ptr HLLFunction.t_c)
  let () = seal t_c

  type t = {
    index : int;
    name : string;
    functions : HLLFunction.t list
  }

  (* internal to Ain module *)
  let of_ptr p lib_no =
    let rec functions_of_ptr p i n result =
      if n = 0 then
        List.rev result
      else
        functions_of_ptr (p +@ 1) (i + 1) (n - 1) ((HLLFunction.of_ptr p lib_no i)::result)
    in
    { index = lib_no;
      name = getf !@p name;
      functions = functions_of_ptr (getf !@p functions) 0 (Signed.Int32.to_int (getf !@p nr_functions)) []
    }

  (* internal to Ain module *)
  let c_of_int = foreign "_ain_library" (ain_ptr @-> int @-> returning (ptr_opt t_c))

  (* internal to Ain module *)
  let c_of_int_checked ain i =
    match c_of_int ain i with
    | Some obj -> obj
    | None -> failwith "_ain_library returned NULL"

  (** Get a library object by index from an ain file. *)
  let of_int ain no =
    of_ptr (c_of_int_checked ain no) no

  let equal a b =
    (String.equal a.name b.name) && (List.for_all2_exn a.functions b.functions ~f:HLLFunction.equal)
end

(** Bindings for `struct ain_switch` objects. *)
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

(** Bindings for `struct ain_function_type` objects. *)
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
    mutable return_type : Type.t;
    mutable nr_arguments : int;
    mutable variables : Variable.t list
  }

  (* internal to Ain module *)
  let of_ptr p i =
    let rec vars_of_ptr p n result =
      if n = 0 then
        List.rev result
      else
        vars_of_ptr (p +@ 1) (n - 1) ((Variable.of_ptr p (List.length result))::result)
    in
    { index = i;
      name = getf !@p name;
      return_type = Type.of_ptr (addr (getf !@p return_type));
      nr_arguments = Signed.Int32.to_int (getf !@p nr_arguments);
      variables = vars_of_ptr (getf !@p variables) (Signed.Int32.to_int (getf !@p nr_variables)) []
    }

  (* internal to Ain module *)
  let c_of_int = foreign "_ain_functype" (ain_ptr @-> int @-> returning (ptr_opt t_c))

  (* internal to Ain module *)
  let c_of_int_checked ain i =
    match c_of_int ain i with
    | None -> failwith "_ain_functype returned NULL"
    | Some obj -> obj

  (** Get a functype object by index from an ain file. *)
  let of_int ain no =
    of_ptr (c_of_int_checked ain no) no

  (** Commit any changes to a functype object to an ain file. *)
  let write ain f =
    let realloc_vars =
      foreign "_ain_functype_realloc_vars" ((ptr t_c) @-> int @->returning void)
    in
    let rec write_vars dst = function
      | [] -> ()
      | x::xs ->
          Variable.write_ptr ain x dst;
          write_vars (dst +@ 1) xs
    in
    let f_c = c_of_int_checked ain f.index in
    (* XXX: name is fixed upon creation and never updated *)
    Type.write_ptr ain f.return_type (addr (getf !@f_c return_type));
    setf !@f_c nr_arguments (Signed.Int32.of_int f.nr_arguments);
    realloc_vars f_c (List.length f.variables);
    write_vars (getf !@f_c variables) f.variables

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

(** Bindings for `struct ain_enum` objects. *)
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
  let ain_open = foreign "_ain_open" (string @-> ptr int @-> returning ain_ptr) in
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

let create = foreign "ain_new" (int @-> int @-> returning ain_ptr)
let free = foreign "ain_free" (ain_ptr @-> returning void)
let get_function' = foreign "ain_get_function" (ain_ptr @-> string @-> returning int)
let get_global' = foreign "ain_get_global" (ain_ptr @-> string @-> returning int)
let get_struct' = foreign "ain_get_struct" (ain_ptr @-> string @-> returning int)
let get_enum' = foreign "ain_get_enum" (ain_ptr @-> string @-> returning int)
let get_library' = foreign "ain_get_library" (ain_ptr @-> string @-> returning int)
let get_library_function' =
  foreign "ain_get_library_function" (ain_ptr @-> int @-> string @-> returning int)
let get_functype' = foreign "ain_get_functype" (ain_ptr @-> string @-> returning int)
let get_string_no' = foreign "ain_get_string_no" (ain_ptr @-> string @-> returning int)
let get_string = foreign "_ain_string" (ain_ptr @-> int @-> returning string_opt)
let get_message = foreign "_ain_message" (ain_ptr @-> int @-> returning string_opt)
let add_function' = foreign "ain_add_function" (ain_ptr @-> string @-> returning int)
let dup_function = foreign "ain_dup_function" (ain_ptr @-> int @-> returning int)
let add_functype' = foreign "ain_add_functype" (ain_ptr @-> string @-> returning int)
let add_global = foreign "ain_add_global" (ain_ptr @-> string @-> returning int)
let add_initval = foreign "ain_add_initval" (ain_ptr @-> int @-> returning int)
let add_struct' = foreign "ain_add_struct" (ain_ptr @-> string @-> returning int)
let add_library = foreign "ain_add_library" (ain_ptr @-> string @-> returning int)
let add_string = foreign "ain_add_string" (ain_ptr @-> string @-> returning int)
let add_message = foreign "ain_add_message" (ain_ptr @-> string @-> returning int)
let add_file = foreign "ain_add_file" (ain_ptr @-> string @-> returning int)

let return_option i =
  if i < 0 then None else Some i

let get_enum p name = get_enum' p name |> return_option
let get_library_index p name = get_library' p name |> return_option
let get_library_function_index p i name = get_library_function' p i name |> return_option
let get_string_no p s = get_string_no' p s |> return_option

let get_functype_index p name = get_functype' p name |> return_option
let get_struct_index p name = get_struct' p name |> return_option

let get_c_global_by_index p i =
  let ain_global = foreign "_ain_global" (ain_ptr @-> int @-> returning (ptr_opt (Variable.t_c))) in
  match ain_global p i with
  | None -> failwith "_ain_global returned NULL"
  | Some obj -> obj

let get_global_by_index p i =
  Variable.of_ptr (get_c_global_by_index p i) i

let get_global p name =
  match get_global' p name with
  | -1 -> None
  | i  -> Some (get_global_by_index p i)

let write_global p name t =
  match get_global' p name with
  | -1 -> failwith "global not defined in ain file"
  | i ->
      let g = get_c_global_by_index p i in
      Type.write_ptr p t (addr (getf (!@ g) Variable.value_type))

let write_new_global p (g:Variable.t) =
  let no = add_global p g.name in
  let c_g = get_c_global_by_index p no in
  Type.write_ptr p g.value_type (addr (getf (!@ c_g) Variable.value_type));
  no

let get_function_by_index p no =
  match Function.c_of_int p no with
  | None -> failwith "_ain_function returned NULL"
  | Some obj -> Function.of_ptr obj no

let get_function p name =
  match get_function' p name with
  | -1 -> None
  | i -> Some (Function.of_int p i)

let add_function p name =
  let no = add_function' p name in
  match Function.c_of_int p no with
  | None -> failwith "_ain_function returned NULL"
  | Some obj -> Function.of_ptr obj no

let write_new_function p (f:Function.t) =
  let no = add_function' p f.name in
  Function.write p {f with index=no};
  no

let get_struct_by_index p no =
  match Struct.c_of_int p no with
  | None -> failwith "_ain_struct returned NULL"
  | Some obj -> Struct.of_ptr obj no

let get_struct p name =
  match get_struct' p name with
  | -1 -> None
  | i -> Some (Struct.of_int p i)

let add_struct p name =
  get_struct_by_index p (add_struct' p name)

let write_new_struct p (s:Struct.t) =
  let no = add_struct' p s.name in
  Struct.write p {s with index=no};
  no

let get_functype p name =
  match get_functype' p name with
  | -1 -> None
  | i -> Some (FunctionType.of_int p i)

let function_of_functype_index ain no =
    let ft = FunctionType.of_int ain no in
    let (r:Function.t) =
      { index = no;
        address = 0;
        name = ft.name;
        nr_args = ft.nr_arguments;
        vars = ft.variables;
        return_type = ft.return_type;
        is_label = false;
        is_lambda = 0;
        crc = 0;
        struct_type = -1;
        enum_type = -1
      }
    in
    r

let add_functype p name =
  FunctionType.of_int p (add_functype' p name)

let write_new_functype p (ft:FunctionType.t) =
  let no = add_functype' p ft.name in
  FunctionType.write p {ft with index=no};
  no

let function_of_hll_function_index ain lib_no fun_no =
  let get_fun = foreign "_ain_library_function" (ain_ptr @-> int @-> int @-> returning (ptr_opt (Library.HLLFunction.t_c))) in
  match get_fun ain lib_no fun_no with
  | Some p ->
      let var_of_hll_arg (arg:Library.HLLArgument.t) =
        let (r:Variable.t) =
          { index = 0;
            name = arg.name;
            name2 = Some "";
            value_type = arg.value_type;
            initval = None;
            group_index = 0;
            var_type = 0
          }
        in
        r
      in
      let f = Library.HLLFunction.of_ptr p lib_no fun_no in
      let (r:Function.t) =
        { index = fun_no;
          address = 0;
          name = f.name;
          nr_args = List.length f.arguments;
          vars = List.map f.arguments ~f:var_of_hll_arg ;
          return_type = f.return_type;
          is_label = false;
          is_lambda = 0;
          crc = 0;
          struct_type = -1;
          enum_type = -1
        }
      in
      r
  | None ->
      failwith "_ain_library_function returned NULL"

let append_bytecode = foreign "_ain_append_bytecode" (ain_ptr @-> CBuffer.buffer_ptr @-> returning void)

let code_size = foreign "_ain_code_size" (ain_ptr @-> returning int)

let set_main_function = foreign "_ain_set_main_function" (ain_ptr @-> int @-> returning void)
let set_message_function = foreign "_ain_set_message_function" (ain_ptr @-> int @-> returning void)

let write p filename =
  let write' = foreign "ain_write" (string @-> ain_ptr @-> returning void) in
  write' filename p

let nr_globals = foreign "_ain_nr_globals" (ain_ptr @-> returning int)
let nr_functions = foreign "_ain_nr_functions" (ain_ptr @-> returning int)
let nr_structs = foreign "_ain_nr_structures" (ain_ptr @-> returning int)
let nr_functypes = foreign "_ain_nr_functypes" (ain_ptr @-> returning int)
let nr_libraries = foreign "_ain_nr_libraries" (ain_ptr @-> returning int)

let global_iter' from iter p =
  let globals = (foreign "_ain_globals" (ain_ptr @-> returning (ptr Variable.t_c))) p in
  iter (globals +@ from) ((nr_globals p) - from) from

let global_iter ?(from = 0) ~f p =
  let rec iter g n i =
    if n <= 0 then
      ()
    else begin
      f (Variable.of_ptr g i);
      iter (g +@ 1) (n - 1) (i + 1)
    end
  in
  global_iter' from iter p

let global_for_all ?(from = 0) ~f p =
  let rec iter g n i =
    if n <= 0 then
      true
    else begin
      if f (Variable.of_ptr g i) then
        iter (g +@ 1) (n - 1) (i + 1)
      else
        false
    end
  in
  global_iter' from iter p

let function_iter' from iter p =
  let functions = (foreign "_ain_functions" (ain_ptr @-> returning (ptr Function.t_c))) p in
  iter (functions +@ from) ((nr_functions p) - from) from

let function_iter ?(from = 0) ~f p =
  let rec iter c_f n i =
    if n <= 0 then
      ()
    else begin
      f (Function.of_ptr c_f i);
      iter (c_f +@ 1) (n - 1) (i + 1)
    end
  in
  function_iter' from iter p

let function_for_all ?(from = 0) ~f p =
  let rec iter c_f n i =
    if n <= 0 then
      true
    else begin
      if f (Function.of_ptr c_f i) then
        iter (c_f +@ 1) (n - 1) (i + 1)
      else
        false
    end
  in
  function_iter' from iter p

let struct_iter' from iter p =
  let structs = (foreign "_ain_structures" (ain_ptr @-> returning (ptr Struct.t_c))) p in
  iter (structs +@ from) ((nr_structs p) - from) from

let struct_iter ?(from = 0) ~f p =
  let rec iter s n i =
    if n <= 0 then
      ()
    else begin
      f (Struct.of_ptr s i);
      iter (s +@ 1) (n - 1) (i + 1)
    end
  in
  struct_iter' from iter p

let struct_for_all ?(from = 0) ~f p =
  let rec iter s n i =
    if n <= 0 then
      true
    else begin
      if f (Struct.of_ptr s i) then
        iter (s +@ 1) (n - 1) (i + 1)
      else
        false
    end
  in
  struct_iter' from iter p

let functype_iter' from iter p =
  let functypes = (foreign "_ain_functypes" (ain_ptr @-> returning (ptr FunctionType.t_c))) p in
  iter (functypes +@ from) ((nr_functypes p) - from) from

let functype_iter ?(from = 0) ~f p =
  let rec iter ft n i =
    if n <= 0 then
      ()
    else begin
      f (FunctionType.of_ptr ft i);
      iter (ft +@ 1) (n - 1) (i + 1)
    end
  in
  functype_iter' from iter p

let functype_for_all ?(from = 0) ~f p =
  let rec iter ft n i =
    if n <= 0 then
      true
    else begin
      if f (FunctionType.of_ptr ft i) then
        iter (ft +@ 1) (n - 1) (i + 1)
      else
        false
    end
  in
  functype_iter' from iter p

module DASM = struct
  type t = unit ptr
  let dasm_ptr : t typ = ptr void

  let create = foreign "dasm_open" (ain_ptr @-> returning dasm_ptr)
  let destroy = foreign "dasm_close" (dasm_ptr @-> returning void)
  let eof = foreign "dasm_eof" (dasm_ptr @-> returning bool)
  let addr = foreign "dasm_addr" (dasm_ptr @-> returning int)
  let jump = foreign "dasm_jump" (dasm_ptr @-> int @-> returning void)
  let next = foreign "dasm_next" (dasm_ptr @-> returning void)
  let peek = foreign "dasm_peek" (dasm_ptr @-> returning int)
  let opcode = foreign "dasm_opcode" (dasm_ptr @-> returning int)
  let nr_args = foreign "dasm_nr_args" (dasm_ptr @-> returning int)
  let arg = foreign "dasm_arg" (dasm_ptr @-> int @-> returning int)
  let arg_type = foreign "dasm_arg_type" (dasm_ptr @-> int @-> returning int)

  let arguments dasm =
    List.map (List.init (nr_args dasm) ~f:(~+)) ~f:(arg dasm)

  let argument_types dasm =
    List.map (List.init (nr_args dasm) ~f:(~+)) ~f:(arg_type dasm)
end

let foreach_instruction ~f p =
  let dasm = DASM.create p in
  while not (DASM.eof dasm) do
    f dasm;
    DASM.next dasm
  done;
  DASM.destroy dasm
