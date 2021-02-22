(* XXX: Work around braindamage in dune/ctypes (force linking of ain.o) *)
external strangle_linker : unit -> unit = "ain_new"

open Ctypes
open Foreign

type ain = unit ptr
let ain : ain typ = ptr void

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
let get_global p name = get_global' p name |> return_option
let get_struct p name = get_struct' p name |> return_option
let get_enum p name = get_enum' p name |> return_option
let get_library p name = get_library' p name |> return_option
let get_library_function p i name = get_library_function' p i name |> return_option
let get_functype p name = get_functype' p name |> return_option
let get_string_no p s = get_string_no' p s |> return_option
