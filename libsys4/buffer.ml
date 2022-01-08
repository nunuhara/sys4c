(* XXX: force linking of buffer.o *)
external strangle_dune : unit -> unit = "_buffer_create"

open Ctypes
open Foreign

type t = unit ptr
let buffer_ptr : t typ = ptr void

let create initial_size =
  let create' = foreign "_buffer_create" (size_t @-> returning buffer_ptr) in
  create' (Unsigned.Size_t.of_int initial_size)

let free = foreign "_buffer_free" (buffer_ptr @-> returning void)

let clear = foreign "_buffer_clear" (buffer_ptr @-> returning void)

let write_int16 p i =
  let write_int16' = foreign "buffer_write_int16" (buffer_ptr @-> uint16_t @-> returning void) in
  write_int16' p (Unsigned.UInt16.of_int i)

let write_int32 p i =
  let write_int32' = foreign "buffer_write_int32" (buffer_ptr @-> uint32_t @-> returning void) in
  write_int32' p (Unsigned.UInt32.of_int i)

let write_int32_at p pos i =
  let write_int32_at' = foreign "buffer_write_int32_at" (buffer_ptr @-> size_t @-> uint32_t @-> returning void) in
  write_int32_at' p (Unsigned.Size_t.of_int pos) (Unsigned.UInt32.of_int i)

let write_float = foreign "buffer_write_float" (buffer_ptr @-> float @-> returning void)
