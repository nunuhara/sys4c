type t

val create : Ain.t -> t
val eof : t -> bool
val addr : t -> int
val jump : t -> int -> unit
val next : t -> unit
val peek : t -> Bytecode.opcode option
val opcode : t -> int
val nr_args : t -> int
val arg_type : t -> int -> Bytecode.argtype
val argument_types : t -> Bytecode.argtype list
val arg : t -> int -> int32
val arguments : t -> int32 list
