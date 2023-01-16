open Core

type t = {
  mutable buf : bytes;
  mutable len : int;
  mutable pos : int;
  growth_add : int;
}

let create ?(growth_add=256) initial_size =
  { buf = Bytes.create initial_size;
    len = initial_size;
    pos = 0;
    growth_add;
  }

let grow buf =
  let len = buf.len + buf.growth_add in
  let dst = Bytes.create len in
  Bytes.blit ~src:buf.buf ~src_pos:0 ~dst ~dst_pos:0 ~len:buf.pos;
  buf.buf <- dst;
  buf.len <- len

let clear buf =
  buf.pos <- 0

let write_int16 buf v =
  if (buf.len - buf.pos) < 2 then
    grow buf;
  Stdlib.Bytes.set_int16_le buf.buf buf.pos v;
  buf.pos <- buf.pos + 2

let write_int32 buf v =
  if (buf.len - buf.pos) < 4 then
    grow buf;
  Stdlib.Bytes.set_int32_le buf.buf buf.pos (Int32.of_int_trunc v);
  buf.pos <- buf.pos + 4

let write_int32_at buf pos v =
  if (pos + 4) > buf.pos then
    failwith "out of bounds buffer write";
  Stdlib.Bytes.set_int32_le buf.buf pos (Int32.of_int_trunc v)

let write_float buf v =
  if (buf.len - buf.pos) < 4 then
    grow buf;
  Stdlib.Bytes.set_int32_le buf.buf buf.pos (Int32.bits_of_float v);
  buf.pos <- buf.pos + 4
