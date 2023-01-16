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
open Jaf

exception Type_error of Ain.Type.t * expression option * ast_node
exception Undefined_variable of string * ast_node
exception Arity_error of Ain.Function.t * expression list * ast_node
exception Not_lvalue_error of expression * ast_node
exception Const_error of variable
exception CompileError of string * ast_node
exception CompilerBug of string * ast_node option
exception LinkError of string
exception LinkerBug of string

let data_type_error data expr parent =
  raise (Type_error ({data=data; is_ref=false}, expr, parent))

let ref_type_error data expr parent =
  raise (Type_error ({data=data; is_ref=true}, expr, parent))

let undefined_variable_error name parent =
  raise (Undefined_variable (name, parent))

let arity_error t args parent =
  raise (Arity_error (t, args, parent))

let not_an_lvalue_error expr parent =
  raise (Not_lvalue_error (expr, parent))

let const_error v =
  raise (Const_error (v))

let compile_error str node =
  raise (CompileError (str, node))

let compiler_bug str node =
  raise (CompilerBug (str, node))

let link_error str =
  raise (LinkError (str))

let linker_bug str =
  raise (LinkerBug (str))
