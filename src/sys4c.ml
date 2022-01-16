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

open Printf
open Jaf
open Error

let _ =
  let ctx = { ain=(Alice.Ain.create 12 0); const_vars=[] } in
  (*let ctx = { ain=(Alice.Ain.load "in.ain"); const_vars = [] } in*)
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      Declarations.register_type_declarations ctx result;
      Declarations.resolve_types ctx result;
      Declarations.define_types ctx result;
      TypeAnalysis.check_types ctx result;
      ConstEval.evaluate_constant_expressions ctx result;
      VariableAlloc.allocate_variables ctx result;
      SanityCheck.check_invariants result; (* TODO: disable in release builds *)
      Compiler.compile ctx result;
      print_string "-> ";
      List.iter (fun d -> print_string (decl_to_string d)) result;
      print_newline();
      flush stdout
    done
  with
  | Type_error (expected, actual, parent) ->
      let s_expected = Alice.Ain.Type.to_string expected in
      let s_actual =
        match actual with
        | None -> "void"
        | Some expr ->
            match expr.valuetype with
            | None -> "untyped"
            | Some t -> Alice.Ain.Type.to_string t
      in
      printf "Error: Type error: expected %s; got %s\n" s_expected s_actual;
      Option.iter (fun e -> printf "\tat: %s\n" (expr_to_string e)) actual;
      printf "\tin: %s\n" (ast_to_string parent);
      Alice.Ain.free ctx.ain;
      exit 1
  | Undefined_variable (name, _) ->
      printf "Error: Undefined variable: %s\n" name;
      Alice.Ain.free ctx.ain;
      exit 1
  | Arity_error (f, args, parent) ->
      printf "Error: wrong number of arguments to function %s (expected %d; got %d)\n" f.name f.nr_args (List.length args);
      printf "\tin: %s\n" (ast_to_string parent);
      Alice.Ain.free ctx.ain;
      exit 1
  | Not_lvalue_error (expr, parent) ->
      printf "Error: not an lvalue: %s\n" (expr_to_string expr);
      printf "\tin: %s\n" (ast_to_string parent);
      Alice.Ain.free ctx.ain;
      exit 1
  | Const_error (var) ->
      begin match var.initval with
      | Some _ -> printf "Error: value of const variable is not constant\n"
      | None   -> printf "Error: const variable lacks initializer\n"
      end;
      printf "\tin: %s\n" (var_to_string var);
      Alice.Ain.free ctx.ain;
      exit 1
  | CompileError (msg, node) ->
      printf "Error: %s\n" msg;
      printf "\tin: %s\n" (ast_to_string node);
      Alice.Ain.free ctx.ain;
      exit 1
  | CompilerBug (msg, node) ->
      printf "Error: %s\n" msg;
      begin match node with
      | Some n -> printf "\tin: %s\n" (ast_to_string n)
      | None -> ()
      end;
      printf "(This is a compiler bug!)";
      Alice.Ain.free ctx.ain;
      exit 1
  | Lexer.Eof ->
      Alice.Ain.write ctx.ain "out.ain";
      (* FIXME: EOF should be a token handled by the parser, not an exception *)
      Alice.Ain.free ctx.ain;
      exit 0
