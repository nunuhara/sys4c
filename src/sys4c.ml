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
open TypeAnalysis
open ConstEval
open VariableAlloc

(*
 * AST pass to resolve user-defined types (struct/enum/function types).
 *)
class type_resolve_visitor ain = object (self)
  inherit ivisitor as super

  method resolve_type name =
    match Alice.Ain.get_struct_index ain name with
    | Some i -> Struct (name, i)
    | None ->
        begin match Alice.Ain.get_functype_index ain name with
        | Some i -> FuncType (name, i)
        | None -> failwith "Undefined type"
        end

  method resolve_typespec ts =
    match ts.data with
    | Unresolved (t) ->
        ts.data <- self#resolve_type t
    | _ -> ()

  method! visit_expression expr =
    begin match expr.node with
    | New (Unresolved (t), e, _) ->
        expr.node <- New (self#resolve_type t, e, None)
    | _ -> ()
    end;
    super#visit_expression expr

  method! visit_local_variable decl =
    self#resolve_typespec decl.type_spec;
    super#visit_local_variable decl

  method! visit_declaration decl =
    let function_class (f:fundecl) =
      match String.split_on_char '@' f.name with
      | hd :: _ ->
          begin match Alice.Ain.get_struct' ain hd with
          | -1 -> None
          | i -> Some i
          end
      | _ -> None
    in
    let resolve_function f =
      self#resolve_typespec f.return;
      List.iter (fun v -> self#resolve_typespec v.type_spec) f.params
    in
    begin match decl with
    | Function (f) ->
        resolve_function f;
        f.class_index <- function_class f
    | FuncTypeDef (f) ->
        resolve_function f
    | Global (g) ->
        self#resolve_typespec g.type_spec
    | StructDef (s) ->
        let resolve_structdecl = function
          | MemberDecl (d) ->
              self#resolve_typespec d.type_spec
          | Constructor (f)
          | Destructor (f)
          | Method (f) ->
              resolve_function f
        in
        List.iter resolve_structdecl s.decls
    | Enum (_) ->
        failwith "enum types not yet supported"
    end;
    super#visit_declaration decl
end

(*
 * AST pass over top-level declarations to define function/struct types.
 *)
class type_define_visitor ctx = object
  inherit ivisitor

  method! visit_declaration decl =
    match decl with
    | Global (g) ->
        begin match g.type_spec.qualifier with
        | Some Const ->
            ctx.const_vars <- g::ctx.const_vars
        | _ ->
            Alice.Ain.write_global ctx.ain g.name (jaf_to_ain_type g.type_spec)
        end
    | Function (f) ->
        begin match Alice.Ain.get_function ctx.ain f.name with
        | Some (obj) -> obj |> jaf_to_ain_function f |> Alice.Ain.Function.write ctx.ain
        | None -> failwith "undefined function"
        end
    | FuncTypeDef (f) ->
        begin match Alice.Ain.get_functype ctx.ain f.name with
        | Some (obj) -> obj |> jaf_to_ain_functype f |> Alice.Ain.FunctionType.write ctx.ain
        | None -> failwith "undefined functype"
        end
    | StructDef (s) ->
        begin match Alice.Ain.get_struct ctx.ain s.name with
        | Some (obj) -> obj |> jaf_to_ain_struct s |> Alice.Ain.Struct.write ctx.ain
        | None -> failwith "undefined struct"
        end
    | Enum (_) ->
        failwith "enum types not yet supported"
end

(*
 * AST pass over top-level declarations register names in the .ain file.
 *)
class type_declare_visitor ain = object
  inherit ivisitor

  method! visit_declaration decl =
    match decl with
    | Global (g) ->
        begin match g.type_spec.qualifier with
        | Some Const -> ()
        | _ ->
            if Option.is_some (Alice.Ain.get_global ain g.name) then
              failwith "duplicate global variable definition";
            ignore (Alice.Ain.add_global ain g.name)
        end
    | Function (f) ->
        if Option.is_some (Alice.Ain.get_function ain f.name) then
          failwith "duplicate function definition";
        ignore (Alice.Ain.add_function ain f.name)
    | FuncTypeDef (f) ->
        if Option.is_some (Alice.Ain.get_functype ain f.name) then
          failwith "duplicate functype definition";
        ignore (Alice.Ain.add_functype ain f.name)
    | StructDef (s) ->
        if Option.is_some (Alice.Ain.get_struct ain s.name) then
          failwith "duplicate struct definition";
        ignore (Alice.Ain.add_struct ain s.name)
    | Enum (_) ->
        failwith "enum types not yet supported"
end

let _ =
  let ctx = { ain=(Alice.Ain.create 12 0); const_vars=[] } in
  (*let p = Alice.Ain.load "in.ain" in*)
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      (* register global names in ain file *)
      (new type_declare_visitor ctx.ain)#visit_toplevel result;
      (* resolve type names *)
      (new type_resolve_visitor ctx.ain)#visit_toplevel result;
      (* define functions and structs in ain file *)
      (new type_define_visitor ctx)#visit_toplevel result;
      (* type check *)
      (new type_analyze_visitor ctx)#visit_toplevel result;
      (* evaluate constant expressions *)
      (new const_eval_visitor ctx)#visit_toplevel result;
      (* allocate function variables *)
      (new variable_alloc_visitor ctx)#visit_toplevel result;
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
  | Lexer.Eof ->
      (* FIXME: EOF should be a token handled by the parser, not an exception *)
      Alice.Ain.free ctx.ain;
      exit 0
