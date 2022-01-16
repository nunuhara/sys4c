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
open CompileError

(*
 * AST pass over top-level declarations register names in the .ain file.
 *)
class type_declare_visitor ain = object (self)
  inherit ivisitor

  method declare_function decl =
    match (decl.return.qualifier, (Alice.Ain.get_function ain decl.name)) with
    | (Some Override, None) ->
        compile_error "function doesn't exist for override" (ASTDeclaration(Function decl))
    | (Some Override, Some super_f) ->
        decl.index <- Some super_f.index;
        decl.super_index <- Some (Alice.Ain.dup_function ain super_f.index)
    | (_, Some _) ->
        compile_error "Duplicate function definition" (ASTDeclaration(Function decl))
    | (_, None) ->
        decl.index <- Some (Alice.Ain.add_function ain decl.name).index

  method! visit_declaration decl =
    match decl with
    | Global (g) ->
        begin match g.type_spec.qualifier with
        | Some Const -> ()
        | _ ->
            if Option.is_some (Alice.Ain.get_global ain g.name) then
              compile_error "duplicate global variable definition" (ASTDeclaration decl);
            g.index <- Some (Alice.Ain.add_global ain g.name)
        end
    | Function (f) ->
        self#declare_function f
    | FuncTypeDef (f) ->
        if Option.is_some (Alice.Ain.get_functype ain f.name) then
          compile_error "duplicate functype definition" (ASTDeclaration decl);
        ignore (Alice.Ain.add_functype ain f.name)
    | StructDef (s) ->
        if Option.is_some (Alice.Ain.get_struct ain s.name) then
          compile_error "duplicate struct definition" (ASTDeclaration decl);
        let ain_s = Alice.Ain.add_struct ain s.name in
        let visit_decl = function
          | Constructor f ->
              f.name <- s.name ^ "@0";
              f.class_index <- Some ain_s.index;
              self#declare_function f
          | Destructor f ->
              f.name <- s.name ^ "@1";
              f.class_index <- Some ain_s.index;
              self#declare_function f
          | Method f ->
              f.name <- s.name ^ "@" ^ f.name;
              f.class_index <- Some ain_s.index;
              self#declare_function f
          | MemberDecl _ -> ()
        in
        List.iter s.decls ~f:visit_decl
    | Enum (_) ->
        compile_error "enum types not yet supported" (ASTDeclaration decl)
end

let register_type_declarations ctx decls =
  (new type_declare_visitor ctx.ain)#visit_toplevel decls

(*
 * AST pass to resolve user-defined types (struct/enum/function types).
 *)
class type_resolve_visitor ain = object (self)
  inherit ivisitor as super

  method resolve_type name node =
    match Alice.Ain.get_struct_index ain name with
    | Some i -> Struct (name, i)
    | None ->
        begin match Alice.Ain.get_functype_index ain name with
        | Some i -> FuncType (name, i)
        | None -> compile_error ("Undefined type: " ^ name) node
        end

  method resolve_typespec ts node =
    match ts.data with
    | Unresolved (t) ->
        ts.data <- self#resolve_type t node
    | _ -> ()

  method! visit_expression expr =
    begin match expr.node with
    | New (Unresolved (t), e, _) ->
        expr.node <- New (self#resolve_type t (ASTExpression expr), e, None)
    | _ -> ()
    end;
    super#visit_expression expr

  method! visit_local_variable decl =
    self#resolve_typespec decl.type_spec (ASTVariable decl);
    super#visit_local_variable decl

  method! visit_declaration decl =
    let function_class (f:fundecl) =
      match String.split_on_chars f.name ~on:['@'] with
      | hd :: _ ->
          begin match Alice.Ain.get_struct' ain hd with
          | -1 -> None
          | i -> Some i
          end
      | _ -> None
    in
    let resolve_function f =
      self#resolve_typespec f.return (ASTDeclaration(Function f));
      List.iter f.params ~f:(fun v -> self#resolve_typespec v.type_spec (ASTVariable v))
    in
    begin match decl with
    | Function (f) ->
        resolve_function f;
        f.class_index <- function_class f
    | FuncTypeDef (f) ->
        resolve_function f
    | Global (g) ->
        self#resolve_typespec g.type_spec (ASTDeclaration decl)
    | StructDef (s) ->
        let resolve_structdecl = function
          | MemberDecl (d) ->
              self#resolve_typespec d.type_spec (ASTDeclaration decl)
          | Constructor (f)
          | Destructor (f)
          | Method (f) ->
              resolve_function f
        in
        List.iter s.decls ~f:resolve_structdecl
    | Enum (_) ->
        compile_error "enum types not yet supported" (ASTDeclaration decl)
    end;
    super#visit_declaration decl
end

let resolve_types ctx decls =
  (new type_resolve_visitor ctx.ain)#visit_toplevel decls

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
        | None -> compiler_bug "undefined function" (Some(ASTDeclaration decl))
        end
    | FuncTypeDef (f) ->
        begin match Alice.Ain.get_functype ctx.ain f.name with
        | Some (obj) -> obj |> jaf_to_ain_functype f |> Alice.Ain.FunctionType.write ctx.ain
        | None -> compiler_bug "undefined functype" (Some(ASTDeclaration decl))
        end
    | StructDef (s) ->
        begin match Alice.Ain.get_struct ctx.ain s.name with
        | Some (obj) -> obj |> jaf_to_ain_struct s |> Alice.Ain.Struct.write ctx.ain
        | None -> compiler_bug "undefined struct" (Some(ASTDeclaration decl))
        end
    | Enum (_) ->
        compile_error "Enum types not yet supported" (ASTDeclaration decl)
end

let define_types ctx decls =
  (new type_define_visitor ctx)#visit_toplevel decls
