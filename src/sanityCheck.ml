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

open Jaf
open CompileError

class sanity_check_visitor ctx = object
  inherit ivisitor ctx as super

  method! visit_expression expr =
    super#visit_expression expr;
    begin match expr.valuetype with
    | Some t ->
        if t.is_ref then
          begin match t.data with
          | Function _ -> ()
          | _ -> compiler_bug "expression has ref type" (Some(ASTExpression expr))
          end
    | None ->
        compiler_bug "expression has no type" (Some(ASTExpression expr))
    end;
    begin match expr.node with
    | Ident (_, None) ->
        compiler_bug "identifier expression has no ident_type" (Some(ASTExpression expr))
    | Ident (_, Some GlobalConstant) ->
        compiler_bug "global constant not eliminated" (Some(ASTExpression expr))
    | Member (_, _, None) ->
        compiler_bug "member expression has no member_type" (Some(ASTExpression expr))
    | Call (_, _, None) ->
        compiler_bug "call expression has no call_type" (Some(ASTExpression expr))
    | _ -> ()
    end

  method! visit_local_variable v =
    super#visit_local_variable v;
    match v.index with
    | Some _ -> ()
    | None -> compiler_bug "local variable index not set" (Some(ASTVariable v))

  method! visit_fundecl f =
    super#visit_fundecl f;
    match f.index with
    | Some _ -> ()
    | None -> compiler_bug "function index not set" (Some(ASTDeclaration(Function f)))

  method! visit_declaration d =
    super#visit_declaration d;
    match d with
    | Global g ->
        begin match g.index, g.type_spec.qualifier with
        | Some _, _ | None, Some Const -> ()
        | None, _ -> compiler_bug "global variable index not set" (Some(ASTDeclaration(Global g)))
        end
    | _ -> ()
end

let check_invariants ctx decls =
  (new sanity_check_visitor ctx)#visit_toplevel decls
