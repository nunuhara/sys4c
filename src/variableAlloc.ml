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

class variable_alloc_visitor ctx = object (self)
  inherit ivisitor as super

  val mutable vars : variable list = []

  method get_var_no name =
    let rec search i (vars : variable list) =
      match vars with
      | hd::tl ->
          if String.equal hd.name name then i else search (i + 1) tl
      | [] ->
          compiler_bug ("Undefined variable: " ^ name) None
    in
    search 0 (List.rev vars)

  method add_var (v:variable) =
    let i = List.length vars in
    v.index <- Some i;
    begin match v.type_spec with
    | { data=(Int|Bool|Float|FuncType (_, _)); qualifier=Some Ref } ->
        let void = {
          name = "<void>";
          array_dim = [];
          type_spec = { data=Void; qualifier=None };
          initval = None;
          index = Some (i + 1)
        }
        in
        vars <- void::v::vars
    | _ ->
        vars <- v::vars
    end

  method! visit_expression expr =
    super#visit_expression expr;
    begin match expr.node with
    | Ident (name, t) ->
        (* save local variable number at identifier nodes *)
        begin match t with
        | Some LocalVariable _ ->
            expr.node <- Ident (name, Some (LocalVariable (self#get_var_no name)))
        | _ -> ()
        end
    | New (t, args, _) ->
        (* create dummy ref variable to store object for extent of new expression *)
        let struct_name =
          match t with
          | Struct (name, _) -> name
          | _ -> compiler_bug "Non-struct type in new expression" (Some(ASTExpression expr))
        in
        let v = {
          name = "<dummy : new " ^ struct_name ^ ">";
          array_dim = [];
          type_spec = { data = t; qualifier = Some Ref };
          initval = None;
          index = Some (List.length vars)
        }
        in
        expr.node <- New (t, args, Some (Option.value_exn v.index));
        vars <- v::vars
    | _ -> ()
    end

  method! visit_local_variable v =
    self#add_var v;
    super#visit_local_variable v

  method! visit_fundecl f =
    let conv_var (v:variable) =
      Alice.Ain.Variable.make_local v.name (jaf_to_ain_type v.type_spec)
    in
    let add_vars (a_f : Alice.Ain.Function.t) =
      a_f.vars <- List.map (List.rev vars) ~f:conv_var;
      a_f
    in
    (* add params to var list *)
    List.iter f.params ~f:self#add_var;
    super#visit_fundecl f;
    (* write updated fundecl to ain file *)
    begin match Alice.Ain.get_function ctx.ain f.name with
    | Some (obj) -> obj |> jaf_to_ain_function f |> add_vars |> Alice.Ain.Function.write ctx.ain
    | None -> compiler_bug "Undefined function" (Some(ASTDeclaration(Function f)))
    end;
    vars <- []
end

let allocate_variables ctx decls =
  (new variable_alloc_visitor ctx)#visit_toplevel decls
