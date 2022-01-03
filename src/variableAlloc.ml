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

class variable_alloc_visitor ctx = object (self)
  inherit ivisitor as super

  val mutable vars : variable list = []

  method get_var_no name =
    let rec search i (vars : variable list) =
      match vars with
      | hd::tl ->
          if hd.name = name then i else search (i + 1) tl
      | [] ->
          failwith "undefined variable"
    in
    search 0 vars

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
          | _ -> failwith "non-struct type in new expression"
        in
        let v = {
          name = "<dummy : new " ^ struct_name ^ ">";
          array_dim = [];
          type_spec = { data = t; qualifier = Some Ref };
          initval = None
        }
        in
        expr.node <- New (t, args, Some (List.length vars));
        vars <- v::vars
    | _ -> ()
    end

  method! visit_local_variable v =
    (* add local to var list *)
    vars <- v::vars;
    super#visit_local_variable v

  method! visit_fundecl f =
    let conv_var (v:variable) =
      Alice.Ain.Variable.make_local v.name (jaf_to_ain_type v.type_spec)
    in
    let add_vars (a_f : Alice.Ain.Function.t) =
      a_f.vars <- List.map conv_var (List.rev vars);
      a_f
    in
    (* add params to var list *)
    List.iter (fun v -> vars <- v::vars) f.params;
    super#visit_fundecl f;
    (* write updated fundecl to ain file *)
    begin match Alice.Ain.get_function ctx.ain f.name with
    | Some (obj) -> obj |> jaf_to_ain_function f |> add_vars |> Alice.Ain.Function.write ctx.ain
    | None -> failwith "undefined function"
    end
end
