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

class sanity_check_visitor = object
  inherit ivisitor as super

  method! visit_expression expr =
    super#visit_expression expr;
    begin match expr.valuetype with
    | Some t ->
        if t.is_ref then
          failwith "expression has ref type"
    | None ->
        failwith "expression has no type"
    end;
    begin match expr.node with
    | Ident (_, None) ->
        failwith "identifier expression has no ident_type"
    | Ident (_, Some GlobalConstant) ->
        failwith "global constant not eliminated"
    | _ -> ()
    end

  method! visit_local_variable v =
    super#visit_local_variable v;
    match v.index with
    | Some _ -> ()
    | None -> failwith "local variable index not set"

  method! visit_fundecl f =
    super#visit_fundecl f;
    match f.index with
    | Some _ -> ()
    | None -> failwith "function index not set"

  method! visit_declaration d =
    super#visit_declaration d;
    match d with
    | Global g ->
        begin match g.index with
        | Some _ -> ()
        | None -> failwith "global variable index not set"
        end
    | _ -> ()
end
