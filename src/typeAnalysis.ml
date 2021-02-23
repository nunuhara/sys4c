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

exception Type_error of data_type * expression option * ast_node
exception Undefined_variable of string * ast_node

let rec type_equal expected actual =
  match expected with
  | Untyped -> failwith "type checker expected untyped value"
  | Unresolved (_) -> failwith "type checker expected unresolved type"
  | Void -> failwith "type checker expected void type"
  | Int ->
      begin match actual with
      | Int | Bool -> true
      | _ -> false
      end
  | Bool -> 
      begin match actual with
      | Int | Bool -> true
      | _ -> false
      end
  | Float ->
      begin match actual with
      | Float -> true
      | _ -> false
      end
  | String ->
      begin match actual with
      | String -> true
      | _ -> false
      end
  | Struct (_, i) ->
      begin match actual with
      | Struct (_, i_a) -> i == i_a
      | _ -> false
      end
  | Array (t, rank) ->
      begin match actual with
      | Array (t_a, rank_a) -> (type_equal t.data t_a.data) && (rank == rank_a)
      | _ -> false
      end
  | Wrap (t) ->
      begin match actual with
      | Wrap (t_a) -> type_equal t.data t_a.data
      | _ -> false
      end
  | HLLParam ->
      begin match actual with
      | HLLParam -> true
      | _ -> false
      end
  | HLLFunc ->
      begin match actual with
      | HLLFunc -> true
      | _ -> false
      end
  | Delegate ->
      begin match actual with
      | Delegate -> true
      | _ -> false
      end

let type_castable dst src =
  match dst with
  | Untyped -> failwith "type checker cast to untyped value"
  | Unresolved (_) -> failwith "type checker cast to unresolved type"
  (* FIXME: cast to void should be allowed *)
  | Void -> failwith "type checker cast to void type"
  | Int | Bool | Float ->
      begin match src with
      | Int | Bool | Float -> true
      | _ -> false
      end
  | _ -> false

let type_check parent expected actual =
  if not (type_equal expected actual.valuetype.data) then
    raise (Type_error (expected, Some actual, parent))

let type_check_numeric parent actual =
  match actual.valuetype.data with
  | Int | Bool | Float -> ()
  | _ -> raise (Type_error (Int, Some actual, parent))

class type_analyze_visitor ain = object (self)
  inherit ivisitor as super

  (*
   * Lexical environment stack. push/pop should be called when entering and
   * exiting a scope.
   *)
  val env = object
    val mutable stack = []
    val mutable variables = []

    method push =
      stack <- variables :: stack

    method pop =
      match stack with
      | [] ->
          failwith "tried to pop root environment"
      | prev::rest ->
          variables <- prev;
          stack <- rest

    method push_var decl =
      variables <- decl :: variables

    method get name =
      let var_eq (v : variable) = String.equal v.name name in
      let rec search vars rest =
        match List.find_opt var_eq vars with
        | Some v -> Some v
        | None ->
            begin match rest with
            | [] -> None
            | prev::rest -> search prev rest
            end
      in
      search variables stack
  end

  (* XXX: needed for return type check *)
  val mutable current_function = None

  method! visit_expression expr =
    super#visit_expression expr;
    let check = type_check (ASTExpression (expr)) in
    let check_numeric = type_check_numeric (ASTExpression (expr)) in
    match expr.node with
    | ConstInt (_) ->
        expr.valuetype <- { data=Int; qualifier=None }
    | ConstFloat (_) ->
        expr.valuetype <- { data=Float; qualifier=None }
    | ConstChar (_) -> ()
    | ConstString (_) ->
        expr.valuetype <- { data=String; qualifier=None }
    | Ident (name) ->
        begin match env#get name with
        | None ->
            begin match Alice.Ain.get_global ain name with
            | None -> raise (Undefined_variable (name, ASTExpression (expr)))
            | Some _ -> failwith "global variables not yet supported"
            end
        | Some v ->
            expr.valuetype <- { data=v.type_spec.data; qualifier=None }
        end
    | Unary (op, e) ->
        begin match op with
        | UPlus | UMinus | PreInc | PreDec | PostInc | PostDec ->
            check_numeric e
        | LogNot | BitNot ->
            check Int e
        | AddrOf ->
            failwith "function types not yet supported"
        end;
        expr.valuetype <- { data=e.valuetype.data; qualifier=None }
    | Binary (op, a, b) ->
        begin match op with
        | Plus | Minus | Times | Divide | LT | GT | LTE | GTE ->
            check_numeric a;
            check_numeric b;
            (* TODO: allow coercion *)
            check a.valuetype.data b
        | Modulo | LogOr | LogAnd | BitOr | BitXor | BitAnd | LShift | RShift ->
            check Int a;
            check Int b
        | Equal | NEqual ->
            begin match a.valuetype.data with
            | String ->
                check String b
            | _ ->
                check_numeric a;
                check_numeric b;
                (* TODO: allow coercion *)
                check a.valuetype.data b
            end
        end;
        expr.valuetype <- { data=a.valuetype.data; qualifier=None }
    | Assign (op, lhs, rhs) ->
        begin match op with
        | EqAssign ->
            check lhs.valuetype.data rhs
        | PlusAssign | MinusAssign | TimesAssign | DivideAssign ->
            check_numeric lhs;
            check_numeric rhs;
            (* TODO: allow coercion *)
            check lhs.valuetype.data rhs
        | ModuloAssign | OrAssign | XorAssign | AndAssign
        | LShiftAssign | RShiftAssign ->
            check Int lhs;
            check Int rhs
        end;
        expr.valuetype <- { data=lhs.valuetype.data; qualifier=None }
    | Seq (_, e) ->
        expr.valuetype <- { data=e.valuetype.data; qualifier=None }
    | Ternary (test, con, alt) ->
        check Int test;
        check con.valuetype.data alt;
        expr.valuetype <- { data=con.valuetype.data; qualifier=None }
    | Cast (t, e) ->
        if not (type_castable t e.valuetype.data) then
          raise (Type_error (t, Some e, ASTExpression (expr)));
        expr.valuetype <- { data=t; qualifier=None }
    | Subscript (obj, i) ->
        check Int i;
        begin match obj.valuetype.data with
        | Array (t, rank) ->
            if rank == 1 then
              expr.valuetype <- { data=t.data; qualifier=None }
            else
              expr.valuetype <- { data=Array(t, rank - 1); qualifier=None }
        | _ ->
            let array_type = { data=Unresolved ("?"); qualifier=None } in
            let expected = Array (array_type, 1) in
            raise (Type_error (expected, Some obj, ASTExpression (expr)))
        end
    | Member (_, _) ->
        failwith "struct members not yet supported"
    | Call (_, _) ->
        failwith "function calls not yet supported"
    | New (_, _) ->
        failwith "'new' operator not yet supported"
    | This ->
        failwith "'this' keyword not yet supported"

  method! visit_statement stmt =
    (* Create new scope if needed *)
    begin match stmt.node with
    | Compound (_) ->
        env#push
    | For (Declarations (decls), _, _, _) ->
        env#push;
        List.iter env#push_var decls
    | _ -> ()
    end;
    super#visit_statement stmt;
    match stmt.node with
    | EmptyStatement -> ()
    | Expression (_) -> ()
    | Compound (_) ->
        env#pop
    | Labeled (_, _) -> ()
    | If (test, _, _) | While (test, _) | DoWhile (test, _) ->
        type_check (ASTStatement (stmt)) Int test
    | For (_, test, _, _) ->
        env#pop;
        type_check (ASTStatement (stmt)) Int test
    | Goto (_) -> ()
    | Continue -> ()
    | Break -> ()
    | Return (Some e) ->
        begin match current_function with
        | None -> failwith "return statement outside of function"
        | Some f -> type_check (ASTStatement (stmt)) f.return.data e
        end
    | Return (None) ->
        begin match current_function with
        | None -> failwith "return statement outside of function"
        | Some f ->
            begin match f.return.data with
            | Void -> ()
            | _ -> raise (Type_error (f.return.data, None, ASTStatement (stmt)))
            end
        end
    | MessageCall (_, _) ->
        failwith "message calls not yet supported"
    | RefAssign (_, _) ->
        failwith "reference assignment not yet supported"

  method! visit_variable var =
    super#visit_variable var;
    self#check_variable var

  method! visit_declaration decl =
    (* Create new scope if needed *)
    begin match decl with
    | Function (f) ->
        env#push;
        current_function <- Some f;
        List.iter env#push_var f.params
    | _ -> ()
    end;
    super#visit_declaration decl;
    match decl with
    | Global (g) ->
        self#check_variable g
    | Function (_) ->
        env#pop;
        current_function <- None
    | FuncType (_) -> ()
    | StructDef (_) -> ()
    | Enum (_) -> ()

  method check_variable decl =
    List.iter (fun e -> type_check (ASTVariable (decl)) Int e) decl.array_dim;
    match decl.initval with
    | Some expr -> type_check (ASTVariable (decl)) decl.type_spec.data expr
    | None -> ()

end
