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

exception Type_error of Alice.Ain.Type.data * expression option * ast_node
exception Undefined_variable of string * ast_node
exception Arity_error of Alice.Ain.Function.t * expression list * ast_node

let rec type_equal expected actual =
  match expected with
  | Alice.Ain.Type.Void -> failwith "type checker expected void type"
  | Alice.Ain.Type.Int ->
      begin match actual with
      | Alice.Ain.Type.Int | Alice.Ain.Type.Bool | Alice.Ain.Type.LongInt -> true
      | _ -> false
      end
  | Alice.Ain.Type.Bool ->
      begin match actual with
      | Alice.Ain.Type.Int | Alice.Ain.Type.Bool | Alice.Ain.Type.LongInt -> true
      | _ -> false
      end
  | Alice.Ain.Type.LongInt ->
      begin match actual with
      | Alice.Ain.Type.Int | Alice.Ain.Type.Bool | Alice.Ain.Type.LongInt -> true
      | _ -> false
      end
  | Alice.Ain.Type.Float ->
      begin match actual with
      | Alice.Ain.Type.Float -> true
      | _ -> false
      end
  | Alice.Ain.Type.String ->
      begin match actual with
      | Alice.Ain.Type.String -> true
      | _ -> false
      end
  | Alice.Ain.Type.Struct (i) ->
      begin match actual with
      | Alice.Ain.Type.Struct (i_a) -> i == i_a
      | _ -> false
      end
  | Alice.Ain.Type.IMainSystem ->
      begin match actual with
      | Alice.Ain.Type.IMainSystem -> true
      | _ -> false
      end
  | Alice.Ain.Type.FuncType (i) ->
      begin match actual with
      | Alice.Ain.Type.FuncType (i_a) -> i == i_a
      | _ -> false
      end
  | Alice.Ain.Type.Delegate (i) ->
      begin match actual with
      | Alice.Ain.Type.Delegate (i_a) -> i == i_a
      | _ -> false
      end
  | Alice.Ain.Type.HLLParam ->
      begin match actual with
      | Alice.Ain.Type.HLLParam -> true
      | _ -> false
      end
  | Alice.Ain.Type.Array (t) ->
      begin match actual with
      | Alice.Ain.Type.Array (t_a) -> type_equal t.data t_a.data
      | _ -> false
      end
  | Alice.Ain.Type.Wrap (t) ->
      begin match actual with
      | Alice.Ain.Type.Wrap (t_a) -> type_equal t.data t_a.data
      | _ -> false
      end
  | Alice.Ain.Type.Option (t) ->
      begin match actual with
      | Alice.Ain.Type.Option (t_a) -> type_equal t.data t_a.data
      | _ -> false
      end
  | Alice.Ain.Type.Unknown87 (t) ->
      begin match actual with
      | Alice.Ain.Type.Unknown87 (t_a) -> type_equal t.data t_a.data
      | _ -> false
      end
  | Alice.Ain.Type.IFace ->
      begin match actual with
      | Alice.Ain.Type.IFace -> true
      | _ -> false
      end
  | Alice.Ain.Type.Enum2 (i) ->
      begin match actual with
      | Alice.Ain.Type.Enum2 (i_a) -> i == i_a
      | _ -> false
      end
  | Alice.Ain.Type.Enum (i) ->
      begin match actual with
      | Alice.Ain.Type.Enum (i_a) -> i == i_a
      | _ -> false
      end
  | Alice.Ain.Type.HLLFunc ->
      begin match actual with
      | Alice.Ain.Type.HLLFunc -> true
      | _ -> false
      end
  | Alice.Ain.Type.IFaceWrap ->
      begin match actual with
      | Alice.Ain.Type.IFaceWrap -> true
      | _ -> false
      end
  | Alice.Ain.Type.Function (_) ->
      begin match actual with
      (* TODO: should functype/delegate count here? *)
      | Alice.Ain.Type.Function (_) -> true
      | _ -> false
      end

let type_castable dst src =
  match dst with
  (* FIXME: cast to void should be allowed *)
  | Void -> failwith "type checker cast to void type"
  | Int | Bool | Float ->
      begin match src with
      | Alice.Ain.Type.Int | Alice.Ain.Type.Bool | Alice.Ain.Type.Float -> true
      | _ -> false
      end
  | _ -> false

let type_check parent expected actual =
  match actual.valuetype with
  | None ->
      failwith "tried to type check untyped expression"
  | Some a_t ->
      if not (type_equal expected a_t.data) then
        raise (Type_error (expected, Some actual, parent))

let type_check_numeric parent actual =
  match actual.valuetype with
  | None ->
      failwith "tried to type check untyped expression"
  | Some a_t ->
      begin match a_t.data with
      | Alice.Ain.Type.Int | Alice.Ain.Type.Bool | Alice.Ain.Type.Float -> ()
      | _ -> raise (Type_error (Int, Some actual, parent))
      end

let type_check_struct parent actual =
  match actual.valuetype with
  | None ->
      failwith "tried to type check untyped expression"
  | Some a_t ->
      begin match a_t.data with
      | Alice.Ain.Type.Struct (i) -> i
      | _ -> raise (Type_error(Struct 0, Some actual, parent))
      end

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
  (* XXX: needed for 'this' expression *)
  val mutable current_class = None

  method! visit_expression expr =
    super#visit_expression expr;
    let unwrap valuetype =
      match valuetype with
      | None -> failwith "type-checker: valuetype is None"
      | Some vt -> vt
    in
    let check = type_check (ASTExpression (expr)) in
    let check_numeric = type_check_numeric (ASTExpression (expr)) in
    let check_struct = type_check_struct (ASTExpression (expr)) in
    let check_expr a b = check (unwrap a.valuetype).data b in
    let set_valuetype spec =
      expr.valuetype <- Some (jaf_to_ain_type spec)
    in
    match expr.node with
    | ConstInt (_) ->
        expr.valuetype <- Some (Alice.Ain.Type.make Alice.Ain.Type.Int)
    | ConstFloat (_) ->
        expr.valuetype <- Some (Alice.Ain.Type.make Alice.Ain.Type.Float)
    | ConstChar (_) -> ()
    | ConstString (_) ->
        expr.valuetype <- Some (Alice.Ain.Type.make Alice.Ain.Type.String)
    | Ident (name) ->
        begin match env#get name with
        | Some v ->
            set_valuetype { data=v.type_spec.data; qualifier=None }
        | None ->
            begin match Alice.Ain.get_global ain name with
            | Some g ->
                expr.valuetype <- Some g.value_type
            | None ->
                begin match Alice.Ain.get_function ain name with
                | Some f ->
                    expr.valuetype <- Some (Alice.Ain.Type.make (Alice.Ain.Type.Function f.index))
                | None ->
                    raise (Undefined_variable (name, ASTExpression (expr)))
                end
            end
        end
    | Unary (op, e) ->
        begin match op with
        | UPlus | UMinus | PreInc | PreDec | PostInc | PostDec ->
            check_numeric e
        | LogNot | BitNot ->
            check Alice.Ain.Type.Int e
        | AddrOf ->
            failwith "function types not yet supported"
        end;
        expr.valuetype <- Some (unwrap e.valuetype)
    | Binary (op, a, b) ->
        begin match op with
        | Plus | Minus | Times | Divide | LT | GT | LTE | GTE ->
            check_numeric a;
            check_numeric b;
            (* TODO: allow coercion *)
            check_expr a b
        | Modulo | LogOr | LogAnd | BitOr | BitXor | BitAnd | LShift | RShift ->
            check Alice.Ain.Type.Int a;
            check Alice.Ain.Type.Int b
        | Equal | NEqual ->
            begin match (unwrap a.valuetype).data with
            | Alice.Ain.Type.String ->
                check Alice.Ain.Type.String b
            | _ ->
                check_numeric a;
                check_numeric b;
                (* TODO: allow coercion *)
                check_expr a b
            end
        end;
        expr.valuetype <- a.valuetype
    | Assign (op, lhs, rhs) ->
        (* TODO: check that lhs is an lvalue *)
        begin match op with
        | EqAssign ->
            check_expr lhs rhs
        | PlusAssign | MinusAssign | TimesAssign | DivideAssign ->
            check_numeric lhs;
            check_numeric rhs;
            (* TODO: allow coercion *)
            check_expr lhs rhs
        | ModuloAssign | OrAssign | XorAssign | AndAssign
        | LShiftAssign | RShiftAssign ->
            check Alice.Ain.Type.Int lhs;
            check Alice.Ain.Type.Int rhs
        end;
        expr.valuetype <- lhs.valuetype
    | Seq (_, e) ->
        expr.valuetype <- e.valuetype
    | Ternary (test, con, alt) ->
        check Alice.Ain.Type.Int test;
        check_expr con alt;
        expr.valuetype <- con.valuetype
    | Cast (t, e) ->
        if not (type_castable t (unwrap e.valuetype).data) then
          raise (Type_error ((jaf_to_ain_data_type t), Some e, ASTExpression (expr)));
        set_valuetype { data=t; qualifier=None }
    | Subscript (obj, i) ->
        check Int i;
        begin match (unwrap obj.valuetype).data with
        | Array (t) ->
            expr.valuetype <- Some t
        | _ ->
            let array_type = { data=Unresolved ("?"); qualifier=None } in
            let expected = Array (array_type, 1) in
            raise (Type_error ((jaf_to_ain_data_type expected), Some obj, ASTExpression (expr)))
        end
    | Member (obj, member_name) ->
        let struc = Alice.Ain.get_struct_by_index ain (check_struct obj) in
        let check_member (m : Alice.Ain.Variable.t) =
          String.equal m.name member_name
        in
        begin match List.find_opt check_member struc.members with
        | Some member ->
            expr.valuetype <- Some member.value_type
        | None ->
            let fun_name = struc.name ^ "@" ^ member_name in
            begin match Alice.Ain.get_function ain fun_name with
            | Some f ->
                expr.valuetype <- Some (Alice.Ain.Type.make (Alice.Ain.Type.Function f.index))
            | None ->
                (* TODO: separate error type for this? *)
                raise (Undefined_variable (struc.name ^ "." ^ member_name, ASTExpression(expr)))
            end
        end
    | Call (fn, args) ->
        begin match (unwrap fn.valuetype).data with
        | Alice.Ain.Type.Function (no) ->
            let f = Alice.Ain.Function.of_int ain no in
            if f.nr_args != (List.length args) then
              raise (Arity_error (f, args, ASTExpression (expr)))
            else if f.nr_args > 0 then begin
              (* `take` not in standard library... *)
              let take n lst =
                let rec take_r n lst result =
                  if n = 0 then
                    List.rev result
                  else
                    match lst with
                    | [] -> failwith "nr_args is > nr_vars"
                    | x::xs -> take_r (n - 1) xs (x::result)
                in
                take_r n lst []
              in
              let check_arg a (v:Alice.Ain.Variable.t) =
                check v.value_type.data a
              in
              List.iter2 check_arg args (take f.nr_args f.vars)
            end;
            expr.valuetype <- Some f.return_type
        | _ ->
            raise (Type_error (Alice.Ain.Type.Function (-1), Some fn, ASTExpression (expr)))
        end
    | New (_, _) ->
        failwith "'new' operator not yet supported"
    | This ->
        match current_class with
        | Some i ->
            expr.valuetype <- Some (Alice.Ain.Type.make (Alice.Ain.Type.Struct i))
        | None ->
            (* TODO: separate error type for this? *)
            raise (Undefined_variable ("this", ASTExpression(expr)))

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
        | Some f -> type_check (ASTStatement (stmt)) (jaf_to_ain_data_type f.return.data) e
        end
    | Return (None) ->
        begin match current_function with
        | None -> failwith "return statement outside of function"
        | Some f ->
            begin match f.return.data with
            | Void -> ()
            | _ -> raise (Type_error ((jaf_to_ain_data_type f.return.data), None, ASTStatement (stmt)))
            end
        end
    | MessageCall (_, f_name) ->
        begin match Alice.Ain.get_function ain f_name with
        | Some f ->
            if f.nr_args > 0 then
              raise (Arity_error(f, [], ASTStatement (stmt)))
        | None ->
            raise (Undefined_variable (f_name, ASTStatement (stmt)))
        end
    | RefAssign (_, _) ->
        failwith "reference assignment not yet supported"

  method! visit_variable var =
    super#visit_variable var;
    self#check_variable var

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
    (* Create new scope if needed *)
    begin match decl with
    | Function (f) ->
        env#push;
        current_function <- Some f;
        current_class <- function_class f;
        List.iter env#push_var f.params
    | _ -> ()
    end;
    super#visit_declaration decl;
    match decl with
    | Global (g) ->
        self#check_variable g
    | Function (_) ->
        env#pop;
        current_function <- None;
        current_class <- None
    | FuncType (_) -> ()
    | StructDef (_) -> ()
    | Enum (_) -> ()

  method check_variable decl =
    (* check that array dims are integers *)
    List.iter (fun e -> type_check (ASTVariable (decl)) Int e) decl.array_dim;
    (* check initval matches declared type *)
    begin match decl.initval with
    | Some expr -> type_check (ASTVariable (decl)) (jaf_to_ain_data_type decl.type_spec.data) expr
    | None -> ()
    end;
    (* add local variable to environment *)
    env#push_var decl

end
