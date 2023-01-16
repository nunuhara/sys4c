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

let expr_replace dst expr =
  dst.valuetype <- expr.valuetype;
  dst.node <- expr.node

let const_replace dst const_expr =
  begin match const_expr with
  | ConstInt _    -> dst.valuetype <- Some (Ain.Type.make Ain.Type.Int)
  | ConstFloat _  -> dst.valuetype <- Some (Ain.Type.make Ain.Type.Float)
  | ConstChar _   -> dst.valuetype <- Some (Ain.Type.make Ain.Type.Int)
  | ConstString _ -> dst.valuetype <- Some (Ain.Type.make Ain.Type.String)
  | _             -> compiler_bug "const_replace: not a constant expression"
                                  (Some(ASTExpression {node=const_expr; valuetype=None}))
  end;
  dst.node <- const_expr

let const_binary dst a b int_op float_op =
  match a with
  | ConstInt i_a ->
      begin match b with
      | ConstInt i_b ->
          begin match int_op with
          | Some iop -> const_replace dst (ConstInt (iop i_a i_b))
          | None -> ()
          end
      | _ -> ()
      end
  | ConstFloat f_a ->
      begin match b with
      | ConstFloat f_b ->
          begin match float_op with
          | Some fop -> const_replace dst (ConstFloat (fop f_a f_b))
          | None -> ()
          end
      | _ -> ()
      end
  | _ -> ()

let const_compare dst a b int_op float_op =
  match a with
  | ConstInt i_a ->
      begin match b with
      | ConstInt i_b -> const_replace dst (ConstInt (int_op i_a i_b))
      | _ -> ()
      end
  | ConstFloat f_a ->
      begin match b with
      | ConstFloat f_b -> const_replace dst (ConstInt (float_op f_a f_b))
      | _ -> ()
      end
  | _ -> ()

let const_unary dst e int_op float_op =
  match e with
  | ConstInt i ->
      begin match int_op with
      | Some iop -> const_replace dst (ConstInt (iop i))
      | None -> ()
      end
  | ConstFloat f ->
      begin match float_op with
      | Some fop -> const_replace dst (ConstFloat (fop f))
      | None -> ()
      end
  | _ -> ()

class const_eval_visitor ctx = object (self)
  inherit ivisitor ctx as super

  method eval_expression (expr:expression) =
    match expr.node with
    | ConstInt _ -> ()
    | ConstFloat _ -> ()
    | ConstChar _ -> ()
    | ConstString _ -> ()
    | Ident (name, _) ->
        begin match environment#resolve name with
        | ResolvedLocal v | ResolvedConstant v ->
            begin match v.type_spec.qualifier with
            | Some Const ->
                begin match v.initval with
                | Some e -> const_replace expr e.node
                | None -> const_error v
                end
            | _ -> ()
            end
        | _ -> ()
        end
    | Unary (op, e) ->
        let const_not i = if i = 0 then 1 else 0 in
        begin match op with
        | UPlus  -> const_unary expr e.node (Some (~+)) (Some (~+.))
        | UMinus -> const_unary expr e.node (Some (~-)) (Some (~-.))
        | LogNot -> const_unary expr e.node (Some const_not) None
        | BitNot -> const_unary expr e.node (Some lnot) None
        | AddrOf -> ()
        | PreInc -> ()
        | PreDec -> ()
        | PostInc -> ()
        | PostDec -> ()
        end
    | Binary (op, a, b) ->
        let mk_compare op = fun a b -> if op a b then 1 else 0 in
        let const_eq = mk_compare (=) in
        let const_neq a b = if a = b then 0 else 1 in
        let const_lt = mk_compare (<) in
        let const_gt = mk_compare (<) in
        let const_lte = mk_compare (<=) in
        let const_gte = mk_compare (>=) in
        let const_feq = mk_compare Float.equal in
        let const_fneq a b = if Float.equal a b then 0 else 1 in
        let const_flt = mk_compare Float.(<) in
        let const_fgt = mk_compare Float.(>) in
        let const_flte = mk_compare Float.(<=) in
        let const_fgte = mk_compare Float.(>=) in
        let const_logor a b = if not (a = 0) then a else (if not (b = 0) then b else 0) in
        let const_logand a b = if not (a = 0) then (if not (b = 0) then 1 else 0) else 0 in
        begin match op with
        | Plus -> const_binary expr a.node b.node (Some (+)) (Some (+.))
        | Minus -> const_binary expr a.node b.node (Some (-)) (Some (-.))
        | Times -> const_binary expr a.node b.node (Some ( * )) (Some ( *.))
        | Divide -> const_binary expr a.node b.node (Some (/)) (Some (/.))
        | Modulo -> const_binary expr a.node b.node (Some (mod)) None
        | Equal -> const_compare expr a.node b.node const_eq const_feq
        | NEqual -> const_compare expr a.node b.node const_neq const_fneq
        | LT -> const_compare expr a.node b.node const_lt const_flt
        | GT -> const_compare expr a.node b.node const_gt const_fgt
        | LTE -> const_compare expr a.node b.node const_lte const_flte
        | GTE -> const_compare expr a.node b.node const_gte const_fgte
        | LogOr -> const_binary expr a.node b.node (Some const_logor) None
        | LogAnd -> const_binary expr a.node b.node (Some const_logand) None
        | BitOr -> const_binary expr a.node b.node (Some (lor)) None
        | BitXor -> const_binary expr a.node b.node (Some (lxor)) None
        | BitAnd -> const_binary expr a.node b.node (Some (land)) None
        | LShift -> const_binary expr a.node b.node (Some (lsl)) None
        | RShift -> const_binary expr a.node b.node (Some (lsr)) None
        end
    | Assign (_, _, _) -> ()
    | Seq (_, _) -> ()
    | Ternary (test, con, alt) ->
        begin match test.node with
        | ConstInt 0 -> expr_replace expr alt
        | ConstInt _ -> expr_replace expr con
        | _ -> ()
        end
    | Cast (t, e) ->
        begin match t with
        | Int ->
            begin match e.node with
            | ConstInt _ -> const_replace expr e.node
            | ConstFloat f -> const_replace expr (ConstInt (int_of_float f))
            | ConstChar _ -> () (* TODO? *)
            | _ -> ()
            end
        | Bool ->
            begin match e.node with
            | ConstInt i -> const_replace expr (ConstInt (if i = 0 then 0 else 1))
            | _ -> ()
            end
        | Float ->
            begin match e.node with
            | ConstInt i -> const_replace expr (ConstFloat (float_of_int i))
            | ConstFloat _ -> const_replace expr e.node
            | ConstChar _ -> () (* TODO? *)
            | _ -> ()
            end
        | _ -> ()
        end
    | Subscript (_, _) -> ()
    | Member (_, _, _) -> ()
    | Call (_, _, _) -> ()
    | New (_, _, _) -> ()
    | This -> ()

  method! visit_toplevel decls =
    let eval_global g =
      match g.initval with
      | Some expr -> self#eval_expression expr
      | None -> const_error g
    in
    (* XXX: evaluate all global constants first *)
    List.iter ctx.const_vars ~f:eval_global;
    super#visit_toplevel decls

  method! visit_expression expr =
    super#visit_expression expr;
    self#eval_expression expr

  method check_vardecl v =
    match v.type_spec.qualifier with
    | Some Const ->
        begin match v.initval with
        | Some e ->
            begin match e.node with
            | ConstInt _ -> ()
            | ConstFloat _ -> ()
            | ConstChar _ -> ()
            | ConstString _ -> ()
            | _ -> const_error v
            end
        | None ->
            const_error v
        end
    | _ -> ()

  method! visit_local_variable v =
    super#visit_local_variable v;
    self#check_vardecl v

  method! visit_declaration d =
    super#visit_declaration d;
    match d with
    | Global v -> self#check_vardecl v
    | _ -> ()

end

let evaluate_constant_expressions ctx decls =
  (new const_eval_visitor ctx)#visit_toplevel decls
