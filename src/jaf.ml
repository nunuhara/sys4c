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

type unary_op =
  | UPlus
  | UMinus
  | LogNot
  | BitNot
  | AddrOf
  | PreInc
  | PreDec
  | PostInc
  | PostDec

type binary_op =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Equal
  | NEqual
  | LT
  | GT
  | LTE
  | GTE
  | LogOr
  | LogAnd
  | BitOr
  | BitXor
  | BitAnd
  | LShift
  | RShift

type assign_op =
  | EqAssign
  | PlusAssign
  | MinusAssign
  | TimesAssign
  | DivideAssign
  | ModuloAssign
  | OrAssign
  | XorAssign
  | AndAssign
  | LShiftAssign
  | RShiftAssign

type type_qualifier =
  | Const
  | Ref
  | Override

type type_specifier = {
  data      : data_type;
  qualifier : type_qualifier option
}
and data_type =
  | Untyped
  | Void
  | Int
  | Bool
  | Float
  | String
  | Struct   of string
  (*| Enum*)
  | Array    of type_specifier * int
  | Wrap     of type_specifier
  | HLLParam
  | HLLFunc
  | Delegate
  (*| Typedef*)
  (*| Functype*)

type expression = {
  valuetype : type_specifier;
  node      : ast_expression
}
and ast_expression =
  | ConstInt    of int
  | ConstFloat  of float
  | ConstChar   of string
  | ConstString of string
  | Ident       of string
  | Unary       of unary_op   * expression
  | Binary      of binary_op  * expression      * expression
  | Assign      of assign_op  * expression      * expression
  | Seq         of expression * expression
  | Ternary     of expression * expression      * expression
  | Cast        of data_type  * expression
  | Subscript   of expression * expression
  | Member      of expression * string
  | Call        of expression * expression list
  | New         of string     * expression list
  | This

type block_item =
  | Statement    of statement
  | Declarations of declaration list
and statement = {
  node : ast_statement
}
and ast_statement =
  | EmptyStatement
  | Expression     of expression
  | Compound       of block_item list
  | Labeled        of string          * statement
  | If             of expression      * statement * statement
  | While          of expression      * statement
  | DoWhile        of expression      * statement
  | For            of block_item      * statement * expression option * statement
  | Goto           of string
  | Continue
  | Break
  | Return         of expression option
  | MessageCall    of string          * string
  | RefAssign      of expression      * expression
and declaration = {
  name      : string;
  array_dim : expression list;
  type_spec : type_specifier;
  initval   : expression option
}

type fundecl = {
  name   : string;
  return : type_specifier;
  params : declaration list;
  body   : block_item list
}

type struct_declaration =
  | MemberDecl of declaration
  | Constructor of fundecl
  | Destructor of fundecl
  | Method of fundecl

type structdecl = {
  name : string;
  decls : struct_declaration list
}

type enumdecl = {
  name   : string option;
  values : (string * expression option) list
}

type external_declaration =
  | Function of fundecl
  | Global of declaration
  | FuncType of fundecl
  | Struct of structdecl
  | Enum of enumdecl

type ast_visitor = {
  expr_pre : (expression -> unit) option;
  expr_post : (expression -> unit) option;
  stmt_pre  : (statement -> unit) option;
  stmt_post : (statement -> unit) option;
  local_pre  : (declaration -> unit) option;
  local_post : (declaration -> unit) option;
  decl_pre   : (external_declaration -> unit) option;
  decl_post  : (external_declaration -> unit) option
}

let rec accept_expr v e =
  let accept = accept_expr v in
  if (Option.is_some v.expr_pre) || (Option.is_some v.expr_post) then begin
    Option.iter (fun f -> f e) v.expr_pre;
    begin match e.node with
    | ConstInt (_) -> ()
    | ConstFloat (_) -> ()
    | ConstChar (_) -> ()
    | ConstString (_) -> ()
    | Ident (_) -> ()
    | Unary (_, e2) -> accept e2
    | Binary (_, lhs, rhs) -> accept lhs; accept rhs
    | Assign (_, lhs, rhs) -> accept lhs; accept rhs
    | Seq (a, b) -> accept a; accept b
    | Ternary (a, b, c) -> accept a; accept b; accept c
    | Cast (_, a) -> accept a
    | Subscript (a, i) -> accept a; accept i
    | Member (a, _) -> accept a
    | Call (f, a) -> accept f; List.iter accept a
    | New (_, a) -> List.iter accept a
    | This -> ()
    end;
    Option.iter (fun f -> f e) v.expr_post
  end

let rec accept_stmt v s =
  let e_accept = accept_expr v in
  let s_accept = accept_stmt v in
  let i_accept = accept_block_item v in
  Option.iter (fun f -> f s) v.stmt_pre;
  begin match s.node with
  | EmptyStatement -> ()
  | Expression (e) -> e_accept e
  | Compound (items) -> List.iter i_accept items
  | Labeled (_, a) -> s_accept a
  | If (test, cons, alt) -> e_accept test; s_accept cons; s_accept alt
  | While (test, body) -> e_accept test; s_accept body
  | DoWhile (test, body) -> s_accept body; e_accept test
  | For (init, test, inc, body) ->
      i_accept init;
      s_accept test;
      Option.iter e_accept inc;
      s_accept body
  | Goto (_) -> ()
  | Continue -> ()
  | Break -> ()
  | Return (e) -> Option.iter e_accept e
  | MessageCall (_, _) -> ()
  | RefAssign (a, b) -> e_accept a; e_accept b
  end;
  Option.iter (fun f -> f s) v.stmt_post
and accept_local v d =
  Option.iter (fun f -> f d) v.local_pre;
  List.iter (accept_expr v) d.array_dim;
  Option.iter (accept_expr v) d.initval;
  Option.iter (fun f -> f d) v.local_post
and accept_block_item v = function
  | Statement (s) -> accept_stmt v s
  | Declarations (ds) -> List.iter (accept_local v) ds

let accept_decl v d =
  let f_accept f = List.iter (accept_block_item v) f.body in
  let d_accept decl =
    List.iter (accept_expr v) decl.array_dim;
    Option.iter (accept_expr v) decl.initval
  in
  Option.iter (fun f -> f d) v.decl_pre;
  begin match d with
  | Global (g) -> d_accept g
  | Function (f) -> f_accept f
  | FuncType (f) -> f_accept f
  | Struct (s) ->
      let structdecl_accept = function
        | MemberDecl (decl) -> d_accept decl
        | Constructor (f) -> f_accept f
        | Destructor (f) -> f_accept f
        | Method (f) -> f_accept f
      in
      List.iter structdecl_accept s.decls
  | Enum (enum) ->
      let v_accept (_, expr) = Option.iter (accept_expr v) expr in
      List.iter v_accept enum.values
  end;
  Option.iter (fun f -> f d) v.decl_post
