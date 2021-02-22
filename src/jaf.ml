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
  mutable data : data_type;
  qualifier    : type_qualifier option
}
and data_type =
  | Untyped
  | Unresolved of string
  | Void
  | Int
  | Bool
  | Float
  | String
  | Struct   of string * int
  (*| Enum*)
  | Array    of type_specifier * int
  | Wrap     of type_specifier
  | HLLParam
  | HLLFunc
  | Delegate
  (*| Typedef*)
  (*| Functype*)

type expression = {
  mutable valuetype : type_specifier;
  mutable node : ast_expression
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
  | New         of data_type  * expression list
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
  | StructDef of structdecl
  | Enum of enumdecl

class ivisitor = object
  method visit_expr_pre (_ : expression) = true
  method visit_stmt_pre (_ : statement) = true
  method visit_local_pre (_ : declaration) = true
  method visit_decl_pre (_ : external_declaration) = true
  method visit_expr_post (_ : expression) = ()
  method visit_stmt_post (_ : statement) = ()
  method visit_local_post (_ : declaration) = ()
  method visit_decl_post (_ : external_declaration) = ()
end

let rec accept_expr v (e : expression) =
  let accept = accept_expr v in
  if v#visit_expr_pre e then begin
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
    v#visit_expr_post e
  end

let rec accept_stmt v s =
  let e_accept = accept_expr v in
  let s_accept = accept_stmt v in
  let i_accept = accept_block_item v in
  if v#visit_stmt_pre s then begin
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
    v#visit_stmt_post s
  end
and accept_local v d =
  if v#visit_local_pre d then begin
    List.iter (accept_expr v) d.array_dim;
    Option.iter (accept_expr v) d.initval;
    v#visit_local_post d
  end
and accept_block_item v = function
  | Statement (s) -> accept_stmt v s
  | Declarations (ds) -> List.iter (accept_local v) ds

let accept_decl v d =
  let f_accept f = List.iter (accept_block_item v) f.body in
  let d_accept decl =
    List.iter (accept_expr v) decl.array_dim;
    Option.iter (accept_expr v) decl.initval
  in
  if v#visit_decl_pre d then begin
    begin match d with
    | Global (g) -> d_accept g
    | Function (f) -> f_accept f
    | FuncType (f) -> f_accept f
    | StructDef (s) ->
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
    v#visit_decl_post d
  end

let accept_toplevel v decls = List.iter (accept_decl v) decls

let unary_op_to_string op =
  match op with
  | UPlus -> "+"
  | UMinus -> "-"
  | LogNot -> "!"
  | BitNot -> "~"
  | AddrOf -> "&"
  | PreInc -> "++"
  | PreDec -> "--"
  | PostInc -> "++"
  | PostDec -> "--"

let binary_op_to_string op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | Equal -> "=="
  | NEqual -> "!="
  | LT -> "<"
  | GT -> ">"
  | LTE -> "<="
  | GTE -> ">="
  | LogOr -> "||"
  | LogAnd -> "&&"
  | BitOr -> "|"
  | BitXor -> "^"
  | BitAnd -> "&"
  | LShift -> "<<"
  | RShift -> ">>"

let assign_op_to_string op =
  match op with
  | EqAssign -> "="
  | PlusAssign -> "+="
  | MinusAssign -> "-="
  | TimesAssign -> "*="
  | DivideAssign -> "/="
  | ModuloAssign -> "%="
  | OrAssign -> "|="
  | XorAssign -> "^="
  | AndAssign -> "&="
  | LShiftAssign -> "<<="
  | RShiftAssign -> ">>="

let type_qualifier_to_string = function
  | Const -> "const"
  | Ref -> "ref"
  | Override -> "override"

let rec data_type_to_string = function
  | Untyped -> "untyped"
  | Unresolved (s) -> "Unresolved<" ^ s ^ ">"
  | Void -> "void"
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Struct (s, _) -> s
  | Array (t, _) -> "array<" ^ (type_spec_to_string t) ^ ">" (* TODO: rank *)
  | Wrap (t) -> "wrap<" ^ (type_spec_to_string t) ^ ">"
  | HLLParam -> "hll_param"
  | HLLFunc -> "hll_func"
  | Delegate -> "delegate"
and type_spec_to_string ts =
  match ts.qualifier with
  | Some q -> (type_qualifier_to_string q) ^ " " ^ (data_type_to_string ts.data)
  | None -> (data_type_to_string ts.data)



let rec expr_to_string (e : expression) =
  let arglist_to_string = function
    | [] -> "()"
    | arg::args ->
        let rec loop result = function
          | [] -> result
          | arg::args -> loop (sprintf "%s, %s" result (expr_to_string arg)) args
        in
        sprintf "(%s)" (loop (expr_to_string arg) args)
  in
  match e.node with
  | ConstInt (i) ->
      string_of_int i
  | ConstFloat (f) ->
      string_of_float f
  | ConstChar (s) ->
      sprintf "'%s'" s
  | ConstString (s) ->
      sprintf "\"%s\"" s
  | Ident (s) ->
      s
  | Unary (op, e) ->
      begin match op with
      | PostInc | PostDec -> (expr_to_string e) ^ (unary_op_to_string op)
      | _ -> (unary_op_to_string op) ^ (expr_to_string e)
      end
  | Binary (op, a, b) ->
      sprintf "%s %s %s" (expr_to_string a) (binary_op_to_string op) (expr_to_string b)
  | Assign (op, a, b) ->
      sprintf "%s %s %s" (expr_to_string a) (assign_op_to_string op) (expr_to_string b)
  | Seq (a, b) ->
      sprintf "%s, %s" (expr_to_string a) (expr_to_string b)
  | Ternary (a, b, c) ->
      sprintf "%s ? %s : %s" (expr_to_string a) (expr_to_string b) (expr_to_string c)
  | Cast (t, e) ->
      sprintf "(%s)%s" (data_type_to_string t) (expr_to_string e)
  | Subscript (e, i) ->
      sprintf "%s[%s]" (expr_to_string e) (expr_to_string i)
  | Member (e, s) ->
      sprintf "%s.%s" (expr_to_string e) s
  | Call (f, args) ->
      sprintf "%s%s" (expr_to_string f) (arglist_to_string args)
  | New (t, args) ->
      sprintf "%s%s" (data_type_to_string t) (arglist_to_string args)
  | This ->
      "this"

let rec stmt_to_string (stmt : statement) =
  match stmt.node with
  | EmptyStatement ->
      ";"
  | Expression (e) ->
      (expr_to_string e) ^ ";"
  | Compound (items) ->
      items
      |> List.map block_item_to_string
      |> List.fold_left (^) ""
  | Labeled (label, stmt) ->
      sprintf "%s: %s" label (stmt_to_string stmt)
  | If (test, body, alt) ->
      let s_test = expr_to_string test in
      let s_body = stmt_to_string body in
      let s_alt = stmt_to_string alt in
      sprintf "if (%s) %s else %s" s_test s_body s_alt
  | While (test, body) ->
      sprintf "while (%s) %s" (expr_to_string test) (stmt_to_string body)
  | DoWhile (test, body) ->
      sprintf "do %s while (%s);" (stmt_to_string body) (expr_to_string test)
  | For (init, test, inc, body) ->
      let s_init = block_item_to_string init in
      let s_test = stmt_to_string test in
      let s_body = stmt_to_string body in
      let s_inc =
        match inc with
        | None -> ""
        | Some e -> expr_to_string e
      in
      sprintf "for (%s %s %s) %s" s_init s_test s_inc s_body
  | Goto (label) ->
      sprintf "goto %s;" label
  | Continue ->
      "continue;"
  | Break ->
      "break;"
  | Return (None) ->
      "return;"
  | Return (Some e) ->
      sprintf "return %s;" (expr_to_string e)
  | MessageCall (msg, f) ->
      sprintf "'%s' %s;" msg f
  | RefAssign (dst, src) ->
      sprintf "%s <- %s;" (expr_to_string dst) (expr_to_string src)
and decl_to_string' d =
  let t = type_spec_to_string d.type_spec in
  let dim_iter l r = l ^ (sprintf "[%s]" (expr_to_string r)) in
  let dims = List.fold_left dim_iter "" d.array_dim in
  let init =
    match d.initval with
    | None -> ""
    | Some e -> sprintf " = %s" (expr_to_string e)
  in
  sprintf "%s %s%s%s" t dims d.name init
and decl_to_string d =
  (decl_to_string' d) ^ ";"
and block_item_to_string item =
  match item with
  | Statement (s) -> stmt_to_string s
  | Declarations (ds) -> List.fold_left (^) "" (List.map decl_to_string ds)

let extdecl_to_string d =
  let params_to_string = function
    | [] -> "()"
    | p::ps ->
        let rec loop result = function
          | [] -> result
          | p::ps -> loop (sprintf "%s, %s" result (decl_to_string' p)) ps
        in
        sprintf "(%s)" (loop (decl_to_string' p) ps)
  in
  let block_to_string block =
    List.fold_left (^) "" (List.map block_item_to_string block)
  in
  match d with
  | Global (d) ->
      decl_to_string d
  | Function (d) ->
      let return = type_spec_to_string d.return in
      let params = params_to_string d.params in
      let body = block_to_string d.body in
      sprintf "%s %s%s { %s }" return d.name params body
  | FuncType (d) ->
      let return = type_spec_to_string d.return in
      let params = params_to_string d.params in
      sprintf "functype %s %s%s;" return d.name params
  | StructDef (d) ->
      let sdecl_to_string = function
        | MemberDecl (d) ->
            decl_to_string d
        | Constructor (d) ->
            let params = params_to_string d.params in
            let body = block_to_string d.body in
            sprintf "%s%s { %s }" d.name params body
        | Destructor (d) ->
            let params = params_to_string d.params in
            let body = block_to_string d.body in
            sprintf "~%s%s { %s }" d.name params body
        | Method (d) ->
            let return = type_spec_to_string d.return in
            let params = params_to_string d.params in
            let body = block_to_string d.body in
            sprintf "%s %s%s { %s }" return d.name params body
      in
      let body = List.fold_left (^) "" (List.map sdecl_to_string d.decls) in
      sprintf "struct %s { %s };" d.name body
  | Enum (d) ->
      let enumval_to_string = function
        | (s, None) -> s
        | (s, Some e) -> sprintf "%s = %s" s (expr_to_string e)
      in
      let enumvals_fold l r = l ^ ", " ^ r in
      let body = List.fold_left enumvals_fold "" (List.map enumval_to_string d.values) in
      let name =
        match d.name with
        | None -> ""
        | Some s -> s ^ " "
      in
      sprintf "enum %s{ %s };" name body
