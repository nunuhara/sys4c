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
  (*| Functype*)

type expression = {
  mutable valuetype : Alice.Ain.Type.t option;
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
  | Declarations of variable list
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
  | For            of block_item      * expression * expression option * statement
  | Goto           of string
  | Continue
  | Break
  | Return         of expression option
  | MessageCall    of string          * string
  | RefAssign      of expression      * expression
and variable = {
  name      : string;
  array_dim : expression list;
  type_spec : type_specifier;
  initval   : expression option
}

type fundecl = {
  name   : string;
  return : type_specifier;
  params : variable list;
  body   : block_item list
}

type struct_declaration =
  | MemberDecl of variable
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

type declaration =
  | Function of fundecl
  | Global of variable
  | FuncType of fundecl
  | StructDef of structdecl
  | Enum of enumdecl

type ast_node =
  | ASTExpression of expression
  | ASTStatement of statement
  | ASTVariable of variable
  | ASTDeclaration of declaration

class ivisitor = object (self)

  method visit_expression (e : expression) =
    match e.node with
    | ConstInt (_) -> ()
    | ConstFloat (_) -> ()
    | ConstChar (_) -> ()
    | ConstString (_) -> ()
    | Ident (_) -> ()
    | Unary (_, e) ->
        self#visit_expression e
    | Binary (_, lhs, rhs) ->
        self#visit_expression lhs;
        self#visit_expression rhs
    | Assign (_, lhs, rhs) ->
        self#visit_expression lhs;
        self#visit_expression rhs
    | Seq (a, b) ->
        self#visit_expression a;
        self#visit_expression b
    | Ternary (a, b, c) ->
        self#visit_expression a;
        self#visit_expression b;
        self#visit_expression c
    | Cast (_, obj) ->
        self#visit_expression obj
    | Subscript (arr, i) ->
        self#visit_expression arr;
        self#visit_expression i
    | Member (obj, _) ->
        self#visit_expression obj
    | Call (f, args) ->
        self#visit_expression f;
        List.iter self#visit_expression args
    | New (_, args) ->
        List.iter self#visit_expression args
    | This -> ()

  method visit_statement (s : statement) =
    match s.node with
    | EmptyStatement -> ()
    | Expression (e) ->
        self#visit_expression e
    | Compound (items) ->
        List.iter self#visit_block_item items
    | Labeled (_, a) ->
        self#visit_statement a
    | If (test, cons, alt) ->
        self#visit_expression test;
        self#visit_statement cons;
        self#visit_statement alt
    | While (test, body) ->
        self#visit_expression test;
        self#visit_statement body
    | DoWhile (test, body) ->
        self#visit_statement body;
        self#visit_expression test
    | For (init, test, inc, body) ->
        self#visit_block_item init;
        self#visit_expression test;
        Option.iter self#visit_expression inc;
        self#visit_statement body
    | Goto (_) -> ()
    | Continue -> ()
    | Break -> ()
    | Return (e) ->
        Option.iter self#visit_expression e
    | MessageCall (_, _) -> ()
    | RefAssign (a, b) ->
        self#visit_expression a;
        self#visit_expression b

  method visit_variable v =
    List.iter self#visit_expression v.array_dim;
    Option.iter self#visit_expression v.initval

  method visit_block_item item =
    match item with
    | Statement (s) -> self#visit_statement s
    | Declarations (ds) -> List.iter self#visit_variable ds

  method visit_declaration d =
    let visit_vardecl d =
      List.iter self#visit_expression d.array_dim;
      Option.iter self#visit_expression d.initval
    in
    let visit_fundecl d =
      List.iter self#visit_block_item d.body
    in
    match d with
    | Global (g) -> visit_vardecl g
    | Function (f) -> visit_fundecl f
    | FuncType (f) -> visit_fundecl f
    | StructDef (s) ->
        let visit_structdecl = function
          | MemberDecl (d) -> visit_vardecl d
          | Constructor (f) -> visit_fundecl f
          | Destructor (f) -> visit_fundecl f
          | Method (f) -> visit_fundecl f
        in
        List.iter visit_structdecl s.decls
    | Enum (enum) ->
        let visit_enumval (_, expr) = Option.iter self#visit_expression expr in
        List.iter visit_enumval enum.values

  method visit_toplevel decls =
    List.iter self#visit_declaration decls

end

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
      let s_test = expr_to_string test in
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
and var_to_string' d =
  let t = type_spec_to_string d.type_spec in
  let dim_iter l r = l ^ (sprintf "[%s]" (expr_to_string r)) in
  let dims = List.fold_left dim_iter "" d.array_dim in
  let init =
    match d.initval with
    | None -> ""
    | Some e -> sprintf " = %s" (expr_to_string e)
  in
  sprintf "%s %s%s%s" t dims d.name init
and var_to_string d =
  (var_to_string' d) ^ ";"
and block_item_to_string item =
  match item with
  | Statement (s) -> stmt_to_string s
  | Declarations (ds) -> List.fold_left (^) "" (List.map var_to_string ds)

let decl_to_string d =
  let params_to_string = function
    | [] -> "()"
    | p::ps ->
        let rec loop result = function
          | [] -> result
          | p::ps -> loop (sprintf "%s, %s" result (var_to_string' p)) ps
        in
        sprintf "(%s)" (loop (var_to_string' p) ps)
  in
  let block_to_string block =
    List.fold_left (^) "" (List.map block_item_to_string block)
  in
  match d with
  | Global (d) ->
      var_to_string d
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
            var_to_string d
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

let ast_to_string = function
  | ASTExpression (e) -> expr_to_string e
  | ASTStatement (s) -> stmt_to_string s
  | ASTVariable (v) -> var_to_string v
  | ASTDeclaration (d) -> decl_to_string d

let jaf_to_ain_data_type data =
  match data with
  | Untyped -> failwith "tried to convert Untyped to ain data type"
  | Unresolved (_) -> failwith "tried to covert Unresolved to ain data type"
  | Void -> Alice.Ain.Type.Void
  | Int -> Alice.Ain.Type.Int
  | Bool -> Alice.Ain.Type.Bool
  | Float -> Alice.Ain.Type.Float
  | String -> Alice.Ain.Type.String
  | Struct (_, i) -> (Alice.Ain.Type.Struct i)
  | Array (_, _) -> failwith "arrays not yet supported"
  | Wrap (_) -> failwith "wrap<...> not yet supported"
  | HLLParam -> Alice.Ain.Type.HLLParam
  | HLLFunc -> Alice.Ain.Type.HLLFunc
  | Delegate -> failwith "delegates not yet supported"

let jaf_to_ain_type spec =
  let is_ref =
    match spec.qualifier with
    | Some Ref -> true
    | _ -> false
  in
  Alice.Ain.Type.make ~is_ref (jaf_to_ain_data_type spec.data)
