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
  | Array    of type_specifier
  | Wrap     of type_specifier
  | HLLParam
  | HLLFunc
  | Delegate
  | FuncType of string * int

type ident_type =
  | LocalVariable of int
  | GlobalVariable of int
  | GlobalConstant
  | FunctionName of int
  | HLLName of int
  | System

type member_type =
  | ClassVariable of int * int
  | ClassMethod of int * int
  | HLLFunction of int * int
  | SystemFunction of Bytecode.syscall
  | BuiltinMethod of Bytecode.builtin

type call_type =
  | FunctionCall of int
  | MethodCall of int * int
  | HLLCall of int * int * int
  | SystemCall of Bytecode.syscall
  | BuiltinCall of Bytecode.builtin
  | FuncTypeCall of int

type expression = {
  mutable valuetype : Alice.Ain.Type.t option;
  mutable node : ast_expression
}
and ast_expression =
  | ConstInt    of int
  | ConstFloat  of float
  | ConstChar   of string
  | ConstString of string
  | Ident       of string     * ident_type option
  | Unary       of unary_op   * expression
  | Binary      of binary_op  * expression      * expression
  | Assign      of assign_op  * expression      * expression
  | Seq         of expression * expression
  | Ternary     of expression * expression      * expression
  | Cast        of data_type  * expression
  | Subscript   of expression * expression
  | Member      of expression * string * member_type option
  | Call        of expression * expression list * call_type option
  | New         of data_type  * expression list * int option
  | This

type block_item =
  | Statement    of statement
  | Declarations of variable list
and statement = {
  mutable node : ast_statement
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
  | MessageCall    of string          * string option * int option
  | RefAssign      of expression      * expression
and variable = {
  name      : string;
  array_dim : expression list;
  type_spec : type_specifier;
  initval   : expression option;
  mutable index : int option
}

type fundecl = {
  mutable name : string;
  return : type_specifier;
  params : variable list;
  body : block_item list;
  mutable index : int option;
  mutable class_index : int option;
  mutable super_index : int option
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
  | FuncTypeDef of fundecl
  | StructDef of structdecl
  | Enum of enumdecl

type ast_node =
  | ASTExpression of expression
  | ASTStatement of statement
  | ASTVariable of variable
  | ASTDeclaration of declaration

type context = {
  ain : Alice.Ain.t;
  import_ain : Alice.Ain.t;
  mutable const_vars : variable list
}

type resolved_name =
  | ResolvedLocal of variable
  | ResolvedConstant of variable
  | ResolvedGlobal of Alice.Ain.Variable.t
  | ResolvedFunction of int
  | ResolvedLibrary of int
  | ResolvedSystem
  | UnresolvedName

class ivisitor ctx = object (self)

  val environment = object (self)
    val mutable stack = []
    val mutable variables = []
    val mutable current_function = None

    method push =
      stack <- variables :: stack

    method pop =
      match stack with
      | [] ->
          failwith "visitor tried to pop root environment"
      | prev::rest ->
          variables <- prev;
          stack <- rest

    method push_var decl =
      variables <- decl :: variables

    method enter_function decl =
      self#push;
      current_function <- Some decl;
      List.iter decl.params ~f:self#push_var

    method leave_function =
      self#pop;
      current_function <- None

    method current_function = current_function

    method current_class =
      match current_function with
      | Some f -> f.class_index
      | None -> None

    method get_local name =
      let var_eq _ (v : variable) = String.equal v.name name in
      let rec search vars rest =
        match List.findi vars ~f:var_eq with
        | Some (_, v) -> Some v
        | None ->
            begin match rest with
            | [] -> None
            | prev::rest -> search prev rest
            end
      in
      search variables stack

    method resolve name =
      let ain_resolve ain =
        match Alice.Ain.get_global ain name with
        | Some g -> ResolvedGlobal g
        | None ->
            begin match Alice.Ain.get_function ain name with
            | Some f -> ResolvedFunction f.index
            | None ->
                begin match Alice.Ain.get_library_index ain name with
                | Some i -> ResolvedLibrary i
                | None -> UnresolvedName
                end
            end
      in
      match name with
      | "system" ->
          (* NOTE: on ain v11+, "system" is a library *)
          if Alice.Ain.version_gte ctx.ain 11 0 then
            begin match Alice.Ain.get_library_index ctx.ain "system" with
            | Some i -> ResolvedLibrary i
            | None -> UnresolvedName
            end
          else
            ResolvedSystem
      | "super" ->
          begin match current_function with
          | Some { super_index=Some super_no; _} -> ResolvedFunction super_no
          | _ -> UnresolvedName
          end
      | _ ->
          begin match self#get_local name with
          | Some v -> ResolvedLocal v
          | None ->
              begin match List.findi ctx.const_vars ~f:(fun _ (v:variable) -> String.equal v.name name) with
              | Some (_, v) -> ResolvedConstant v
              | None ->
                  begin match ain_resolve ctx.ain with
                  | UnresolvedName ->
                      (* Try to import declaration from import_ain *)
                      begin match ain_resolve ctx.import_ain with
                      | ResolvedGlobal g ->
                          let no = Alice.Ain.write_new_global ctx.ain g in
                          ResolvedGlobal (Alice.Ain.get_global_by_index ctx.ain no)
                      | ResolvedFunction i ->
                          let f = Alice.Ain.get_function_by_index ctx.import_ain i in
                          Alice.Ain.Function.set_undefined f;
                          ResolvedFunction (Alice.Ain.write_new_function ctx.ain f)
                      | ResolvedLibrary _ ->
                          failwith "importing of libraries not implemented"
                      | ResolvedLocal _ | ResolvedConstant _ | ResolvedSystem ->
                          failwith "ain_resolve returned invalid result"
                      | UnresolvedName -> UnresolvedName
                      end
                  | result -> result
                  end
              end
          end
  end

  method visit_expression (e : expression) =
    match e.node with
    | ConstInt (_) -> ()
    | ConstFloat (_) -> ()
    | ConstChar (_) -> ()
    | ConstString (_) -> ()
    | Ident (_, _) -> ()
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
    | Member (obj, _, _) ->
        self#visit_expression obj
    | Call (f, args, _) ->
        self#visit_expression f;
        List.iter args ~f:self#visit_expression
    | New (_, args, _) ->
        List.iter args ~f:self#visit_expression
    | This -> ()

  method visit_statement (s : statement) =
    match s.node with
    | EmptyStatement -> ()
    | Expression (e) ->
        self#visit_expression e
    | Compound (items) ->
        environment#push;
        List.iter items ~f:self#visit_block_item;
        environment#pop
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
        environment#push;
        self#visit_block_item init;
        self#visit_expression test;
        Option.iter inc ~f:self#visit_expression;
        self#visit_statement body;
        environment#pop
    | Goto (_) -> ()
    | Continue -> ()
    | Break -> ()
    | Return (e) ->
        Option.iter e ~f:self#visit_expression
    | MessageCall (_, _, _) -> ()
    | RefAssign (a, b) ->
        self#visit_expression a;
        self#visit_expression b

  method visit_local_variable v =
    List.iter v.array_dim ~f:self#visit_expression;
    Option.iter v.initval ~f:self#visit_expression;
    environment#push_var v

  method visit_block_item item =
    match item with
    | Statement (s) -> self#visit_statement s
    | Declarations (ds) -> List.iter ds ~f:self#visit_local_variable

  method visit_fundecl f =
    environment#enter_function f;
    List.iter f.body ~f:self#visit_block_item;
    environment#leave_function

  method visit_declaration d =
    let visit_vardecl d =
      List.iter d.array_dim ~f:self#visit_expression;
      Option.iter d.initval ~f:self#visit_expression
    in
    match d with
    | Global (g) -> visit_vardecl g
    | Function (f) ->
        self#visit_fundecl f;
    | FuncTypeDef (_) -> ()
    | StructDef (s) ->
        let visit_structdecl = function
          | MemberDecl (d) -> visit_vardecl d
          | Constructor (f) -> self#visit_fundecl f
          | Destructor (f) -> self#visit_fundecl f
          | Method (f) -> self#visit_fundecl f
        in
        List.iter s.decls ~f:visit_structdecl
    | Enum (enum) ->
        let visit_enumval (_, expr) = Option.iter expr ~f:self#visit_expression in
        List.iter enum.values ~f:visit_enumval

  method visit_toplevel decls =
    List.iter decls ~f:self#visit_declaration

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
  | Unresolved s -> "Unresolved<" ^ s ^ ">"
  | Void -> "void"
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Struct (s, _) -> s
  | Array t -> "array<" ^ (type_spec_to_string t) ^ ">" (* TODO: rank *)
  | Wrap t -> "wrap<" ^ (type_spec_to_string t) ^ ">"
  | HLLParam -> "hll_param"
  | HLLFunc -> "hll_func"
  | Delegate -> "delegate"
  | FuncType (s, _) -> s
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
  | Ident (s, _) ->
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
  | Member (e, s, _) ->
      sprintf "%s.%s" (expr_to_string e) s
  | Call (f, args, _) ->
      sprintf "%s%s" (expr_to_string f) (arglist_to_string args)
  | New (t, args, _) ->
      sprintf "new %s%s" (data_type_to_string t) (arglist_to_string args)
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
      |> List.map ~f:block_item_to_string
      |> List.fold ~init:"" ~f:(^)
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
  | MessageCall (msg, f, _) ->
      begin match f with
      | Some name -> sprintf "'%s' %s;" msg name
      | None      -> sprintf "'%s';" msg
      end
  | RefAssign (dst, src) ->
      sprintf "%s <- %s;" (expr_to_string dst) (expr_to_string src)
and var_to_string' d =
  let t = type_spec_to_string d.type_spec in
  let dim_iter l r = l ^ (sprintf "[%s]" (expr_to_string r)) in
  let dims = List.fold d.array_dim ~init:"" ~f:dim_iter in
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
  | Declarations (ds) -> List.fold (List.map ds ~f:var_to_string) ~init:"" ~f:(^)

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
    List.fold (List.map block ~f:block_item_to_string) ~init:"" ~f:(^)
  in
  match d with
  | Global (d) ->
      var_to_string d
  | Function (d) ->
      let return = type_spec_to_string d.return in
      let params = params_to_string d.params in
      let body = block_to_string d.body in
      sprintf "%s %s%s { %s }" return d.name params body
  | FuncTypeDef (d) ->
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
      let body = List.fold (List.map d.decls ~f:sdecl_to_string) ~init:"" ~f:(^) in
      sprintf "struct %s { %s };" d.name body
  | Enum (d) ->
      let enumval_to_string = function
        | (s, None) -> s
        | (s, Some e) -> sprintf "%s = %s" s (expr_to_string e)
      in
      let enumvals_fold l r = l ^ ", " ^ r in
      let body = List.fold (List.map d.values ~f:enumval_to_string) ~init:"" ~f:enumvals_fold in
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

let rec jaf_to_ain_data_type data =
  match data with
  | Untyped -> failwith "tried to convert Untyped to ain data type"
  | Unresolved _ -> failwith "tried to covert Unresolved to ain data type"
  | Void -> Alice.Ain.Type.Void
  | Int -> Alice.Ain.Type.Int
  | Bool -> Alice.Ain.Type.Bool
  | Float -> Alice.Ain.Type.Float
  | String -> Alice.Ain.Type.String
  | Struct (_, i) -> (Alice.Ain.Type.Struct i)
  | Array t -> Alice.Ain.Type.Array (jaf_to_ain_type t)
  | Wrap t -> Alice.Ain.Type.Wrap (jaf_to_ain_type t)
  | HLLParam -> Alice.Ain.Type.HLLParam
  | HLLFunc -> Alice.Ain.Type.HLLFunc
  | Delegate -> failwith "delegates not yet supported"
  | FuncType (_, i) -> Alice.Ain.Type.FuncType i
and jaf_to_ain_type spec =
  let is_ref =
    match spec.qualifier with
    | Some Ref -> true
    | _ -> false
  in
  Alice.Ain.Type.make ~is_ref (jaf_to_ain_data_type spec.data)

let jaf_to_ain_parameters j_p =
  let rec convert_params (params:variable list) (result:Alice.Ain.Variable.t list) =
    match params with
    | [] -> List.rev result
    | x::xs ->
        let var = Alice.Ain.Variable.make_local x.name (jaf_to_ain_type x.type_spec) in
        begin match x.type_spec with
        | { data=(Int|Bool|Float|FuncType(_,_)); qualifier=Some Ref } ->
            let void = Alice.Ain.Variable.make_local "<void>" (Alice.Ain.Type.make Void) in
            convert_params xs (void::var::result)
        | _ ->
            convert_params xs (var::result)
        end
  in
  convert_params j_p []

let jaf_to_ain_function j_f (a_f:Alice.Ain.Function.t) =
  a_f.vars <- jaf_to_ain_parameters j_f.params;
  a_f.nr_args <- List.length a_f.vars;
  a_f.return_type <- jaf_to_ain_type j_f.return;
  a_f

let jaf_to_ain_struct j_s (a_s:Alice.Ain.Struct.t) =
  let filter_members = function
    | MemberDecl (v) -> Some (Alice.Ain.Variable.make_member v.name (jaf_to_ain_type v.type_spec))
    | _ -> None
  in
  (* TODO: interfaces *)
  (* TODO: constructor *)
  (* TODO: destructor *)
  a_s.members <- List.filter_map j_s.decls ~f:filter_members;
  (* TODO: vmethods *)
  a_s

let jaf_to_ain_functype j_f (a_f:Alice.Ain.FunctionType.t) =
  a_f.variables <- jaf_to_ain_parameters j_f.params;
  a_f.nr_arguments <- List.length a_f.variables;
  a_f.return_type <- jaf_to_ain_type j_f.return;
  a_f
