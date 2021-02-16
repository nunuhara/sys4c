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
  | Void -> "void"
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Struct (s) -> s
  | Array (t, _) -> "array<" ^ (type_spec_to_string t) ^ ">" (* TODO: rank *)
  | Wrap (t) -> "wrap<" ^ (type_spec_to_string t) ^ ">"
  | HLLParam -> "hll_param"
  | HLLFunc -> "hll_func"
  | Delegate -> "delegate"
and type_spec_to_string ts =
  match ts.qualifier with
  | Some q -> (type_qualifier_to_string q) ^ " " ^ (data_type_to_string ts.data)
  | None -> (data_type_to_string ts.data)

let rec print_expr (e : expression) =
  let print_arglist args =
    print_string "(";
    begin match args with
      | [] -> ()
      | fst::[] -> print_expr fst
      | fst::tail ->
          print_expr fst;
          List.iter (fun e -> print_string ","; print_expr e) tail
    end;
    print_string ")"
  in

  match e.node with
  | ConstInt (i) -> print_int i
  | ConstFloat (f) -> print_float f
  | ConstChar (c) -> print_string ("'" ^ c ^ "'")
  | ConstString (s) -> print_string ("\"" ^ s ^ "\"")
  | Ident (name) -> print_string name
  | Unary (op, a) ->
     (match op with
      | PostInc -> print_expr a; print_string (unary_op_to_string op)
      | PostDec -> print_expr a; print_string (unary_op_to_string op)
      | _ -> print_string (unary_op_to_string op); print_expr a)
  | Binary (op, a, b) ->
      print_expr a;
      print_string (binary_op_to_string op);
      print_expr b
  | Assign (op, a, b) ->
      print_expr a;
      print_string (assign_op_to_string op);
      print_expr b
  | Seq (a, b) ->
      print_expr a;
      print_string ",";
      print_expr b
  | Ternary (cond, cons, alt) ->
      print_expr cond;
      print_string "?";
      print_expr cons;
      print_string ":";
      print_expr alt
  | Cast (t, e) ->
      print_string "(";
      print_string (data_type_to_string t);
      print_string ")";
      print_expr e
  | Subscript (obj, i) ->
      print_expr obj;
      print_string "[";
      print_expr i;
      print_string "]"
  | Member (obj, name) ->
      print_expr obj;
      print_string ("." ^ name)
  | Call (f, args) ->
      print_expr f;
      print_arglist args
  | New (t, args) ->
      print_string "new ";
      print_string t;
      print_arglist args
  | This ->
      print_string "this"

let rec print_stmt s =
  match s.node with
  | EmptyStatement ->
      print_string ";"
  | Expression (e) ->
      print_expr e;
      print_string ";"
  | Compound (lst) ->
      print_string "{";
      List.iter print_block_item lst;
      print_string "}"
  | Labeled (label, stmt) ->
      print_string (label ^ ": ");
      print_stmt stmt
  | If (test, cons, alt) ->
      print_string "if(";
      print_expr test;
      print_string ")";
      print_stmt cons;
      begin match alt.node with
      | Jaf.EmptyStatement -> ()
      | _ ->
          print_string "else ";
          print_stmt alt
      end
  | While (test, stmt) ->
      print_string "while(";
      print_expr test;
      print_string")";
      print_stmt stmt
  | DoWhile (test, stmt) ->
      print_string "do ";
      print_stmt stmt;
      print_string " while(";
      print_expr test;
      print_string ")"
  | For (init, test, inc, body) ->
      print_string "for(";
      print_block_item init;
      print_stmt test;
      Option.iter print_expr inc;
      print_string ")";
      print_stmt body
  | Goto (label) ->
      print_string ("goto " ^ label ^ ";")
  | Continue ->
      print_string "continue;"
  | Break ->
      print_string "break;"
  | Return (Some e) ->
      print_string "return ";
      print_expr e;
      print_string ";"
  | Return (None) ->
      print_string "return;"
  | MessageCall (msg, f) ->
      print_string ("'" ^ msg ^ "'" ^ f ^ ";")
  | RefAssign (lhs, rhs) ->
      print_expr lhs;
      print_string "<-";
      print_expr rhs
and print_decl d =
  print_string ((type_spec_to_string d.type_spec) ^ " " ^ d.name);
  Option.iter (fun e -> print_string " = "; print_expr e) d.initval
and print_block_item = function
  | Statement (s) -> print_stmt s
  | Declarations (ds) -> List.iter (fun d -> print_decl d; print_string ";") ds

let print_params params =
  print_string "(";
  begin match params with
  | [] -> ()
  | fst::[] -> print_decl fst
  | fst::tail ->
      print_decl fst;
      List.iter (fun d -> print_string ","; print_decl d) tail
  end;
  print_string ")"

let print_function_body body =
  print_string "{";
  List.iter print_block_item body;
  print_string "}"

let print_external_declaration = function
  | Global (g) ->
      print_decl g;
      print_string ";"
  | Function (f) ->
      print_string ((type_spec_to_string f.return) ^ " " ^ f.name);
      print_params f.params;
      print_function_body f.body
  | FuncType (f) ->
      print_string ("functype " ^ (type_spec_to_string f.return) ^ " " ^ f.name);
      print_params f.params;
      print_string ";"
  | Struct (s) ->
      let print_struct_decl = function
        | MemberDecl (d) ->
            print_decl d;
            print_string ";"
        | Constructor (f) ->
            print_string s.name;
            print_params f.params;
            print_function_body f.body
        | Destructor (f) ->
            print_string ("~" ^ s.name);
            print_params f.params;
            print_function_body f.body
        | Method (f) ->
            print_string ((type_spec_to_string f.return) ^ " " ^ s.name ^ "@" ^ f.name);
            print_params f.params;
            print_function_body f.body
      in
      print_string ("struct " ^ s.name ^ " {");
      List.iter print_struct_decl s.decls;
      print_string "};"
  | Enum (e) ->
      let print_enum_value = function
        | (s, Some e) -> print_string (s ^ " "); print_expr e; print_string ","
        | (s, None) -> print_string (s ^ ",")
      in
      print_string "enum ";
      Option.iter (fun s -> print_string (s ^ " ")) e.name;
      print_string "{";
      List.iter print_enum_value e.values;
      print_string "}"

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      print_string "-> ";
      List.iter print_external_declaration result;
      print_newline();
      flush stdout
    done
  with Lexer.Eof ->
    exit 0
