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

{
open Parser
exception Eof

(* Unquote & splice string literal *)
let process_string s =
  let rec loop s i in_text result =
    if i >= String.length s then
      String.of_seq (List.to_seq (List.rev result))
    else
      match String.get s i with
      | '"' ->
          loop s (i+1) (not in_text) result
      | '\\' ->
          let c = match String.get s (i+1) with
          | 'r' -> '\r'
          | 'n' -> '\n'
          | 't' -> '\t'
          (* TODO: warn about invalid escape sequences *)
          | a   -> a
          in
          loop s (i+2) in_text (c :: result)
      | c ->
          if in_text then
            loop s (i+1) in_text (c :: result)
          else
            loop s (i+1) in_text result
  in
  loop s 0 false []

(* Unquote & splice message *)
let process_message s =
  let rec loop s i in_text result =
    if i >= String.length s then
      String.of_seq (List.to_seq (List.rev result))
    else
      match String.get s i with
      | '\'' ->
          loop s (i+1) (not in_text) result
      | '\\' ->
          let c = match String.get s (i+1) with
          | 'r' -> '\r'
          | 'n' -> '\n'
          | 't' -> '\t'
          (* TODO: warn about invalid escape sequences *)
          | a   -> a
          in
          loop s (i+2) in_text (c :: result)
      | c ->
          if in_text then
            loop s (i+1) in_text (c :: result)
          else
            loop s (i+1) in_text result
  in
  loop s 0 false []
}

let b  = ['0' '1']
let bp = '0' ['b' 'B']
let o  = ['0'-'7']
let op = '0' ['o' 'O']
let d  = ['0'-'9']
let h  = ['a'-'f' 'A'-'F' '0'-'9']
let hp = '0' ['x' 'X']
let l  = ['a'-'z' 'A'-'Z' '_']
let a  = ['a'-'z' 'A'-'Z' '_' '0'-'9' ':' '@' '#']
let at = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let e  = ['E' 'e'] ['+' '-']? d+
let p  = ['P' 'p'] ['+' '-']? d+
let es = '\\' ( ['\'' '"' '?' '\\' 'a' 'b' 'f' 'n' 'r' 't' 'v'] | 'x' h+ )
let ws = [' ' '\t']
let sc = [^ '\\' '\n'] | es

rule token = parse
    [' ' '\t']              { token lexbuf } (* skip blanks *)
  | "//" [^ '\n']*          { token lexbuf }
  | ['\n' ]                 { Lexing.new_line lexbuf; token lexbuf }
  | d+ as n                 { I_CONSTANT(int_of_string n) }
  | (bp b+) as n            { I_CONSTANT(int_of_string n) }
  | (op o+) as n            { I_CONSTANT(int_of_string n) }
  | (hp h+) as n            { I_CONSTANT(int_of_string n) }
  | (d+ e) as n             { F_CONSTANT(float_of_string n) }
  | (d* '.' d+ e?) as n     { F_CONSTANT(float_of_string n) }
  | (d+ '.' e?) as n        { F_CONSTANT(float_of_string n) }
  | (hp h+ p) as n          { F_CONSTANT(float_of_string n) }
  | (hp h* '.' h+ p) as n   { F_CONSTANT(float_of_string n) }
  | (hp h+ '.' p) as n      { F_CONSTANT(float_of_string n) }
  | ("'" sc* "'") as s      { C_CONSTANT(process_message s) }
  | ('"' sc* '"' ws*)+ as s { S_CONSTANT(process_string s) }
  | '$'                     { DOLLAR } (* XXX: remove this later *)
  | '+'                     { PLUS }
  | '-'                     { MINUS }
  | '*'                     { TIMES }
  | '/'                     { DIV }
  | '%'                     { MOD }
  | "<<"                    { LSHIFT }
  | ">>"                    { RSHIFT }
  | '&'                     { BITAND }
  | '|'                     { BITOR }
  | '^'                     { BITXOR }
  | "&&"                    { AND }
  | "||"                    { OR }
  | '<'                     { LT }
  | '>'                     { GT }
  | "<="                    { LTE }
  | ">="                    { GTE }
  | "=="                    { EQUAL }
  | "!="                    { NEQUAL }
  | "++"                    { INC }
  | "--"                    { DEC }
  | '~'                     { BITNOT }
  | '!'                     { LOGNOT }
  | '('                     { LPAREN }
  | ')'                     { RPAREN }
  | '['                     { LBRACKET }
  | ']'                     { RBRACKET }
  | '{'                     { LBRACE }
  | '}'                     { RBRACE }
  | '&'                     { ADDROF }
  | '?'                     { QUESTION }
  | '='                     { ASSIGN }
  | "+="                    { PLUSASSIGN }
  | "-="                    { MINUSASSIGN }
  | "*="                    { TIMESASSIGN }
  | "/="                    { DIVIDEASSIGN }
  | "%="                    { MODULOASSIGN }
  | "|="                    { ORASSIGN }
  | "^="                    { XORASSIGN }
  | "&="                    { ANDASSIGN }
  | "<<="                   { LSHIFTASSIGN }
  | ">>="                   { RSHIFTASSIGN }
  | "<-"                    { REFASSIGN }
  | '.'                     { DOT }
  | ','                     { COMMA }
  | ':'                     { COLON }
  | ';'                     { SEMICOLON }
  | '~'                     { TILDE }
  | "void"                  { VOID }
  | "char"                  { CHAR }
  | "int"                   { INT }
  | "float"                 { FLOAT }
  | "bool"                  { BOOL }
  | "string"                { STRING }
  | "hll_param"             { HLL_PARAM }
  | "hll_func"              { HLL_FUNC }
  | "delegate"              { DELEGATE }
  | "true"                  { TRUE }
  | "false"                 { FALSE }
  | "if"                    { IF }
  | "else"                  { ELSE }
  | "while"                 { WHILE }
  | "do"                    { DO }
  | "for"                   { FOR }
  | "goto"                  { GOTO }
  | "continue"              { CONTINUE }
  | "break"                 { BREAK }
  | "return"                { RETURN }
  | "this"                  { THIS }
  | "new"                   { NEW }
  | "const"                 { CONST }
  | "ref"                   { REF }
  | "override"              { OVERRIDE }
  | "array"                 { ARRAY }
  | "wrap"                  { WRAP }
  | "functype"              { FUNCTYPE }
  | "struct"                { STRUCT }
  | "enum"                  { ENUM }
  | l as c                  { IDENTIFIER(String.make 1 c) } (* TODO: check_type *)
  | (l a* at) as s          { IDENTIFIER(s) } (* TODO: check_type *)
  | eof                     { raise Eof }
