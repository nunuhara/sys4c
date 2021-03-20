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

%{

open Jaf

let qtype qualifier data =
  { data=data; qualifier=qualifier }

let expr ast =
  { valuetype=None; node=ast }

let stmt ast =
  { node=ast }

let decl typespec ((name, dims), init) =
  { name=name; array_dim=dims; type_spec=typespec; initval=init } 

let decls typespec var_list =
  List.map (decl typespec) var_list

let func typespec name params body =
  { name=name; return=typespec; params=params; body=body }

%}

%token <int> I_CONSTANT
%token <float> F_CONSTANT
%token <string> C_CONSTANT
%token <string> S_CONSTANT
%token <string> IDENTIFIER
%token <string> TYPEDEF_NAME
/* arithmetic */
%token PLUS MINUS TIMES DIV MOD
/* bitwise */
%token LSHIFT RSHIFT BITAND BITOR BITXOR
/* logic/comparison */
%token AND OR LT GT LTE GTE EQUAL NEQUAL
/* unary */
%token INC DEC BITNOT LOGNOT ADDROF
/* assignment */
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODULOASSIGN
%token ORASSIGN XORASSIGN ANDASSIGN LSHIFTASSIGN RSHIFTASSIGN REFASSIGN
/* delimiters */
%token LPAREN RPAREN RBRACKET LBRACKET LBRACE RBRACE
%token QUESTION COLON SEMICOLON COMMA DOT
/* types */
%token VOID CHAR INT FLOAT BOOL STRING HLL_PARAM HLL_FUNC DELEGATE
/* keywords */
%token TRUE FALSE IF ELSE WHILE DO FOR THIS NEW TILDE
%token GOTO CONTINUE BREAK RETURN
%token CONST REF OVERRIDE ARRAY WRAP FUNCTYPE STRUCT ENUM

%token DOLLAR

%nonassoc IFX
%nonassoc ELSE

%start main
%type <declaration list> main

%%

main
  (* XXX: DOLLAR for testing only; to be removed *)
  : external_declaration+ DOLLAR { List.concat $1 }
  ;

primary_expression
  : IDENTIFIER { expr (Ident ($1)) }
  | THIS { expr This }
  | constant { expr $1 }
  | string { expr $1 }
  | LPAREN expression RPAREN { $2 }
  ;

constant
  : I_CONSTANT { ConstInt ($1) }
  | C_CONSTANT { ConstChar ($1) }
  | F_CONSTANT { ConstFloat ($1) }
  | TRUE       { ConstInt (1) }
  | FALSE      { ConstInt (0) }
  (* E_CONSTANT *)
  ;

string
  : S_CONSTANT { ConstString ($1) }
  (* FILE_MACRO *)
  (* LINE_MACRO *)
  (* FUNC_MACRO *)
  (* DATE_MACRO *)
  (* TIME_MACRO *)
  ;

postfix_expression
  : primary_expression { $1 }
  | postfix_expression LBRACKET expression RBRACKET { expr (Subscript ($1, $3)) }
  | atomic_type_specifier LPAREN expression RPAREN { expr (Cast ($1, $3)) }
  | postfix_expression arglist { expr (Call ($1, $2)) }
  | NEW IDENTIFIER arglist { expr (New (Unresolved ($2), $3)) }
  | postfix_expression DOT IDENTIFIER { expr (Member ($1, $3)) }
  | postfix_expression INC { expr (Unary (PostInc, $1)) }
  | postfix_expression DEC { expr (Unary (PostDec, $1)) }
  ;

arglist: LPAREN separated_list(COMMA, assign_expression) RPAREN { $2 }

unary_expression
  : postfix_expression { $1 }
  | INC unary_expression { expr (Unary (PreInc, $2)) }
  | DEC unary_expression { expr (Unary (PreDec, $2)) }
  | unary_operator cast_expression { expr (Unary ($1, $2)) }
  ;

unary_operator
  : PLUS { UPlus }
  | MINUS { UMinus }
  | BITNOT { BitNot }
  | LOGNOT { LogNot }
  | ADDROF { AddrOf }
  ;

cast_expression
  : unary_expression { $1 }
  | LPAREN atomic_type_specifier RPAREN cast_expression { expr (Cast ($2, $4)) }
  ;

mul_expression
  : cast_expression { $1 }
  | mul_expression TIMES cast_expression { expr (Binary (Times, $1, $3)) }
  | mul_expression DIV cast_expression { expr (Binary (Divide, $1, $3)) }
  | mul_expression MOD cast_expression { expr (Binary (Modulo, $1, $3)) }
  ;

add_expression
  : mul_expression { $1 }
  | add_expression PLUS mul_expression { expr (Binary (Plus, $1, $3)) }
  | add_expression MINUS mul_expression { expr (Binary (Minus, $1, $3)) }
  ;

shift_expression
  : add_expression { $1 }
  | shift_expression LSHIFT add_expression { expr (Binary (LShift, $1, $3)) }
  | shift_expression RSHIFT add_expression { expr (Binary (RShift, $1, $3)) }
  ;

rel_expression
  : shift_expression { $1 }
  | rel_expression LT shift_expression { expr (Binary (LT, $1, $3)) }
  | rel_expression GT shift_expression { expr (Binary (GT, $1, $3)) }
  | rel_expression LTE shift_expression { expr (Binary (LTE, $1, $3)) }
  | rel_expression GTE shift_expression { expr (Binary (GTE, $1, $3)) }
  ;

eql_expression
  : rel_expression { $1 }
  | eql_expression EQUAL rel_expression { expr (Binary (Equal, $1, $3)) }
  | eql_expression NEQUAL rel_expression { expr (Binary (NEqual, $1, $3)) }
  ;

and_expression
  : eql_expression { $1 }
  | and_expression BITAND eql_expression { expr (Binary (BitAnd, $1, $3)) }
  ;

xor_expression
  : and_expression { $1 }
  | xor_expression BITXOR and_expression { expr (Binary (BitXor, $1, $3)) }
  ;

ior_expression
  : xor_expression { $1 }
  | ior_expression BITOR xor_expression { expr (Binary (BitOr, $1, $3)) }
  ;

logand_expression
  : ior_expression { $1 }
  | logand_expression AND ior_expression { expr (Binary (LogAnd, $1, $3)) }
  ;

logor_expression
  : logand_expression { $1 }
  | logor_expression OR logand_expression { expr (Binary (LogOr, $1, $3)) }
  ;

cond_expression
  : logor_expression { $1 }
  | logor_expression QUESTION expression COLON cond_expression { expr (Ternary ($1, $3, $5)) }
  ;

assign_expression
  : cond_expression { $1 }
  | unary_expression assign_operator assign_expression { expr (Assign ($2, $1, $3)) }
  ;

assign_operator
  : ASSIGN       { EqAssign }
  | PLUSASSIGN   { PlusAssign }
  | MINUSASSIGN  { MinusAssign }
  | TIMESASSIGN  { TimesAssign }
  | DIVIDEASSIGN { DivideAssign }
  | MODULOASSIGN { ModuloAssign }
  | ORASSIGN     { OrAssign }
  | XORASSIGN    { XorAssign }
  | ANDASSIGN    { AndAssign }
  | LSHIFTASSIGN { LShiftAssign }
  | RSHIFTASSIGN { RShiftAssign }
  ;

expression
  : assign_expression { $1 }
  | expression COMMA assign_expression { expr (Seq ($1, $3)) }
  ;

constant_expression
  : cond_expression { $1 }
  ;

atomic_type_specifier
  : VOID      { Void }
  | CHAR      { Int }
  | INT       { Int }
  | FLOAT     { Float }
  | BOOL      { Bool }
  | STRING    { String }
  | HLL_PARAM { HLLParam }
  | HLL_FUNC  { HLLFunc }
  | DELEGATE  { Delegate }
  ;

type_qualifier
  : CONST { Const }
  | REF { Ref }
  | OVERRIDE { Override }
  ;

type_specifier
  : atomic_type_specifier { $1 }
  (* FIXME: this disallows arrays/wraps of ref-qualified types *)
  | ARRAY LT type_specifier GT { Array (qtype None $3, 1) }
  | ARRAY LT QUESTION GT { Array (qtype None Void, 1) }
  | WRAP LT type_specifier GT { Wrap (qtype None $3) }
  | WRAP LT QUESTION GT { Wrap (qtype None Void) }
  | IDENTIFIER { Unresolved ($1) }

statement
  : labeled_statement { stmt $1 }
  | compound_statement { stmt $1 }
  | expression_statement { stmt $1 }
  | selection_statement { stmt $1 }
  | iteration_statement { stmt $1 }
  | jump_statement { stmt $1 }
  | message_statement { stmt $1 }
  | rassign_statement { stmt $1 }
  ;

labeled_statement
  : IDENTIFIER COLON statement { Labeled ($1, $3) }
  (* case *)
  (* default *)
  ;

compound_statement
  : LBRACE RBRACE { EmptyStatement }
  | block { Compound $1 }
  ;

block_item
  : declaration { Declarations ($1) }
  | statement { Statement ($1) }
  ;

block: LBRACE nonempty_list(block_item) RBRACE { $2 }

expression_statement
  : SEMICOLON { EmptyStatement }
  | expression SEMICOLON { Expression ($1) }
  ;

selection_statement
  : IF LPAREN expression RPAREN statement %prec IFX
    { If ($3, $5, stmt EmptyStatement) }
  | IF LPAREN expression RPAREN statement ELSE statement
    { If ($3, $5, $7) }
  (* switch *)
  ;

iteration_statement
  : WHILE LPAREN expression RPAREN statement { While ($3, $5) }
  | DO statement WHILE LPAREN expression RPAREN { DoWhile ($5, $2) }
  | FOR LPAREN expression_statement expression SEMICOLON expression? RPAREN statement
    { For (Statement (stmt $3),
           $4,
           $6,
           $8)
    }
  | FOR LPAREN declaration expression SEMICOLON expression? RPAREN statement
    { For (Declarations $3,
           $4,
           $6,
           $8)
    }
  ; 

jump_statement
  : GOTO IDENTIFIER SEMICOLON { Goto ($2) }
  | CONTINUE SEMICOLON { Continue }
  | BREAK SEMICOLON { Break }
  | RETURN expression? SEMICOLON { Return ($2) }
  ;

message_statement
  : C_CONSTANT IDENTIFIER SEMICOLON { MessageCall ($1, $2) }
  ;

rassign_statement
  : expression REFASSIGN expression SEMICOLON { RefAssign ($1, $3) }

declaration
  : declaration_specifiers separated_nonempty_list(COMMA, init_declarator) SEMICOLON
    { decls $1 $2 }
  ;

declaration_specifiers
  : type_qualifier type_specifier { qtype (Some $1) $2 }
  | type_specifier { qtype None $1 }
  ;

init_declarator
  : declarator ASSIGN assign_expression { ($1, Some $3) }
  | declarator { ($1, None) }
  ;

declarator
  : IDENTIFIER { ($1, []) }
  | array_allocation { $1 }
  ;

array_allocation
  : IDENTIFIER LBRACKET expression RBRACKET { ($1, [$3]) }
  | array_allocation LBRACKET expression RBRACKET { ((fst $1), $3 :: (snd $1)) }
  ;

external_declaration
  : declaration
    { List.map (fun d -> Global (d)) $1 }
  | declaration_specifiers IDENTIFIER parameter_list block
    { [Function (func $1 $2 $3 $4)] }
  | FUNCTYPE declaration_specifiers IDENTIFIER functype_parameter_list SEMICOLON
    { [FuncType (func $2 $3 $4 [])] }
  | STRUCT IDENTIFIER LBRACE struct_declaration+ RBRACE SEMICOLON
    { [StructDef ({ name=$2; decls=(List.concat $4) })] }
  | ENUM enumerator_list SEMICOLON
    { [Enum ({ name=None; values=$2 })] }
  | ENUM IDENTIFIER enumerator_list SEMICOLON
    { [Enum ({ name=Some $2; values=$3 })] }
  ;

enumerator_list
  : LBRACE separated_nonempty_list(COMMA, enumerator) RBRACE { $2 }
  ;

enumerator
  : IDENTIFIER ASSIGN constant_expression { ($1, Some $3) }
  | IDENTIFIER { ($1, None) }
  ;

parameter_declaration
  : declaration_specifiers declarator { decl $1 ($2, None) }
  ;

parameter_list
  : LPAREN separated_list(COMMA, parameter_declaration) RPAREN { $2 }
  ;

functype_parameter_declaration
  : declaration_specifiers { decl $1 (("<anonymous>", []), None) }
  | parameter_declaration { $1 }
  ;

functype_parameter_list
  : LPAREN separated_list(COMMA, functype_parameter_declaration) RPAREN { $2 }
  ;

struct_declaration
  : declaration_specifiers separated_nonempty_list(COMMA, declarator) SEMICOLON
    { $2
      |> List.map (fun d -> (d, None))
      |> decls $1
      |> List.map (fun d -> MemberDecl (d))
    }
  | declaration_specifiers IDENTIFIER parameter_list block
    { [Method (func $1 $2 $3 $4)] }
  | IDENTIFIER LPAREN RPAREN block
    { [Constructor (func {data=Void; qualifier=None} "<constructor>" [] $4)] }
  | TILDE IDENTIFIER LPAREN RPAREN block
    { [Destructor (func {data=Void; qualifier=None} "<destructor>" [] $5)] }
  ;
