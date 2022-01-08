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
open Bytecode

class jaf_compiler ain = object (self)

  (* The function currently being compiled. *)
  val mutable current_function : Alice.Ain.Function.t option = None
  (* The bytecode output buffer. *)
  val mutable buffer = Alice.Buffer.create 2048
  (* Address of the start of the current buffer. *)
  val mutable start_address : int = 0
  (* Current address within the code section. *)
  val mutable current_address : int = 0

  method write_instruction0 op =
    Alice.Buffer.write_int16 buffer (int_of_opcode op);
    current_address <- current_address + 2

  method write_instruction1 op arg0 =
    Alice.Buffer.write_int16 buffer (int_of_opcode op);
    Alice.Buffer.write_int32 buffer arg0;
    current_address <- current_address + 6

  method write_instruction1_float op arg0 =
    Alice.Buffer.write_int16 buffer (int_of_opcode op);
    Alice.Buffer.write_float buffer arg0;
    current_address <- current_address + 6

  method write_instruction2 op arg0 arg1 =
    Alice.Buffer.write_int16 buffer (int_of_opcode op);
    Alice.Buffer.write_int32 buffer arg0;
    Alice.Buffer.write_int32 buffer arg1;
    current_address <- current_address + 10

  method write_address_at dst addr =
    Alice.Buffer.write_int32_at buffer (dst - start_address) addr

  method write_buffer =
    Alice.Ain.append_bytecode ain buffer;
    Alice.Buffer.clear buffer;
    start_address <- current_address

  method get_local i =
    match current_function with
    | Some f -> List.nth f.vars i
    | None   -> failwith "get_local outside of function"

  (** Emit the code to put the value of a variable onto the stack (including
      member variables and array elements). Assumes a page + page-index is
      already on the stack. *)
  method compile_dereference (t:Alice.Ain.Type.t) =
    match t.data with
    | Int | Float | Bool | LongInt | FuncType _ ->
        if t.is_ref then
          self#write_instruction0 REFREF;
        self#write_instruction0 REF
    | String ->
        if Alice.Ain.version_gte ain 11 0 then
          begin
            self#write_instruction0 REF;
            self#write_instruction0 A_REF
          end
        else
          self#write_instruction0 S_REF
    | Array _ ->
        self#write_instruction0 REF;
        self#write_instruction0 A_REF
    | Struct no ->
        if Alice.Ain.version_gte ain 11 0 then
          begin
            self#write_instruction0 REF;
            self#write_instruction0 A_REF
          end
        else
          self#write_instruction1 SR_REF no
    | Void | IMainSystem | Delegate _ | HLLParam | Wrap _ | Option _
    | Unknown87 _ | IFace | Enum2 _ | Enum _ | HLLFunc | IFaceWrap
    | Function _ ->
        failwith "dereference not supported for type"

  (** Emit the code to put a location (variable, struct member, or array
      element) onto the stack, e.g. to prepare for an assignment or to pass
      a variable by reference. *)
  method compile_lvalue (e : expression) =
    let compile_lvalue_after (t : Alice.Ain.Type.t) =
      if t.is_ref then
        match t.data with
        | Int | Float | Bool | LongInt ->
            self#write_instruction0 REFREF
        | String ->
            if not (Alice.Ain.version_gte ain 14 0) then
              self#write_instruction0 REF
        | Array _ | Struct _ ->
            self#write_instruction0 REF
        | _ -> ()
      else
        match t.data with
        | String ->
            if not (Alice.Ain.version_gte ain 14 0) then
              self#write_instruction0 REF
        | _ -> ()
    in
    match e.node with
    | Ident (_, Some (LocalVariable i)) ->
        let v = self#get_local i in
        self#write_instruction0 PUSHLOCALPAGE;
        self#write_instruction1 PUSH i;
        compile_lvalue_after v.value_type
    | Ident (_, Some (GlobalVariable i)) ->
        let v = Alice.Ain.get_global_by_index ain i in
        self#write_instruction0 PUSHGLOBALPAGE;
        self#write_instruction1 PUSH i;
        compile_lvalue_after v.value_type
    | Member (_, _) ->
        failwith "struct member lvalue not implemented"
    | Subscript (_, _) ->
        failwith "subscript lvalue not implemented"
    | _ ->
        failwith "invalid lvalue"

  (** Emit the code to pop a value off the stack. *)
  method compile_pop (t:Alice.Ain.Type.t) =
    match t.data with
    | Void ->
        ()
    | Int | Float | Bool | LongInt ->
        self#write_instruction0 POP
    | String ->
        self#write_instruction0 S_POP
    | Struct _
    | IMainSystem
    | FuncType _
    | Delegate _
    | HLLParam
    | Array _
    | Wrap _
    | Option _
    | Unknown87 _
    | IFace
    | Enum2 _
    | Enum _
    | HLLFunc
    | IFaceWrap
    | Function _ ->
        failwith "compile_pop: unsupported value type"

  (** Emit the code to compute an expression. Computing an expression produces
      a value (of the expression's value type) on the stack. *)
  method compile_expression (expr:expression) =
    match expr.node with
    | ConstInt i ->
        self#write_instruction1 PUSH i
    | ConstFloat f ->
        self#write_instruction1_float F_PUSH f
    | ConstChar _ ->
        failwith "char constants not supported"
    | ConstString _ ->
        (* TODO: look up string in string table (add if needed) *)
        failwith "strings not supported"
    | Ident (_, Some (LocalVariable i)) ->
        self#write_instruction0 PUSHLOCALPAGE;
        self#write_instruction1 PUSH i;
        self#compile_dereference (self#get_local i).value_type
    | Ident (_, Some (GlobalVariable i)) ->
        self#write_instruction0 PUSHGLOBALPAGE;
        self#write_instruction1 PUSH i;
        self#compile_dereference (Alice.Ain.get_global_by_index ain i).value_type
    | Ident (_, Some GlobalConstant) ->
        failwith "global constant not eliminated"
    | Ident (_, Some (FunctionName _)) ->
        failwith "tried to compile function identifier"
    | Ident (_, Some (HLLName _)) ->
        failwith "tried to compile HLL identifier"
    | Ident (_, None) ->
        failwith "identifier type is none"
    | Unary (UPlus, e) ->
        self#compile_expression e
    | Unary (UMinus, e) ->
        self#compile_expression e;
        self#write_instruction0 INV
    | Unary (LogNot, e) ->
        self#compile_expression e;
        self#write_instruction0 NOT
    | Unary (BitNot, e) ->
        self#compile_expression e;
        self#write_instruction0 COMPL
    | Unary (AddrOf, _) ->
        failwith "function types not supported"
    | Unary (PreInc, e) ->
        self#compile_lvalue e;
        self#write_instruction0 INC
    | Unary (PreDec, e) ->
        self#compile_lvalue e;
        self#write_instruction0 DEC
    | Unary (PostInc, e) ->
        self#compile_lvalue e;
        if Alice.Ain.version_gte ain 14 0 then
          begin
            self#write_instruction1 X_DUP 2;
            self#write_instruction1 X_REF 1;
            self#write_instruction2 X_MOV 3 1;
            self#write_instruction0 INC
          end
        else
          begin
            self#write_instruction0 DUP2;
            self#write_instruction0 REF;
            self#write_instruction0 DUP_X2;
            self#write_instruction0 POP;
            self#write_instruction0 INC
          end
    | Unary (PostDec, e) ->
        self#compile_lvalue e;
        if Alice.Ain.version_gte ain 14 0 then
          begin
            self#write_instruction1 X_DUP 2;
            self#write_instruction1 X_REF 1;
            self#write_instruction2 X_MOV 3 1;
            self#write_instruction0 DEC
          end
        else
          begin
            self#write_instruction0 DUP2;
            self#write_instruction0 REF;
            self#write_instruction0 DUP_X2;
            self#write_instruction0 POP;
            self#write_instruction0 DEC
          end
    | Binary (LogOr, _, _) ->
        failwith "logical-or not implemented"
    | Binary (LogAnd, _, _) ->
        failwith "logical-and not implemented"
    | Binary (op, a, b) ->
        self#compile_expression a;
        self#compile_expression b;
        begin match ((Option.get a.valuetype).data, op) with
        | (Int, Plus)      -> self#write_instruction0 ADD
        | (Int, Minus)     -> self#write_instruction0 SUB
        | (Int, Times)     -> self#write_instruction0 MUL
        | (Int, Divide)    -> self#write_instruction0 DIV
        | (Int, Modulo)    -> self#write_instruction0 MOD
        | (Int, Equal)     -> self#write_instruction0 EQUALE
        | (Int, NEqual)    -> self#write_instruction0 NOTE
        | (Int, LT)        -> self#write_instruction0 LT
        | (Int, GT)        -> self#write_instruction0 GT
        | (Int, LTE)       -> self#write_instruction0 LTE
        | (Int, GTE)       -> self#write_instruction0 GTE
        | (Int, BitOr)     -> self#write_instruction0 OR
        | (Int, BitXor)    -> self#write_instruction0 XOR
        | (Int, BitAnd)    -> self#write_instruction0 AND
        | (Int, LShift)    -> self#write_instruction0 LSHIFT
        | (Int, RShift)    -> self#write_instruction0 RSHIFT
        | (Int, (LogOr|LogAnd)) ->
            failwith "invalid integer operator"
        | (Float, Plus)    -> self#write_instruction0 F_ADD
        | (Float, Minus)   -> self#write_instruction0 F_SUB
        | (Float, Times)   -> self#write_instruction0 F_MUL
        | (Float, Divide)  -> self#write_instruction0 F_DIV
        | (Float, Equal)   -> self#write_instruction0 F_EQUALE
        | (Float, NEqual)  -> self#write_instruction0 F_NOTE
        | (Float, LT)      -> self#write_instruction0 F_LT
        | (Float, GT)      -> self#write_instruction0 F_GT
        | (Float, LTE)     -> self#write_instruction0 F_LTE
        | (Float, GTE)     -> self#write_instruction0 F_GTE
        | (Float, (Modulo|BitOr|BitXor|BitAnd|LShift|RShift|LogOr|LogAnd)) ->
            failwith "invalid floating point operator"
        | (String, Plus)   -> self#write_instruction0 S_ADD
        | (String, Equal)  -> self#write_instruction0 S_EQUALE
        | (String, NEqual) -> self#write_instruction0 S_NOTE
        | (String, LT)     -> self#write_instruction0 S_LT
        | (String, GT)     -> self#write_instruction0 S_GT
        | (String, LTE)    -> self#write_instruction0 S_LTE
        | (String, GTE)    -> self#write_instruction0 S_GTE
        | (String, Modulo) ->
            let int_of_t (t : Alice.Ain.Type.data) =
              match t with
              | Int    -> 2
              | Float  -> 3
              | String -> 4
              | _ -> failwith "invalid type for string formatting"
            in
            self#write_instruction1 S_MOD (int_of_t (Option.get b.valuetype).data)
        | (String, (Minus|Times|Divide|BitOr|BitXor|BitAnd|LShift|RShift|LogOr|LogAnd)) ->
            failwith "invalid string operator"
        | _ -> failwith "invalid binary expression"
        end
    | Assign (op, lhs, rhs) ->
        self#compile_lvalue lhs;
        self#compile_expression rhs;
        begin match ((Option.get lhs.valuetype).data, op) with
        | ((Int|Bool), EqAssign)     -> self#write_instruction0 ASSIGN
        | ((Int|Bool), PlusAssign)   -> self#write_instruction0 PLUSA
        | ((Int|Bool), MinusAssign)  -> self#write_instruction0 MINUSA
        | ((Int|Bool), TimesAssign)  -> self#write_instruction0 MULA
        | ((Int|Bool), DivideAssign) -> self#write_instruction0 DIVA
        | ((Int|Bool), ModuloAssign) -> self#write_instruction0 MODA
        | ((Int|Bool), OrAssign)     -> self#write_instruction0 ORA
        | ((Int|Bool), XorAssign)    -> self#write_instruction0 XORA
        | ((Int|Bool), AndAssign)    -> self#write_instruction0 ANDA
        | ((Int|Bool), LShiftAssign) -> self#write_instruction0 LSHIFTA
        | ((Int|Bool), RShiftAssign) -> self#write_instruction0 RSHIFTA
        | (Float, EqAssign)          -> self#write_instruction0 F_ASSIGN
        | (Float, PlusAssign)        -> self#write_instruction0 F_PLUSA
        | (Float, MinusAssign)       -> self#write_instruction0 F_MINUSA
        | (Float, TimesAssign)       -> self#write_instruction0 F_MULA
        | (Float, DivideAssign)      -> self#write_instruction0 F_DIVA
        | (String, EqAssign)         -> self#write_instruction0 S_ASSIGN
        | (String, PlusAssign)       -> self#write_instruction0 S_PLUSA
        | (_, _) -> failwith "invalid assignment"
        end
    | Seq (a, b) ->
        self#compile_expression a;
        self#compile_pop (Option.get a.valuetype);
        self#compile_expression b
    | Ternary (test, con, alt) ->
        self#compile_expression test;
        let ifz_addr = current_address + 2 in
        self#write_instruction1 IFZ 0;
        self#compile_expression con;
        let jump_addr = current_address + 2 in
        self#write_instruction1 JUMP 0;
        self#write_address_at ifz_addr current_address;
        self#compile_expression alt;
        self#write_address_at jump_addr current_address
    | Cast (_, e) ->
        let dst_t = (Option.get expr.valuetype).data in
        let src_t = (Option.get e.valuetype).data in
        self#compile_expression e;
        begin match src_t with
        | Int ->
            begin match dst_t with
            | Int -> ()
            | Float ->
                self#write_instruction0 ITOF
            | String ->
                self#write_instruction0 I_STRING
            | _ ->
                failwith "invalid cast"
            end
        | Float ->
            begin match dst_t with
            | Float -> ()
            | Int ->
                self#write_instruction0 FTOI
            | String ->
                self#write_instruction1 PUSH 6;
                self#write_instruction0 FTOS
            | _ ->
                failwith "invalid cast"
            end
        | String ->
            begin match dst_t with
            | String -> ()
            | Int ->
                self#write_instruction0 STOI
            | _ ->
                failwith "invalid cast"
            end
        | _ ->
            failwith "invalid cast"
        end
    | Subscript (_, _) ->
        failwith "subscript not implemented"
    | Member (_, _) ->
        failwith "member not implemented"
    | Call (_, _) ->
        failwith "call not implemented"
    | New (_, _, _) ->
        failwith "new not implemented"
    | This ->
        self#write_instruction0 PUSHSTRUCTPAGE

  (** Emit the code for a statement. Statements are stack-neutral, i.e. the
      state of the stack is unchanged after executing a statement. *)
  method compile_statement (stmt:statement) =
    match stmt.node with
    | EmptyStatement ->
        ()
    | Expression e ->
        self#compile_expression e;
        self#compile_pop (Option.get e.valuetype)
    | Compound items ->
        self#compile_block items
    | Labeled (_, _) ->
        failwith "labels not implemented"
    | If (test, con, alt) ->
        self#compile_expression test;
        let ifnz_addr = current_address + 2 in
        let jump_addr = current_address + 8 in
        self#write_instruction1 IFNZ 0;
        self#write_instruction1 JUMP 0;
        self#write_address_at ifnz_addr current_address;
        self#compile_statement con;
        begin match alt.node with
        | EmptyStatement ->
            self#write_address_at jump_addr current_address
        | _ ->
            let skip_addr = current_address + 2 in
            self#write_instruction1 JUMP 0;
            self#write_address_at jump_addr current_address;
            self#compile_statement alt;
            self#write_address_at skip_addr current_address
        end
    | While (test, body) ->
        (* loop test *)
        let loop_addr = current_address in
        self#compile_expression test;
        let break_addr = current_address + 2 in
        self#write_instruction1 IFZ 0;
        (* loop body *)
        self#compile_statement body;
        self#write_instruction1 JUMP loop_addr;
        (* loop end *)
        self#write_address_at break_addr current_address
    | DoWhile (test, body) ->
        (* skip loop test *)
        let jump_addr = current_address + 2 in
        self#write_instruction1 JUMP 0;
        (* loop test *)
        let loop_addr = current_address in
        self#compile_expression test;
        let break_addr = current_address + 2 in
        self#write_instruction1 IFZ 0;
        (* loop body *)
        self#write_address_at jump_addr current_address;
        self#compile_statement body;
        self#write_instruction1 JUMP loop_addr;
        (* loop end *)
        self#write_address_at break_addr current_address;
    | For (decl, test, inc, body) ->
        (* loop init *)
        self#compile_block [decl];
        (* loop test *)
        let test_addr = current_address in
        self#compile_expression test;
        let break_addr = current_address + 2 in
        self#write_instruction1 IFZ 0;
        let body_addr = current_address + 2 in
        self#write_instruction1 JUMP 0;
        (* loop increment *)
        let loop_addr = current_address in
        begin match inc with
        | Some e ->
            self#compile_expression e;
            self#compile_pop (Option.get e.valuetype);
        | None -> ()
        end;
        self#write_instruction1 JUMP test_addr;
        (* loop body *)
        self#write_address_at body_addr current_address;
        self#compile_statement body;
        self#write_instruction1 JUMP loop_addr;
        (* loop end *)
        self#write_address_at break_addr current_address
    | Goto _ ->
        failwith "goto not implemented"
    | Continue ->
        failwith "continue not implemented"
    | Break ->
        failwith "break not implemented"
    | Return None ->
        self#write_instruction0 RETURN
    | Return (Some e) ->
        self#compile_expression e;
        self#write_instruction0 RETURN
    | MessageCall (_, _) ->
        failwith "message calls not implemented"
    | RefAssign (_, _) ->
        failwith "reference assignment not implemented"

  (** Emit the code for a variable declaration. If the variable has an initval,
      the initval expression is computed and assigned to the variable.
      Otherwise a default value is assigned. *)
  method compile_variable (decl:variable) =
    match decl.type_spec.qualifier with
    | Some Const -> ()
    | _ ->
        let push_local i =
          self#write_instruction0 PUSHLOCALPAGE;
          self#write_instruction1 PUSH i
        in
        let v = self#get_local (Option.get decl.index) in
        if v.value_type.is_ref then
          failwith "ref types not implemented"
        else
          begin match v.value_type.data with
          | Int | Bool | FuncType _ ->
              push_local v.index;
              begin match decl.initval with
              | Some e -> self#compile_expression e
              | None -> self#write_instruction1 PUSH 0
              end;
              self#write_instruction0 ASSIGN;
              self#write_instruction0 POP
          | LongInt ->
              push_local v.index;
              begin match decl.initval with
              | Some e -> self#compile_expression e
              | None -> self#write_instruction1 PUSH 0
              end;
              self#write_instruction0 LI_ASSIGN;
              self#write_instruction0 POP
          | Float ->
              push_local v.index;
              begin match decl.initval with
              | Some e -> self#compile_expression e
              | None -> self#write_instruction1 F_PUSH 0
              end;
              self#write_instruction0 F_ASSIGN;
              self#write_instruction0 POP
          | String ->
              push_local v.index;
              if Alice.Ain.version_gte ain 14 0 then
                begin
                  self#write_instruction1 X_DUP 2;
                  self#write_instruction1 X_REF 1;
                  self#write_instruction0 DELETE;
                  begin match decl.initval with
                  | Some e -> self#compile_expression e
                  | None -> self#write_instruction1 S_PUSH 0
                  end;
                  self#write_instruction1 X_ASSIGN 1;
                  self#write_instruction0 POP
                end
              else
                begin
                  self#write_instruction0 REF;
                  begin match decl.initval with
                  | Some e -> self#compile_expression e
                  | None -> self#write_instruction1 S_PUSH 0
                  end;
                  self#write_instruction0 S_ASSIGN;
                  if Alice.Ain.version_gte ain 11 0 then
                    self#write_instruction0 DELETE
                  else
                    self#write_instruction0 S_POP
                end
          | Struct no ->
              (* FIXME: use verbose versions *)
              self#write_instruction1 SH_LOCALDELETE v.index;
              self#write_instruction2 SH_LOCALCREATE v.index no
          | Array _ ->
              let has_dims = (List.length decl.array_dim) > 0 in
              push_local v.index;
              if Alice.Ain.version_gte ain 14 0 then
                begin
                  self#write_instruction1 X_DUP 2;
                  self#write_instruction1 X_REF 1;
                  self#write_instruction0 DELETE;
                  begin match decl.initval with
                  | Some e ->
                      if has_dims then
                        failwith "initializer provided for array with explicit dimensions";
                      self#compile_expression e;
                      self#write_instruction1 X_ASSIGN 1;
                      self#write_instruction0 POP
                  | None ->
                      self#write_instruction1 PUSH 0;
                      self#write_instruction1 X_A_INIT 0;
                      self#write_instruction0 POP;
                      if has_dims then
                        failwith "v14+ array alloc not implemented";
                  end
                end
              else if Alice.Ain.version_gte ain 11 0 then
                begin
                  self#write_instruction0 REF;
                  begin match decl.initval with
                  | Some e ->
                      if has_dims then
                        failwith "initializer provided for array with explicit dimensions";
                      self#compile_expression e;
                      self#write_instruction0 X_SET;
                      self#write_instruction0 DELETE
                  | None ->
                      if has_dims then
                        failwith "v11+ array alloc not implemented"
                      else
                        failwith "v11+ array alloc not implemented"
                  end
                end
              else
                begin
                  if has_dims then
                    begin
                      let push_dim (e:expression) =
                        match e.node with
                        | ConstInt i -> self#write_instruction1 PUSH i
                        | _ -> failwith "array dimension is not an integer constant"
                      in
                      (* TODO: verify that nested types are arrays *)
                      List.iter push_dim decl.array_dim;
                      self#write_instruction1 PUSH (List.length decl.array_dim);
                      self#write_instruction0 A_ALLOC
                    end
                  else
                    self#write_instruction0 A_FREE
                end
          | Void | IMainSystem | Delegate _ | HLLParam | Wrap _ | Option _
          | Unknown87 _ | IFace | Enum2 _ | Enum _ | HLLFunc | IFaceWrap
          | Function _ ->
              failwith "unimplemented variable type"
          end

  (** Emit the code for a block of statements. *)
  method compile_block (items:block_item list) =
    let compile_item = function
      | Statement stmt -> self#compile_statement stmt
      | Declarations vars -> List.iter self#compile_variable vars
    in
    List.iter compile_item items

  (** Emit the code for a default return value. *)
  method compile_nullexpr (t:Alice.Ain.Type.t) =
    if t.is_ref then
      match t.data with
      | String | Struct _ | Array _ ->
          self#write_instruction1 PUSH (-1)
      | Int | Float | Bool | LongInt ->
          self#write_instruction1 PUSH (-1);
          self#write_instruction1 PUSH 0
      | _ -> failwith "nullexpr not implemented for ref type"
    else
      match t.data with
      | Void -> ()
      | Int | Bool | LongInt ->
          self#write_instruction1 PUSH 0
      | Float ->
          self#write_instruction1 F_PUSH 0
      | String ->
          self#write_instruction1 S_PUSH 0
      | Struct _ | Array _ ->
          self#write_instruction1 PUSH (-1)
      | _ -> failwith "nullexpr not implemented for type"

  (** Emit the code for a function. *)
  method compile_function (decl:fundecl) =
    let index = Option.get decl.index in
    let func = Alice.Ain.get_function_by_index ain index in
    (* TODO: create buffer to write bytecode into *)
    start_address <- current_address;
    func.address <- current_address + 6;
    current_function <- Some func;
    self#write_instruction1 FUNC index;
    self#compile_block decl.body;
    self#compile_nullexpr (Option.get current_function).return_type;
    self#write_instruction0 RETURN;
    self#write_instruction1 ENDFUNC index;
    Alice.Ain.Function.write ain func;
    current_function <- None

  (** Compile a list of declarations. *)
  method compile (decls:declaration list) =
    let compile_decl = function
      | Function f ->
          begin match f.return.qualifier with
          | Some Override ->
              failwith "function overrides not implemented"
          | _ ->
              self#compile_function f
          end
      | Global g ->
          begin match g.type_spec.qualifier with
          | Some Const -> ()
          | _ ->
              begin match g.initval with
              | Some _ -> failwith "global initvals not implemented"
              | None -> ()
              end
          end
      | FuncTypeDef _ -> ()
      | StructDef d ->
          let compile_struct_decl (d : struct_declaration) =
            match d with
            | MemberDecl _ -> () (* TODO: member initvals? *)
            | Constructor _ -> failwith "constructors not implemented"
            | Destructor _ -> failwith "destructors not implemented"
            | Method _ -> failwith "methods not implemented"
          in
          List.iter compile_struct_decl d.decls
      | Enum _ ->
          (* TODO: built-in enum methods *)
          failwith "enums not implemented"
    in
    List.iter compile_decl decls;
    self#write_buffer
end
