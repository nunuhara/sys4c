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
open Error

type loop = {
  mutable loop_addr : int option;
  mutable break_addrs : int list
}

type scope = {
  mutable vars : Alice.Ain.Variable.t list;
  mutable labels : (string * int) list;
  mutable gotos : (string * int * statement) list
}

class jaf_compiler ain = object (self)

  (* The function currently being compiled. *)
  val mutable current_function : Alice.Ain.Function.t option = None
  (* The bytecode output buffer. *)
  val mutable buffer = Alice.Buffer.create 2048
  (* Address of the start of the current buffer. *)
  val mutable start_address : int = 0
  (* Current address within the code section. *)
  val mutable current_address : int = 0
  (* The currently active loops. *)
  val loops = Stack.create ()
  (* The currentl active scopes. *)
  val scopes = Stack.create ()

  (** Begin a scope. Variables created within a scope are deleted when the
      scope ends. *)
  method start_scope =
    Stack.push { vars=[]; labels=[]; gotos=[] } scopes

  (** End a scope. Deletes variable created within the scope. *)
  method end_scope =
    let scope = Stack.pop scopes in
    (* update goto addresses *)
    let rec update_gotos gotos unresolved =
      match gotos with
      | ((name, addr_loc, _) as goto)::rest ->
          begin match List.find_opt (fun (label_name, _) -> name = label_name) scope.labels with
          | Some (_, addr) ->
              self#write_address_at addr_loc addr;
              update_gotos rest unresolved
          | None ->
              update_gotos rest (goto::unresolved)
          end
      | [] -> unresolved
    in
    let unresolved = update_gotos scope.gotos [] in
    (* unresolved gotos are moved to parent scope *)
    begin match (Stack.top_opt scopes, unresolved) with
    | (_, []) -> ()
    | (None, (_, _, stmt)::_) ->
        compile_error "Unresolved label" (ASTStatement stmt)
    | (Some parent, unresolved) ->
        parent.gotos <- List.append parent.gotos unresolved
    end;
    (* delete scope-local variables *)
    (* NOTE: Variables are deleted automatically by the VM upon function
             return. This code is emitted only to ensure that destructors are
             called at the correct time; hence function-scoped variables need
             not be deleted here. *)
    begin match Stack.top_opt scopes with
    | None -> ()
    | Some _ ->
        let delete_var (v:Alice.Ain.Variable.t) =
          match v.value_type.data with
          | Struct _ ->
              self#compile_local_delete v.index
          | Array t ->
              if Alice.Ain.version_gte ain 11 0 then begin
                let type_no = Alice.Ain.Type.to_c_data ain t in
                self#compile_local_ref v.index;
                self#write_instruction0 REF;
                self#compile_CALLHLL "Array" "Free" type_no (ASTStatement {node=EmptyStatement})
              end else begin
                self#compile_local_ref v.index;
                self#write_instruction0 A_FREE
              end
          | _ -> ()
        in
        List.iter delete_var (List.rev scope.vars)
    end

  (** Add a variable to the current scope. *)
  method scope_add_var v =
    match Stack.top_opt scopes with
    | Some scope -> scope.vars <- v::scope.vars
    | None -> compiler_bug "tried to add variable to null scope" None

  (** Add a label to the current scope. *)
  method scope_add_label name =
    let scope = Stack.top scopes in
    scope.labels <- (name, current_address)::scope.labels

  (** Add a goto address location to the current scope *)
  method scope_add_goto name addr_loc stmt =
    let scope = Stack.top scopes in
    scope.gotos <- (name, addr_loc, stmt)::scope.gotos

  (** Begin a loop. *)
  method start_loop addr =
    Stack.push { loop_addr=Some addr; break_addrs=[] } loops

  (** End the current loop. Updates 'break' addresses. *)
  method end_loop =
    let loop = Stack.pop loops in
    List.iter (fun addr -> self#write_address_at addr current_address) loop.break_addrs

  (** Retrieves the continue address for the current loop (i.e. the address
      that 'continue' statements should jump to). *)
  method get_continue_addr node =
    match Stack.top_opt loops with
    | Some { loop_addr=Some addr; _} -> addr
    | _ -> compile_error "'continue' statement outside of loop" node

  (** Push the location of a 32-bit integer that should be updated to the
      address of the current scope's end point. *)
  method push_break_addr addr node =
    match Stack.top_opt loops with
    | Some loop -> loop.break_addrs <- addr::loop.break_addrs
    | None -> compile_error "'break' statement outside of loop" node

  method compile_CALLHLL lib_name fun_name t parent =
    match Alice.Ain.get_library_index ain lib_name with
    | Some lib_no ->
        begin match Alice.Ain.get_library_function_index ain lib_no fun_name with
        | Some fun_no ->
            self#write_instruction3 CALLHLL lib_no fun_no t
        | None ->
            compile_error "No HLL function found for built-in" parent
        end
    | None ->
        compile_error "No HLL library found for built-in" parent

  method write_instruction0 op =
    match (Alice.Ain.version_gte ain 14 0, op) with
    | (true, REF)    -> self#write_instruction1 X_REF 1
    | (true, REFREF) -> self#write_instruction1 X_REF 2
    | (true, DUP)    -> self#write_instruction1 X_DUP 1
    | (true, DUP2)   -> self#write_instruction1 X_DUP 2
    | (true, ASSIGN) -> self#write_instruction1 X_ASSIGN 1
    | _ ->
        Alice.Buffer.write_int16 buffer (int_of_opcode op);
        current_address <- current_address + 2

  method write_instruction1 op arg0 =
    match (Alice.Ain.version_lt ain 8 0, op) with
    | (true, S_MOD) ->
        self#write_instruction1 PUSH arg0;
        self#write_instruction0 S_MOD
    | _ ->
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

  method write_instruction3 op arg0 arg1 arg2 =
    Alice.Buffer.write_int16 buffer (int_of_opcode op);
    Alice.Buffer.write_int32 buffer arg0;
    Alice.Buffer.write_int32 buffer arg1;
    Alice.Buffer.write_int32 buffer arg2;
    current_address <- current_address + 14

  method write_address_at dst addr =
    Alice.Buffer.write_int32_at buffer (dst - start_address) addr

  method write_buffer =
    if current_address > start_address then
      begin
        Alice.Ain.append_bytecode ain buffer;
        Alice.Buffer.clear buffer;
        start_address <- current_address
      end

  method get_local i =
    match current_function with
    | Some f -> List.nth f.vars i
    | None   -> compiler_bug "get_local outside of function" None

  method compile_lock_peek =
    if Alice.Ain.version_lt ain 6 0 then
      begin
        self#write_instruction1 CALLSYS (int_of_syscall LockPeek);
        self#write_instruction0 POP
      end

  method compile_unlock_peek =
    if Alice.Ain.version_lt ain 6 0 then
      begin
        self#write_instruction1 CALLSYS (int_of_syscall UnlockPeek);
        self#write_instruction0 POP
      end

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
        compiler_bug "dereference not supported for type" None

  method compile_local_ref i =
    self#write_instruction0 PUSHLOCALPAGE;
    self#write_instruction1 PUSH i

  method compile_global_ref i =
    self#write_instruction0 PUSHGLOBALPAGE;
    self#write_instruction1 PUSH i

  method compile_identifier_ref id_type =
    match id_type with
    | LocalVariable i -> self#compile_local_ref (self#get_local i).index
    | GlobalVariable i -> self#compile_global_ref (Alice.Ain.get_global_by_index ain i).index
    | _ -> compiler_bug "Invalid identifier type" None

  method compile_variable_ref (e:expression) =
    match e.node with
    | Ident (_, Some id_type) ->
        self#compile_identifier_ref id_type
    | Member (e, _, Some (ClassVariable (_, member_no))) ->
        self#compile_lvalue e;
        self#write_instruction1 PUSH member_no
    | Subscript (obj, index) ->
        self#compile_lvalue obj;
        self#compile_expression index
    | _ ->
        compiler_bug "Invalid variable ref" (Some(ASTExpression e))

  method compile_delete_ref =
    self#write_instruction0 DUP2;
    self#write_instruction0 REF;
    self#write_instruction0 DELETE

  method compile_local_delete i =
    self#compile_local_ref i;
    self#compile_delete_ref;
    self#write_instruction1 PUSH (-1);
    self#write_instruction0 ASSIGN;
    self#write_instruction0 POP

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
        | Array _ | Struct _ ->
            self#write_instruction0 REF
        | _ -> ()
    in
    match e.node with
    | Ident (_, Some (LocalVariable i)) ->
        let v = self#get_local i in
        self#compile_local_ref v.index;
        compile_lvalue_after v.value_type
    | Ident (_, Some (GlobalVariable i)) ->
        let v = Alice.Ain.get_global_by_index ain i in
        self#compile_global_ref v.index;
        compile_lvalue_after v.value_type
    | Member (obj, _, Some (ClassVariable (_, member_no))) ->
        self#compile_lvalue obj;
        self#write_instruction1 PUSH member_no;
        compile_lvalue_after (Option.get e.valuetype)
    | Subscript (obj, index) ->
        self#compile_lvalue obj;
        self#compile_expression index;
        compile_lvalue_after (Option.get e.valuetype)
    | New (Struct(_, s_no), args, Some var_no) ->
        let s = Alice.Ain.Struct.of_int ain s_no in
        (* delete dummy variable *)
        self#write_instruction0 PUSHLOCALPAGE;
        self#write_instruction1 PUSH var_no;
        self#write_instruction0 REF;
        self#write_instruction0 DELETE;
        (* prepare for assign to dummy variable *)
        self#write_instruction0 PUSHLOCALPAGE;
        self#write_instruction1 PUSH var_no;
        (* call constructor (via NEW) *)
        if s.constructor >= 0 then
          self#compile_function_arguments args (Alice.Ain.Function.of_int ain s.constructor);
        self#compile_lock_peek;
        if Alice.Ain.version_gte ain 11 0 then
          self#write_instruction2 NEW s_no s.constructor
        else
          self#write_instruction1 NEW s_no;
        (* assign to dummy variable *)
        self#write_instruction0 ASSIGN;
        self#compile_unlock_peek
    | This ->
        self#compile_expression e
    | _ ->
        compiler_bug ("invalid lvalue: " ^ (expr_to_string e)) (Some (ASTExpression e))

  (** Emit the code to pop a value off the stack. *)
  method compile_pop (t:Alice.Ain.Type.t) =
    match t.data with
    | Void ->
        ()
    | Int | Float | Bool | LongInt ->
        self#write_instruction0 POP
    | String ->
        if Alice.Ain.version_gte ain 11 0 then
          self#write_instruction0 DELETE
        else
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
        compiler_bug "compile_pop: unsupported value type" None

  method compile_argument (expr:expression) (t:Alice.Ain.Type.t) =
    if t.is_ref then
      begin
        self#compile_lvalue expr;
        (* XXX: in 14+ there is a distinction between a string lvalue and a
                reference argument (string lvalue is a page+index, reference
                argument is just the string page) *)
        if Alice.Ain.version_gte ain 14 0 then
          begin match (Option.get expr.valuetype).data with
          | String -> self#write_instruction1 X_REF 1
          | _ -> ()
          end
      end
    else
      self#compile_expression expr

  method compile_function_arguments args (f:Alice.Ain.Function.t) =
    let compile_arg arg (var:Alice.Ain.Variable.t) =
      self#compile_argument arg var.value_type
    in
    List.iter2 compile_arg args f.vars

  (** Emit the code to call a method. The object upon which the method is to be
      called should already be on the stack before this code is executed. *)
  method compile_method_call args method_no =
    let f = Alice.Ain.get_function_by_index ain method_no in
    if Alice.Ain.version_gte ain 11 0 then
      begin
        self#write_instruction1 PUSH method_no;
        self#compile_function_arguments args f;
        (* TODO: should this be (List.length f.args) or (List.length args)? *)
        self#write_instruction1 CALLMETHOD (List.length args)
      end
    else
      begin
        self#compile_function_arguments args f;
        self#write_instruction1 CALLMETHOD method_no
      end

  (** Emit the code to compute an expression. Computing an expression produces
      a value (of the expression's value type) on the stack. *)
  method compile_expression (expr:expression) =
    match expr.node with
    | ConstInt i ->
        self#write_instruction1 PUSH i
    | ConstFloat f ->
        self#write_instruction1_float F_PUSH f
    | ConstChar s ->
        if String.length s != 1 then
          compile_error "Invalid character constant" (ASTExpression expr)
        else
          self#write_instruction1 PUSH (int_of_char (String.get s 0))
    | ConstString s ->
        (* FIXME: need to SJIS-encode string *)
        let no = Alice.Ain.add_string ain s in
        self#write_instruction1 S_PUSH no
    | Ident (_, Some (LocalVariable i)) ->
        self#write_instruction0 PUSHLOCALPAGE;
        self#write_instruction1 PUSH i;
        self#compile_dereference (self#get_local i).value_type
    | Ident (_, Some (GlobalVariable i)) ->
        self#write_instruction0 PUSHGLOBALPAGE;
        self#write_instruction1 PUSH i;
        self#compile_dereference (Alice.Ain.get_global_by_index ain i).value_type
    | Ident (_, Some GlobalConstant) ->
        compiler_bug "global constant not eliminated" (Some(ASTExpression expr))
    | Ident (_, Some (FunctionName _)) ->
        compiler_bug "tried to compile function identifier" (Some(ASTExpression expr))
    | Ident (_, Some (HLLName _)) ->
        compiler_bug "tried to compile HLL identifier" (Some(ASTExpression expr))
    | Ident (_, Some System) ->
        compiler_bug "tried to compile system identifier" (Some(ASTExpression expr))
    | Ident (_, None) ->
        compiler_bug "identifier type is none" (Some(ASTExpression expr))
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
    | Unary (AddrOf, e) ->
        begin match (Option.get e.valuetype).data with
        | Function no -> self#write_instruction1 PUSH no
        | _ -> compiler_bug "invalid type for & operator" (Some(ASTExpression expr))
        end
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
    | Binary (LogOr, a, b) ->
        self#compile_expression a;
        let lhs_true_addr = current_address + 2 in
        self#write_instruction1 IFNZ 0;
        self#compile_expression b;
        let rhs_true_addr = current_address + 2 in
        self#write_instruction1 IFNZ 0;
        self#write_instruction1 PUSH 0;
        let false_addr = current_address + 2 in
        self#write_instruction1 JUMP 0;
        self#write_address_at lhs_true_addr current_address;
        self#write_address_at rhs_true_addr current_address;
        self#write_instruction1 PUSH 1;
        self#write_address_at false_addr current_address
    | Binary (LogAnd, a, b) ->
        self#compile_expression a;
        let lhs_false_addr = current_address + 2 in
        self#write_instruction1 IFZ 0;
        self#compile_expression b;
        let rhs_false_addr = current_address + 2 in
        self#write_instruction1 IFZ 0;
        self#write_instruction1 PUSH 1;
        let true_addr = current_address + 2 in
        self#write_instruction1 JUMP 0;
        self#write_address_at lhs_false_addr current_address;
        self#write_address_at rhs_false_addr current_address;
        self#write_instruction1 PUSH 0;
        self#write_address_at true_addr current_address
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
            compiler_bug "invalid integer operator" (Some(ASTExpression expr))
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
            compiler_bug "invalid floating point operator" (Some(ASTExpression expr))
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
              | _ -> compiler_bug "invalid type for string formatting" (Some(ASTExpression expr))
            in
            self#write_instruction1 S_MOD (int_of_t (Option.get b.valuetype).data)
        | (String, (Minus|Times|Divide|BitOr|BitXor|BitAnd|LShift|RShift|LogOr|LogAnd)) ->
            compiler_bug "invalid string operator" (Some(ASTExpression expr))
        | _ -> compiler_bug "invalid binary expression" (Some(ASTExpression expr))
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
        | (_, _) -> compiler_bug "invalid assignment" (Some(ASTExpression expr))
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
        begin match (src_t, dst_t) with
        | (Int, Int) -> ()
        | (Int, Float) ->
            self#write_instruction0 ITOF
        | (Int, String) ->
            self#write_instruction0 I_STRING
        | (Float, Float) -> ()
        | (Float, Int) ->
            self#write_instruction0 FTOI
        | (Float, String) ->
            self#write_instruction1 PUSH 6;
            self#write_instruction0 FTOS
        | (String, String) -> ()
        | (String, Int) ->
            self#write_instruction0 STOI
        | _ ->
            compiler_bug "invalid cast" (Some(ASTExpression expr))
        end;
    | Subscript (obj, index) ->
        self#compile_lvalue obj;
        self#compile_expression index;
        self#compile_dereference (Option.get expr.valuetype)
    | Member (e, _, Some (ClassVariable (struct_no, member_no))) ->
        let struct_type = Alice.Ain.get_struct_by_index ain struct_no in
        self#compile_lvalue e;
        self#write_instruction1 PUSH member_no;
        self#compile_dereference (List.nth struct_type.members member_no).value_type
    | Member (_, _, Some (ClassMethod (_, _))) ->
        compiler_bug "tried to compile method member expression" (Some(ASTExpression expr))
    | Member (_, _, Some (HLLFunction (_, _))) ->
        compiler_bug "tried to compile HLL member expression" (Some(ASTExpression expr))
    | Member (_, _, Some (SystemFunction _)) ->
        compiler_bug "tried to compile system call member expression" (Some(ASTExpression expr))
    | Member (_, _, Some (BuiltinMethod _)) ->
        compiler_bug "tried to compile built-in method member expression" (Some(ASTExpression expr))
    | Member (_, _, None) ->
        compiler_bug "member expression has no member_type" (Some(ASTExpression expr))
    (* regular function call *)
    | Call (_, args, Some FunctionCall function_no) ->
        let f = Alice.Ain.get_function_by_index ain function_no in
        self#compile_function_arguments args f;
        self#write_instruction1 CALLFUNC function_no
    (* method call *)
    | Call ({node=Member(e, _, _); _}, args, Some MethodCall (_, method_no)) ->
        self#compile_lvalue e;
        self#compile_method_call args method_no
    (* HLL function call *)
    | Call (_, args, Some HLLCall (lib_no, fun_no, type_param)) ->
        let f = Alice.Ain.function_of_hll_function_index ain lib_no fun_no in
        self#compile_function_arguments args f;
        if Alice.Ain.version_gte ain 11 0 then
          self#write_instruction3 CALLHLL lib_no fun_no type_param
        else
          self#write_instruction2 CALLHLL lib_no fun_no
    (* system call *)
    | Call (_, args, Some SystemCall sys) ->
        if Alice.Ain.version_gte ain 11 0 then
          compiler_bug "attempted to compile old-style system call in ain v11+" (Some(ASTExpression expr))
        else
          begin
            let f = function_of_syscall sys in
            self#compile_function_arguments args f;
            self#write_instruction1 CALLSYS f.index
          end
    (* built-in call *)
    | Call ({node=Member(e, _, _); _}, args, Some BuiltinCall builtin) ->
        if Alice.Ain.version_gte ain 11 0 then
          compiler_bug "tried to compile old-style built-in call in ain v11+" (Some(ASTExpression expr));
        let f = function_of_builtin builtin in
        begin match builtin with
        | IntString | FloatString | StringInt | StringLength | StringLengthByte
        | StringEmpty | StringFind | StringGetPart | StringPushBack
        | StringPopBack | StringErase ->
            self#compile_expression e
        | ArrayAlloc | ArrayRealloc | ArrayFree | ArrayNumof | ArrayCopy
        | ArrayFill | ArrayPushBack | ArrayPopBack | ArrayEmpty | ArrayErase
        | ArrayInsert | ArraySort ->
            self#compile_variable_ref e
        end;
        self#compile_function_arguments args f;
        begin match builtin with
        | IntString ->
            self#write_instruction0 I_STRING
        | FloatString ->
            self#write_instruction1 PUSH 6;
            self#write_instruction0 ITOF
        | StringInt ->
            self#write_instruction0 STOI
        (* FIXME: if `e` is an lvalue, can use S_LENGTH instead of S_LENGTH2, etc. *)
        | StringLength ->
            self#write_instruction0 S_LENGTH2
        | StringLengthByte ->
            self#write_instruction0 S_LENGTHBYTE2
        | StringEmpty ->
            self#write_instruction0 S_EMPTY
        | StringFind ->
            self#write_instruction0 S_FIND
        | StringGetPart ->
            self#write_instruction0 S_GETPART
        | StringPushBack ->
            self#write_instruction0 S_PUSHBACK2
        | StringPopBack ->
            self#write_instruction0 S_POPBACK2
        | StringErase ->
            self#write_instruction0 S_ERASE2
        | ArrayAlloc ->
            (* FIXME: this built-in should be variadic *)
            self#write_instruction1 PUSH 1;
            self#write_instruction0 A_ALLOC
        | ArrayRealloc ->
            (* FIXME: this built-in should be variadic *)
            self#write_instruction1 PUSH 1;
            self#write_instruction0 A_REALLOC
        | ArrayFree ->
            self#write_instruction0 A_FREE
        | ArrayNumof ->
            (* FIXME: this built-in should accept an optional argument *)
            self#write_instruction1 PUSH 1;
            self#write_instruction0 A_NUMOF
        | ArrayCopy ->
            self#write_instruction0 A_COPY
        | ArrayFill ->
            self#write_instruction0 A_FILL
        | ArrayPushBack ->
            self#write_instruction0 A_PUSHBACK
        | ArrayPopBack ->
            self#write_instruction0 A_POPBACK
        | ArrayEmpty ->
            self#write_instruction0 A_EMPTY
        | ArrayErase ->
            self#write_instruction0 A_ERASE
        | ArrayInsert ->
            self#write_instruction0 A_INSERT
        | ArraySort ->
            self#write_instruction0 A_SORT
        end
    (* functype call *)
    | Call (e, args, Some FuncTypeCall no) ->
        let compile_arg (arg:expression) (var:Alice.Ain.Variable.t) =
          self#compile_argument arg var.value_type;
          if var.value_type.is_ref then
            begin
              self#write_instruction0 DUP2_X1;
              self#write_instruction0 POP;
              self#write_instruction0 POP
            end
          else
            self#write_instruction0 SWAP
        in
        let f = Alice.Ain.FunctionType.of_int ain no in
        self#compile_expression e;
        List.iter2 compile_arg args f.variables;
        self#write_instruction1 PUSH no;
        self#write_instruction0 CALLFUNC2
    | Call (_, _, _) ->
        compiler_bug "invalid call expression" (Some(ASTExpression expr))
    | New (_, _, _) ->
        self#compile_lvalue expr;
        self#write_instruction0 A_REF
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
    | Labeled (name, s) ->
        self#scope_add_label name;
        self#compile_statement s
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
        self#start_loop loop_addr;
        self#compile_expression test;
        let break_addr = current_address + 2 in
        self#write_instruction1 IFZ 0;
        (* loop body *)
        self#compile_statement body;
        self#write_instruction1 JUMP loop_addr;
        (* loop end *)
        self#write_address_at break_addr current_address;
        self#end_loop
    | DoWhile (test, body) ->
        (* skip loop test *)
        let jump_addr = current_address + 2 in
        self#write_instruction1 JUMP 0;
        (* loop test *)
        let loop_addr = current_address in
        self#start_loop loop_addr;
        self#compile_expression test;
        let break_addr = current_address + 2 in
        self#write_instruction1 IFZ 0;
        (* loop body *)
        self#write_address_at jump_addr current_address;
        self#compile_statement body;
        self#write_instruction1 JUMP loop_addr;
        (* loop end *)
        self#write_address_at break_addr current_address;
        self#end_loop
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
        self#start_loop loop_addr;
        (* self#set_continue_addr loop_addr; *)
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
        self#write_address_at break_addr current_address;
        self#end_loop
    | Goto name ->
        self#scope_add_goto name (current_address + 2) stmt;
        self#write_instruction1 JUMP 0
    | Continue ->
        self#write_instruction1 JUMP (self#get_continue_addr (ASTStatement stmt))
    | Break ->
        self#push_break_addr (current_address + 2) (ASTStatement stmt);
        self#write_instruction1 JUMP 0
    | Return None ->
        self#write_instruction0 RETURN
    | Return (Some e) ->
        begin match (Option.get current_function).return_type with
        | { is_ref=true; data=(Int|Float|Bool|LongInt|FuncType _) } ->
            self#compile_lvalue e;
            self#write_instruction0 DUP_U2;
            self#write_instruction0 SP_INC
        | { is_ref=true; data=(String|Struct _|Array _) } ->
            self#compile_lvalue e;
            self#write_instruction0 DUP;
            self#write_instruction0 SP_INC
        | { is_ref=true; _} ->
            compile_error "return statement not implemented for ref type" (ASTStatement stmt)
        | _ ->
            self#compile_expression e;
        end;
        self#write_instruction0 RETURN
    | MessageCall (msg, _, fno) ->
        (* FIXME: need to SJIS-encode message *)
        let msg_no = Alice.Ain.add_message ain msg in
        self#write_instruction1 MSG msg_no;
        begin match fno with
        | Some no -> self#write_instruction1 CALLFUNC no
        | None -> ()
        end
    | RefAssign (lhs, rhs) ->
        self#compile_lock_peek;
        self#compile_variable_ref lhs;
        self#compile_delete_ref;
        self#compile_lvalue rhs;
        begin match (Option.get lhs.valuetype).data with
        | Int | Bool | Float | LongInt | FuncType _ ->
            (* NOTE: SDK compiler emits [DUP_U2; SP_INC; R_ASSIGN; POP; POP] here *)
            self#write_instruction0 R_ASSIGN;
            self#write_instruction0 POP;
            self#write_instruction0 SP_INC
        | String | Struct _ | Array _ ->
            (* NOTE: SDK compiler emits [DUP; SP_INC; ASSIGN; POP] here *)
            self#write_instruction0 ASSIGN;
            self#write_instruction0 SP_INC
        | _ ->
            compiler_bug "Invalid LHS in reference assignment" (Some(ASTStatement stmt))
        end;
        self#compile_unlock_peek

  (** Emit the code for a variable declaration. If the variable has an initval,
      the initval expression is computed and assigned to the variable.
      Otherwise a default value is assigned. *)
  method compile_variable_declaration (decl:variable) =
    match decl.type_spec.qualifier with
    | Some Const -> ()
    | _ ->
        self#scope_add_var (List.nth (Option.get current_function).vars (Option.get decl.index));
        let v = self#get_local (Option.get decl.index) in
        if v.value_type.is_ref then
          begin match v.value_type.data with
          | Int | Bool | Float | LongInt | FuncType _ ->
              self#compile_lock_peek;
              self#compile_local_ref v.index;
              self#compile_delete_ref;
              self#write_instruction1 PUSH (-1);
              self#write_instruction1 PUSH 0;
              self#write_instruction0 R_ASSIGN;
              self#write_instruction0 POP;
              self#write_instruction0 POP;
              self#compile_unlock_peek
          | String | Struct _ ->
              self#compile_lock_peek;
              self#compile_local_delete v.index;
              self#compile_unlock_peek
          | _ -> compile_error "This type of reference variable not implemented" (ASTVariable decl)
          end
        else
          begin match v.value_type.data with
          | Int | Bool | FuncType _ ->
              self#compile_local_ref v.index;
              begin match decl.initval with
              | Some e -> self#compile_expression e
              | None -> self#write_instruction1 PUSH 0
              end;
              self#write_instruction0 ASSIGN;
              self#write_instruction0 POP
          | LongInt ->
              self#compile_local_ref v.index;
              begin match decl.initval with
              | Some e -> self#compile_expression e
              | None -> self#write_instruction1 PUSH 0
              end;
              self#write_instruction0 LI_ASSIGN;
              self#write_instruction0 POP
          | Float ->
              self#compile_local_ref v.index;
              begin match decl.initval with
              | Some e -> self#compile_expression e
              | None -> self#write_instruction1 F_PUSH 0
              end;
              self#write_instruction0 F_ASSIGN;
              self#write_instruction0 POP
          | String ->
              self#compile_local_ref v.index;
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
          | Array t ->
              let has_dims = (List.length decl.array_dim) > 0 in
              self#compile_local_ref v.index;
              if Alice.Ain.version_gte ain 14 0 then
                begin
                  self#write_instruction1 X_DUP 2;
                  self#write_instruction1 X_REF 1;
                  self#write_instruction0 DELETE;
                  begin match decl.initval with
                  | Some e ->
                      self#compile_expression e;
                      self#write_instruction1 X_ASSIGN 1;
                      self#write_instruction0 POP
                  | None ->
                      self#write_instruction1 PUSH 0;
                      self#write_instruction1 X_A_INIT 0;
                      self#write_instruction0 POP;
                      if has_dims then
                        begin
                          self#compile_local_ref v.index;
                          self#write_instruction0 REF;
                          self#compile_expression (List.hd decl.array_dim);
                          self#compile_CALLHLL "Array" "Alloc" 1 (ASTVariable decl)
                        end
                  end
                end
              else if Alice.Ain.version_gte ain 11 0 then
                begin
                  self#write_instruction0 REF;
                  begin match decl.initval with
                  | Some e ->
                      self#compile_expression e;
                      self#write_instruction0 X_SET;
                      self#write_instruction0 DELETE
                  | None ->
                      let type_no = Alice.Ain.Type.to_c_data ain t in
                      if has_dims then
                        begin
                          self#write_instruction0 DUP;
                          self#compile_expression (List.hd decl.array_dim);
                          self#write_instruction1 PUSH (-1);
                          self#write_instruction1 PUSH (-1);
                          self#write_instruction1 PUSH (-1);
                          self#compile_CALLHLL "Array" "Alloc" type_no (ASTVariable decl)
                        end
                      else
                        self#compile_CALLHLL "Array" "Free" type_no (ASTVariable decl)
                  end
                end
              else
                begin
                  if has_dims then
                    begin
                      List.iter self#compile_expression decl.array_dim;
                      self#write_instruction1 PUSH (List.length decl.array_dim);
                      self#write_instruction0 A_ALLOC
                    end
                  else
                    self#write_instruction0 A_FREE
                end
          | Void | IMainSystem | Delegate _ | HLLParam | Wrap _ | Option _
          | Unknown87 _ | IFace | Enum2 _ | Enum _ | HLLFunc | IFaceWrap
          | Function _ ->
              compile_error "Unimplemented variable type" (ASTVariable decl)
          end

  (** Emit the code for a block of statements. *)
  method compile_block (items:block_item list) =
    let compile_item = function
      | Statement stmt -> self#compile_statement stmt
      | Declarations vars -> List.iter self#compile_variable_declaration vars
    in
    self#start_scope;
    List.iter compile_item items;
    self#end_scope

  (** Emit the code for a default return value. *)
  method compile_default_return (t:Alice.Ain.Type.t) decl =
    if t.is_ref then
      match t.data with
      | String | Struct _ | Array _ ->
          self#write_instruction1 PUSH (-1)
      | Int | Float | Bool | LongInt ->
          self#write_instruction1 PUSH (-1);
          self#write_instruction1 PUSH 0
      | _ -> compile_error "default return value not implemented for ref type" decl
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
      | _ -> compile_error "default return value not implemented for type" decl

  (** Emit the code for a function. *)
  method compile_function (decl:fundecl) =
    let index = Option.get decl.index in
    let func = Alice.Ain.get_function_by_index ain index in
    start_address <- current_address;
    func.address <- current_address + 6;
    current_function <- Some func;
    self#write_instruction1 FUNC index;
    self#compile_block decl.body;
    self#compile_default_return func.return_type (ASTDeclaration(Function decl));
    self#write_instruction0 RETURN;
    self#write_instruction1 ENDFUNC index;
    Alice.Ain.Function.write ain func;
    current_function <- None

  (** Compile a list of declarations. *)
  method compile (decls:declaration list) =
    start_address <- Alice.Ain.code_size ain;
    current_address <- start_address;
    let compile_decl = function
      | Function f ->
          self#compile_function f
      | Global g ->
          begin match g.type_spec.qualifier with
          | Some Const -> ()
          | _ ->
              begin match g.initval with
              | Some _ -> compile_error "Global initvals not implemented" (ASTDeclaration(Global g))
              | None -> ()
              end
          end
      | FuncTypeDef _ -> ()
      | StructDef d ->
          let compile_struct_decl (d : struct_declaration) =
            match d with
            | MemberDecl _ -> () (* TODO: member initvals? *)
            | Constructor f -> self#compile_function f
            | Destructor f -> self#compile_function f
            | Method f -> self#compile_function f
          in
          List.iter compile_struct_decl d.decls
      | Enum e ->
          (* TODO: built-in enum methods *)
          compile_error "Enums not implemented" (ASTDeclaration(Enum e))
    in
    List.iter compile_decl decls;
    self#write_buffer
end

let compile ctx decls =
  (new jaf_compiler ctx.ain)#compile decls
