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

let rec type_equal (expected:Alice.Ain.Type.data) (actual:Alice.Ain.Type.data) =
  match (expected, actual) with
  | (Void, _) -> true (* XXX: used for type-generic built-ins (e.g. Array.PushBack) *)
  | (Int, (Int|Bool|LongInt)) -> true
  | (Bool, (Int|Bool|LongInt)) -> true
  | (LongInt, (Int|Bool|LongInt)) -> true
  | (Float, Float) -> true
  | (String, String) -> true
  | (Struct a, Struct b) -> a = b
  | (IMainSystem, IMainSystem) -> true
  | (FuncType a, FuncType b) -> a = b
  | (Delegate a, Delegate b) -> a = b
  | (HLLParam, HLLParam) -> true
  | (Array a, Array b) -> type_equal a.data b.data
  | (Wrap a, Wrap b) -> type_equal a.data b.data
  | (Option a, Option b) -> type_equal a.data b.data
  | (Unknown87 a, Unknown87 b) -> type_equal a.data b.data
  | (IFace, IFace) -> true
  | (Enum2 a, Enum2 b) -> a = b
  | (Enum a, Enum b) -> a = b
  | (HLLFunc, HLLFunc) -> true
  | (IFaceWrap, IFaceWrap) -> true
  | (Function _, Function _) -> true
  | (Int, _)
  | (Bool, _)
  | (LongInt, _)
  | (Float, _)
  | (String, _)
  | (Struct _, _)
  | (IMainSystem, _)
  | (FuncType _, _)
  | (Delegate _, _)
  | (HLLParam, _)
  | (Array _, _)
  | (Wrap _, _)
  | (Option _, _)
  | (Unknown87 _, _)
  | (IFace, _)
  | (Enum2 _, _)
  | (Enum _, _)
  | (HLLFunc, _)
  | (IFaceWrap, _)
  | (Function _, _) -> false

let type_castable (dst:data_type) (src:Alice.Ain.Type.data) =
  match (dst, src) with
  (* FIXME: cast to void should be allowed *)
  | (Void, _) -> compiler_bug "type checker cast to void type" None
  | ((Int|Bool|Float), (Int|Bool|Float)) -> true
  | _ -> false

let type_check parent expected actual =
  match actual.valuetype with
  | None ->
      compiler_bug "tried to type check untyped expression" (Some parent)
  | Some a_t ->
      if not (type_equal expected a_t.data) then
        data_type_error expected (Some actual) parent

let type_check_numeric parent actual =
  match actual.valuetype with
  | Some {data=(Int|Bool|Float); _} -> ()
  | Some _ -> data_type_error Int (Some actual) parent
  | None -> compiler_bug "tried to type check untyped expression" (Some parent)

let type_check_struct parent actual =
  match actual.valuetype with
  | Some {data=Struct i; _} -> i
  | Some _ -> data_type_error (Struct 0) (Some actual) parent
  | None -> compiler_bug "tried to type check untyped expression" (Some parent)

class type_analyze_visitor ctx = object (self)
  inherit ivisitor as super

  (* an lvalue is an expression which denotes a location that can be assigned to/referenced *)
  method check_lvalue (e:expression) (parent:ast_node) =
    let check_lvalue_type = function
      | Alice.Ain.Type.Function (_) -> not_an_lvalue_error e parent
      | _ -> ()
    in
    match e.node with
    | Ident (_, _) -> check_lvalue_type (Option.value_exn e.valuetype).data
    | Member (_, _, _) -> check_lvalue_type (Option.value_exn e.valuetype).data
    | Subscript (_, _) -> ()
    | New (_, _, _) -> ()
    | _ -> not_an_lvalue_error e parent

  method check_assign parent t rhs =
    match t with
    (*
     * Assigning to a functype variable is special.
     * The RHS should be an expression like &foo, which has type
     * 'ref function'. This is then converted into the declared
     * functype of the variable (if the prototypes match).
     *)
    | Alice.Ain.Type.FuncType (ft_i) ->
        begin match (Option.value_exn rhs.valuetype) with
        | { data=Alice.Ain.Type.Function (f_i); is_ref=true } ->
            let ft = Alice.Ain.FunctionType.of_int ctx.ain ft_i in
            let f = Alice.Ain.Function.of_int ctx.ain f_i in
            if not (Alice.Ain.FunctionType.function_compatible ft f) then
              data_type_error (Alice.Ain.Type.FuncType (ft_i)) (Some rhs) parent
        | _ ->
            ref_type_error (Alice.Ain.Type.Function (-1)) (Some rhs) parent
        end
    | _ ->
        type_check parent t rhs

  method! visit_expression expr =
    super#visit_expression expr;
    (* convenience functions which always pass parent expression *)
    let check = type_check (ASTExpression expr) in
    let check_numeric = type_check_numeric (ASTExpression expr) in
    let check_struct = type_check_struct (ASTExpression expr) in
    let check_expr a b = check (Option.value_exn a.valuetype).data b in
    (* check function call arguments *)
    let check_call (f:Alice.Ain.Function.t) args =
      let params = Alice.Ain.Function.logical_parameters f in
      let nr_params = List.length params in
      if not (nr_params = (List.length args)) then
        arity_error f args (ASTExpression expr)
      else if nr_params > 0 then begin
        let check_arg a (v:Alice.Ain.Variable.t) = check v.value_type.data a in
        List.iter2_exn args params ~f:check_arg
      end
    in
    let set_valuetype spec =
      expr.valuetype <- Some (jaf_to_ain_type spec)
    in
    match expr.node with
    | ConstInt (_) ->
        expr.valuetype <- Some (Alice.Ain.Type.make Int)
    | ConstFloat (_) ->
        expr.valuetype <- Some (Alice.Ain.Type.make Float)
    | ConstChar (_) ->
        expr.valuetype <- Some (Alice.Ain.Type.make Int)
    | ConstString (_) ->
        expr.valuetype <- Some (Alice.Ain.Type.make String)
    | Ident ("system", _) ->
        (* NOTE: on ain v11+, "system" is a library *)
        if Alice.Ain.version_gte ctx.ain 11 0 then
          begin match Alice.Ain.get_library_index ctx.ain "system" with
          | Some i ->
              expr.node <- Ident ("system", Some (HLLName i));
              expr.valuetype <- Some (Alice.Ain.Type.make Void)
          | None ->
              undefined_variable_error "system" (ASTExpression expr)
          end
        else
          begin
            expr.node <- Ident ("system", Some System);
            expr.valuetype <- Some (Alice.Ain.Type.make Void)
          end
    | Ident (name, _) ->
        begin match environment#get name with
        | Some v ->
            expr.node <- Ident (name, Some (LocalVariable (-1)));
            set_valuetype { data=v.type_spec.data; qualifier=None }
        | None ->
            begin match List.findi ctx.const_vars ~f:(fun _ (v:variable) -> String.equal v.name name) with
            | Some (_, v) ->
                expr.node <- Ident (name, Some GlobalConstant);
                set_valuetype { data=v.type_spec.data; qualifier=None }
            | None ->
                begin match Alice.Ain.get_global ctx.ain name with
                | Some g ->
                    expr.node <- Ident (name, Some (GlobalVariable (g.index)));
                    expr.valuetype <- Some g.value_type
                | None ->
                    begin match Alice.Ain.get_function ctx.ain name with
                    | Some f ->
                        expr.node <- Ident (name, Some (FunctionName (f.index)));
                        expr.valuetype <- Some (Alice.Ain.Type.make (Function f.index))
                    | None ->
                        begin match Alice.Ain.get_library_index ctx.ain name with
                        | Some i ->
                            expr.node <- Ident (name, Some (HLLName i));
                            expr.valuetype <- Some (Alice.Ain.Type.make Void)
                        | None ->
                            undefined_variable_error name (ASTExpression expr)
                        end
                    end
                end
            end
        end
    | Unary (op, e) ->
        begin match op with
        | UPlus | UMinus | PreInc | PreDec | PostInc | PostDec ->
            check_numeric e;
            expr.valuetype <- Some (Option.value_exn e.valuetype)
        | LogNot | BitNot ->
            check Int e;
            expr.valuetype <- Some (Option.value_exn e.valuetype)
        | AddrOf ->
            begin match (Option.value_exn e.valuetype).data with
            | Function i ->
                expr.valuetype <- Some (Alice.Ain.Type.make ~is_ref:true (Function i))
            | _ ->
                data_type_error (Function (-1)) (Some e) (ASTExpression expr)
            end
        end
    | Binary (op, a, b) ->
        begin match op with
        | Plus | Minus | Times | Divide | LT | GT | LTE | GTE ->
            check_numeric a;
            check_numeric b;
            (* TODO: allow coercion *)
            check_expr a b;
            expr.valuetype <- a.valuetype
        | LogOr | LogAnd | BitOr | BitXor | BitAnd | LShift | RShift ->
            check Int a;
            check Int b;
            expr.valuetype <- a.valuetype
        | Modulo ->
            begin match (Option.value_exn a.valuetype).data with
            | String ->
                (* TODO: check type matches format specifier if format string is a literal *)
                begin match (Option.value_exn b.valuetype).data with
                | Int | Float | Bool | LongInt | String -> ()
                | _ -> data_type_error Int (Some b) (ASTExpression expr)
                end
            | Int | Bool | LongInt ->
                check Int b
            | _ ->
                data_type_error Int (Some a) (ASTExpression expr)
            end;
            expr.valuetype <- a.valuetype
        | Equal | NEqual ->
            begin match (Option.value_exn a.valuetype).data with
            | String ->
                check String b
            | _ ->
                check_numeric a;
                check_numeric b;
                (* TODO: allow coercion *)
                check_expr a b
            end;
            expr.valuetype <- Some (Alice.Ain.Type.make Int)
        end;
    | Assign (op, lhs, rhs) ->
        self#check_lvalue lhs (ASTExpression expr);
        begin match op with
        | EqAssign ->
            self#check_assign (ASTExpression expr) (Option.value_exn lhs.valuetype).data rhs
        | PlusAssign | MinusAssign | TimesAssign | DivideAssign ->
            check_numeric lhs;
            check_numeric rhs;
            (* TODO: allow coercion *)
            check_expr lhs rhs
        | ModuloAssign | OrAssign | XorAssign | AndAssign
        | LShiftAssign | RShiftAssign ->
            check Int lhs;
            check Int rhs
        end;
        expr.valuetype <- lhs.valuetype
    | Seq (_, e) ->
        expr.valuetype <- e.valuetype
    | Ternary (test, con, alt) ->
        check Int test;
        check_expr con alt;
        expr.valuetype <- con.valuetype
    | Cast (t, e) ->
        if not (type_castable t (Option.value_exn e.valuetype).data) then
          data_type_error (jaf_to_ain_data_type t) (Some e) (ASTExpression expr);
        set_valuetype { data=t; qualifier=None }
    | Subscript (obj, i) ->
        check Int i;
        begin match (Option.value_exn obj.valuetype).data with
        | Array t ->
            expr.valuetype <- Some t
        | _ ->
            let array_type = { data=Unresolved ("?"); qualifier=None } in
            let expected = Array (array_type) in
            data_type_error (jaf_to_ain_data_type expected) (Some obj) (ASTExpression expr)
        end
    (* system function *)
    | Member ({node=Ident(_, Some System); _} as e, syscall_name, _) ->
        begin match Bytecode.syscall_of_string syscall_name with
        | Some sys ->
            expr.node <- Member(e, syscall_name, Some (SystemFunction sys));
            expr.valuetype <- Some (Alice.Ain.Type.make (Function 0))
        | None ->
            (* TODO: separate error type for this? *)
            undefined_variable_error ("system." ^ syscall_name) (ASTExpression expr)
        end
    (* HLL function *)
    | Member ({node=Ident(lib_name, Some (HLLName lib_no)); _} as e, fun_name, _) ->
        begin match Alice.Ain.get_library_function_index ctx.ain lib_no fun_name with
        | Some fun_no ->
            expr.node <- Member(e, fun_name, Some (HLLFunction (lib_no, fun_no)));
            expr.valuetype <- Some (Alice.Ain.Type.make (Function 0))
        | None ->
            (* TODO: separate error type for this? *)
            undefined_variable_error (lib_name ^ "." ^ fun_name) (ASTExpression(expr))
        end
    (* built-in methods *)
    | Member ({valuetype=Some{data=(Int|Float|String) as t; _}; _} as e, name, _) ->
        begin match Bytecode.builtin_of_string t name with
        | Some builtin ->
            expr.node <- Member (e, name, Some (BuiltinMethod builtin));
            expr.valuetype <- Some (Alice.Ain.Type.make (Function 0))
        | None ->
            (* TODO: separate error type for this? *)
            undefined_variable_error ((Alice.Ain.Type.data_to_string t) ^ name) (ASTExpression(expr))
        end
    (* member variable OR method *)
    | Member (obj, member_name, _) ->
        let struc = Alice.Ain.get_struct_by_index ctx.ain (check_struct obj) in
        let check_member _ (m : Alice.Ain.Variable.t) =
          String.equal m.name member_name
        in
        begin match List.findi struc.members ~f:check_member with
        | Some (_, member) ->
            expr.node <- Member (obj, member_name, Some (ClassVariable (struc.index, member.index)));
            expr.valuetype <- Some member.value_type
        | None ->
            let fun_name = struc.name ^ "@" ^ member_name in
            begin match Alice.Ain.get_function ctx.ain fun_name with
            | Some f ->
                expr.node <- Member (obj, member_name, Some (ClassMethod (struc.index, f.index)));
                expr.valuetype <- Some (Alice.Ain.Type.make (Function f.index))
            | None ->
                (* TODO: separate error type for this? *)
                undefined_variable_error (struc.name ^ "." ^ member_name) (ASTExpression expr)
            end
        end
    (* regular function call *)
    | Call ({node=Ident(_, Some FunctionName fno); _} as e, args, _) ->
        let f = Alice.Ain.Function.of_int ctx.ain fno in
        check_call f args;
        expr.node <- Call (e, args, Some (FunctionCall fno));
        expr.valuetype <- Some f.return_type
    (* method call *)
    | Call ({node=Member(_, _, Some (ClassMethod(sno, mno))); _} as e, args, _) ->
        let f = Alice.Ain.Function.of_int ctx.ain mno in
        check_call f args;
        expr.node <- Call (e, args, Some (MethodCall(sno, mno)));
        expr.valuetype <- Some f.return_type
    (* HLL call *)
    | Call ({node=Member(_, _, Some (HLLFunction(lib_no, fun_no))); _} as e, args, _) ->
        let f = Alice.Ain.function_of_hll_function_index ctx.ain lib_no fun_no in
        check_call f args;
        expr.node <- Call (e, args, Some (HLLCall(lib_no, fun_no, -1)));
        expr.valuetype <- Some f.return_type
    (* system call *)
    | Call ({node=Member(_, _, Some(SystemFunction sys)); _} as e, args, _) ->
        let f = Bytecode.function_of_syscall sys in
        check_call f args;
        expr.node <- Call (e, args, Some (SystemCall sys));
        expr.valuetype <- Some f.return_type
    (* built-in call *)
    | Call ({node=Member(_, _, Some(BuiltinMethod builtin)); _} as e, args, _) ->
        (* TODO: rewrite to HLL call for 11+ (?) *)
        if Alice.Ain.version_gte ctx.ain 11 0 then
          compile_error "ain v11+ built-ins not implemented" (ASTExpression expr);
        let f = Bytecode.function_of_builtin builtin in
        check_call f args;
        expr.node <- Call (e, args, Some (BuiltinCall builtin));
        expr.valuetype <- Some f.return_type
    (* functype call *)
    | Call (e, args, _) ->
        begin match (Option.value_exn e.valuetype).data with
        | FuncType no ->
            let f = Alice.Ain.function_of_functype_index ctx.ain no in
            check_call f args;
            expr.node <- Call (e, args, Some (FuncTypeCall no));
            expr.valuetype <- Some f.return_type
        | _ ->
            data_type_error (Alice.Ain.Type.FuncType (-1)) (Some e) (ASTExpression expr)
        end
    | New (t, args, _) ->
        begin match t with
        | Struct (_, i) ->
            (* TODO: look up the correct constructor for given arguments *)
            begin match (Alice.Ain.Struct.of_int ctx.ain i).constructor with
            | -1 ->
                if not ((List.length args) = 0) then
                  (* TODO: signal error properly here *)
                  compile_error "Arguments provided to default constructor" (ASTExpression expr);
            | no ->
                let ctor = Alice.Ain.Function.of_int ctx.ain no in
                check_call ctor args;
            end;
            set_valuetype { data=t; qualifier=None }
        | _ -> data_type_error (Struct (-1)) None (ASTExpression expr)
        end
    | This ->
        match environment#current_class with
        | Some i ->
            expr.valuetype <- Some (Alice.Ain.Type.make (Struct i))
        | None ->
            (* TODO: separate error type for this? *)
            undefined_variable_error "this" (ASTExpression expr)

  method! visit_statement stmt =
    (* rewrite character constants at statement-level as messages *)
    begin match stmt.node with
    | Expression ({node=ConstChar(msg); _}) ->
        stmt.node <- MessageCall (msg, None, None)
    | _ -> ()
    end;
    super#visit_statement stmt;
    begin match stmt.node with
    | EmptyStatement -> ()
    | Expression (_) -> ()
    | Compound (_) -> ()
    | Labeled (_, _) -> ()
    | If (test, _, _) | While (test, _) | DoWhile (test, _) ->
        type_check (ASTStatement (stmt)) Int test
    | For (_, test, _, _) ->
        type_check (ASTStatement (stmt)) Int test
    | Goto (_) -> ()
    | Continue -> ()
    | Break -> ()
    | Return (Some e) ->
        begin match environment#current_function with
        | None -> compiler_bug "return statement outside of function" (Some(ASTStatement stmt))
        | Some f -> type_check (ASTStatement stmt) (jaf_to_ain_data_type f.return.data) e
        end
    | Return (None) ->
        begin match environment#current_function with
        | None -> compiler_bug "return statement outside of function" (Some(ASTStatement stmt))
        | Some f ->
            begin match f.return.data with
            | Void -> ()
            | _ -> data_type_error (jaf_to_ain_data_type f.return.data) None (ASTStatement stmt)
            end
        end
    | MessageCall (msg, f_name, _) ->
        begin match f_name with
        | Some name ->
          begin match Alice.Ain.get_function ctx.ain name with
          | Some f ->
              if f.nr_args > 0 then
                arity_error f [] (ASTStatement stmt);
              stmt.node <- MessageCall (msg, f_name, Some f.index);
          | None ->
              undefined_variable_error name (ASTStatement stmt)
          end
        | None -> ()
        end
    | RefAssign (lhs, rhs) ->
        (* rhs must be an lvalue in order to create a reference to it *)
        self#check_lvalue rhs (ASTStatement stmt);
        (* check that lhs is a reference variable of the appropriate type *)
        begin match lhs.node with
        | Ident (name, _) ->
            begin match environment#get name with
            | Some v ->
                begin match v.type_spec.qualifier with
                | Some Ref ->
                    type_check (ASTStatement stmt) (Option.value_exn lhs.valuetype).data rhs
                | _ ->
                    ref_type_error (Option.value_exn rhs.valuetype).data (Some lhs) (ASTStatement stmt)
                end
            | None ->
                undefined_variable_error name (ASTStatement stmt)
            end
        | _ ->
            (* FIXME? this isn't really a _type_ error *)
            ref_type_error (Option.value_exn rhs.valuetype).data (Some lhs) (ASTStatement stmt)
        end
    end

  method visit_variable var =
    let rec calculate_array_rank (t:type_specifier) =
      match t.data with
      | Array sub_t -> 1 + (calculate_array_rank sub_t)
      | _ -> 0
    in
    let rank = calculate_array_rank var.type_spec in
    let nr_dims = List.length var.array_dim in
    (* Only one array dimension may be specified in ain v11+ *)
    if (nr_dims > 1) && (Alice.Ain.version_gte ctx.ain 11 0) then
      compile_error "Multiple array dimensions specified for ain v11+" (ASTVariable var);
    (* Check that there is no initializer if array has explicit dimensions *)
    if (nr_dims > 0) && (Option.is_some var.initval) then
      compile_error "Initializer provided for array with explicit dimensions" (ASTVariable var);
    (* Check that number of dims matches rank of array *)
    if (nr_dims > 0) && (not (nr_dims = rank)) then
      compile_error "Number of array dimensions does not match array rank" (ASTVariable var);
    (* Check that array dims are integers *)
    List.iter var.array_dim ~f:(fun e -> type_check (ASTVariable var) Int e);
    (* Check initval matches declared type *)
    begin match var.initval with
    | Some expr ->
        self#check_assign (ASTVariable var) (jaf_to_ain_data_type var.type_spec.data) expr
    | None -> ()
    end

  method! visit_local_variable var =
    super#visit_local_variable var;
    self#visit_variable var

  method! visit_declaration decl =
    super#visit_declaration decl;
    begin match decl with
    | Global g -> self#visit_variable g
    | _ -> ()
    end

  method! visit_fundecl f =
    (* Equality function for function declarations. Two function declarations
       are considered equal if they have the same number and type of parameters,
       and the same return type. *)
    let fundecl_equal (a:Alice.Ain.Function.t) (b:Alice.Ain.Function.t) =
      (* extract parameter types from variable list *)
      let take_arg_types n (vars:Alice.Ain.Variable.t list) =
        let rec take_arg_types' n (vars:Alice.Ain.Variable.t list) result =
          if n = 0 then
            List.rev result
          else
            begin match vars with
            | x::xs -> take_arg_types' (n - 1) xs (x.value_type::result)
            | [] -> compiler_bug "take_arg_types: n > nr_args" (Some(ASTDeclaration(Function f)))
            end
        in
        take_arg_types' n vars []
      in
      (* full type comparison (including ref) *)
      let arg_type_equal (a:Alice.Ain.Type.t) (b:Alice.Ain.Type.t) =
        (Bool.equal a.is_ref b.is_ref) && (type_equal a.data b.data)
      in
      let a_args = take_arg_types a.nr_args a.vars in
      let b_args = take_arg_types b.nr_args b.vars in
      (a.nr_args = b.nr_args)
      && (List.for_all2_exn a_args b_args ~f:arg_type_equal)
      && (arg_type_equal a.return_type b.return_type)
    in
    super#visit_fundecl f;
    begin match f.return.qualifier with
    | Some Override ->
        let child = Alice.Ain.Function.create f.name |> jaf_to_ain_function f in
        let parent = Alice.Ain.get_function_by_index ctx.ain (Option.value_exn f.super_index) in
        if not (fundecl_equal child parent) then
          compile_error "Override function has incorrect signature" (ASTDeclaration(Function f))
    | Some Const ->
        compile_error "Function cannot be declared const" (ASTDeclaration(Function f))
    | _ -> ()
    end;
    if String.equal f.name "main" then
      begin match (f.return, f.params) with
      | ({data=Int; qualifier=(None|Some Override)}, []) ->
          Alice.Ain.set_main_function ctx.ain (Option.value_exn f.index)
      | _ ->
          compile_error "Invalid declaration of 'main' function" (ASTDeclaration(Function f))
      end
    else if String.equal f.name "message" then
      begin match f.return with
      | {data=Void; qualifier=(None|Some Override)} ->
          begin match List.map f.params ~f:(fun v -> v.type_spec) with
          | [{data=Int; qualifier=None}; {data=Int; qualifier=None}; {data=String; qualifier=None}] ->
              Alice.Ain.set_message_function ctx.ain (Option.value_exn f.index)
          | _ ->
              compile_error "Invalid declaration of 'message' function" (ASTDeclaration(Function f))
          end
      | _ ->
          compile_error "invalid declaration of 'message' function" (ASTDeclaration(Function f))
      end
end

let check_types ctx decls =
  (new type_analyze_visitor ctx)#visit_toplevel decls
