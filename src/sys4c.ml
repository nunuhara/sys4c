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
open Jaf

let parse_jaf jaf_file =
  let do_parse file =
    let lexbuf = Lexing.from_channel file in
    Parser.jaf Lexer.token lexbuf
  in
  match jaf_file with
  | "-" ->
      do_parse In_channel.stdin
  | path ->
      In_channel.with_file path ~f:(fun file -> do_parse file)

let compile_jaf ctx jaf_file decl_only =
  let jaf = parse_jaf jaf_file in
  Declarations.register_type_declarations ctx jaf;
  Declarations.resolve_types ctx jaf decl_only;
  Declarations.define_types ctx jaf;
  if not decl_only then begin
    TypeAnalysis.check_types ctx jaf;
    ConstEval.evaluate_constant_expressions ctx jaf;
    VariableAlloc.allocate_variables ctx jaf;
    SanityCheck.check_invariants ctx jaf; (* TODO: disable in release builds *)
    Compiler.compile ctx jaf
  end

let compile_hll ctx hll_file =
  let do_parse file =
    let lexbuf = Lexing.from_channel file in
    Parser.hll Lexer.token lexbuf
  in
  let get_lib_name filename =
    Filename.chop_extension (Filename.basename filename)
  in
  let hll = In_channel.with_file hll_file ~f:(fun file -> do_parse file) in
  Declarations.define_library ctx hll (get_lib_name hll_file)

let compile_sources sources imports major minor decl_only =
  (* open/create the output .ain file *)
  (* XXX: if the first file is a .ain file, open it instead of linking against a blank file *)
  let (ain, sources) =
    match sources with
    | [] ->
        (Alice.Ain.create major minor, ["-"])
    | file::rest when (Filename.check_suffix file ".ain") ->
        (Alice.Ain.load file, rest)
    | _ ->
        (Alice.Ain.create major minor, sources)
  in
  (* open/link the import .ain files *)
  let import_ain =
    let load_import_ain base file =
      let p = Alice.Ain.load file in
      Link.link base p true;
      Alice.Ain.free p
    in
    match imports with
    | [] -> Alice.Ain.create (Alice.Ain.version ain) (Alice.Ain.minor_version ain)
    | file::rest ->
        let base = Alice.Ain.load file in
        List.iter rest ~f:(load_import_ain base);
        base
  in
  (* check versions *)
  if not (phys_equal (Alice.Ain.version ain) (Alice.Ain.version import_ain)) then
    failwith "Import .ain file version doesn't match output version";
  (* compile sources *)
  let ctx = { ain; import_ain; const_vars=[] } in
  let compile_file f =
    if (Filename.check_suffix f ".jaf") || (String.equal f "-") then
      compile_jaf ctx f decl_only
    else if Filename.check_suffix f ".hll" then
      compile_hll ctx f
    else if Filename.check_suffix f ".ain" then
      Link.link ctx.ain (Alice.Ain.load f) decl_only
    else
      failwith "unsupported file type"
  in
  List.iter sources ~f:compile_file;
  Alice.Ain.free ctx.import_ain;
  ctx.ain

let do_compile sources imports output major minor decl_only compile_unit match_decls =
  try
    (* create output .ain file by compiling/linking inputs *)
    let ain = compile_sources sources imports major minor decl_only in
    (* -m option: check if declarations match then return status code *)
    begin match match_decls with
    | [] -> ()
    | _ ->
        let decl_ain = compile_sources match_decls imports major minor true in
        let matched = Link.declarations_match decl_ain ain in
        Alice.Ain.free ain;
        Alice.Ain.free decl_ain;
        exit (if matched then 0 else 1)
    end;
    (* -c/-d option: skip final check for undefined functions *)
    if (not compile_unit) && (not decl_only) then
      Link.check_undefined ain;
    (* write output .ain file to disk *)
    Alice.Ain.write ain output;
    Alice.Ain.free ain
  with
  | CompileError.Type_error (expected, actual, parent) ->
      let s_expected = Alice.Ain.Type.to_string expected in
      let s_actual =
        match actual with
        | None -> "void"
        | Some expr ->
            match expr.valuetype with
            | None -> "untyped"
            | Some t -> Alice.Ain.Type.to_string t
      in
      printf "Error: Type error: expected %s; got %s\n" s_expected s_actual;
      Option.iter actual ~f:(fun e -> printf "\tat: %s\n" (expr_to_string e));
      printf "\tin: %s\n" (ast_to_string parent);
      exit 1
  | CompileError.Undefined_variable (name, _) ->
      printf "Error: Undefined variable: %s\n" name;
      exit 1
  | CompileError.Arity_error (f, args, parent) ->
      printf "Error: wrong number of arguments to function %s (expected %d; got %d)\n" f.name f.nr_args (List.length args);
      printf "\tin: %s\n" (ast_to_string parent);
      exit 1
  | CompileError.Not_lvalue_error (expr, parent) ->
      printf "Error: not an lvalue: %s\n" (expr_to_string expr);
      printf "\tin: %s\n" (ast_to_string parent);
      exit 1
  | CompileError.Const_error (var) ->
      begin match var.initval with
      | Some _ -> printf "Error: value of const variable is not constant\n"
      | None   -> printf "Error: const variable lacks initializer\n"
      end;
      printf "\tin: %s\n" (var_to_string var);
      exit 1
  | CompileError.CompileError (msg, node) ->
      printf "Error: %s\n" msg;
      printf "\tin: %s\n" (ast_to_string node);
      exit 1
  | CompileError.LinkError (msg) ->
      printf "Error: %s\n" msg;
      exit 1
  | CompileError.CompilerBug (msg, node) ->
      printf "Error: %s\n" msg;
      begin match node with
      | Some n -> printf "\tin: %s\n" (ast_to_string n)
      | None -> ()
      end;
      printf "(This is a compiler bug!)";
      exit 1
  | CompileError.LinkerBug (msg) ->
      printf "Error: %s\n" msg;
      printf "(This is a linker bug!)";
      exit 1

let cmd_compile_jaf =
  Command.basic
    ~summary:"Compile a .jaf file"
    ~readme: (fun () -> "Compile a .jaf file, optionally appending to an existing .ain file.")
    Command.Let_syntax.(
      let%map_open
        sources = anon (sequence ("source files" %: Filename.arg_type))
      and output = flag "-output" (optional_with_default "out.ain" Filename.arg_type)
        ~doc:"out-file The output .ain file"
      and major = flag "-ain-version" (optional_with_default 4 int)
        ~doc:"version The output .ain file version (default: 4)"
      and minor = flag "-ain-minor-version" (optional_with_default 0 int)
        ~doc:"version The output .ain file minor version (default: 0)"
      and decl_only = flag "-declarations-only" no_arg
        ~doc:" Output declarations only"
      and imports = flag "-import-declarations" (listed Filename.arg_type)
        ~doc:"ain-file Import declarations from the given .ain file"
      and compile_unit = flag "-compile-unit" no_arg
        ~doc:" Compile as a unit (allow undefined functions)"
      and match_decls = flag "-match-declarations" (listed Filename.arg_type)
        ~doc:"ain-file Compare declarations against the given .ain file"
      in
      fun () ->
        do_compile sources imports output major minor decl_only compile_unit match_decls)

let () =
  Command.run ~version:"0.1" cmd_compile_jaf;
