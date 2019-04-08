open Ast

type action = Ast | Sast | LLVM_IR | Compile
let string_of_action act =
  match act with
    Ast -> "Ast"
  | Sast -> "Sast"
  | LLVM_IR -> "LLVM_IR"
  | Compile -> "Compile"

exception Option_not_implemented of string

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./schedch.native [-a|-s|-l|-c] [file.schedch]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
    match !action with
      Ast -> print_string (Ast.pp_program ast)
    | Sast ->
      let check_opt = Semant.check ast Semant.init_st in
      match check_opt with
        Some (stmts, st) -> print_endline (Sast.string_of_sprogram stmts)
      | None -> raise (Failure "Semantic check failed.")
    | _ -> raise (Option_not_implemented (string_of_action !action))
