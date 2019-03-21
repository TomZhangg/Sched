open Ast

let () =
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
  let ast = Parser.program Scanner.token lexbuf in
    print_string (Ast.string_of_program ast)
