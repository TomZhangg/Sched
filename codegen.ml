(* Code generation: translate takes a semantically
 * checked AST and produces LLVM IR *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(*type state = (L.llvalue StringMap.t) * L.llvalue * L.llbuilder*)
type state = {namespace: L.llvalue StringMap.t;
              func: L.llvalue;
              b: L.llbuilder}

(* translate : Sast.sprogram -> Llvm.module *)
let translate sprogram =
  let context = L.global_context () in
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Schedch"
  and i8_t = L.i8_type context
  and i32_t = L.i32_type context in

  (* Return the LLVM type for a Schedch type *)
  (* TODO: Add types here.
  let ltype_of_typ = function
          ...
  in
  *)

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (** setup main() where all the code will go **)
  (* ftype is the full llvm function signature *)
  let main_t = L.function_type i32_t [||] in
  let main_func = L.define_function "main" main_t the_module in
  let main_builder = L.builder_at_end context (L.entry_block main_func) in
  let str_format_str = L.build_global_stringptr "%s\n" "fmt" main_builder in

  let the_state:state = {namespace=StringMap.empty;
                         func=main_func;
                         b=main_builder} in


  (* Construct code for an expression; return its value. *)
  let rec sxpr builder = function
      (A.String, SStrLit s) -> L.build_global_stringptr s "" builder
      (* A String literal should result in a defined constant being
     * added to the LLVM module and a pointer to that constant. *)
    | (A.Void, SCall("print", [sx])) ->
        let sx' = sxpr builder sx in
        L.build_call printf_func [| str_format_str ; sx' |] "printf" builder
    | _ -> raise (Failure "sxpr codegen type not implemented yet.")
  in

  let rec sstmt builder = function
      SExpr(sx) -> ignore (sxpr builder sx); builder
    | _ -> raise (Failure "sstmt codegen type not implemented yet.")
  in

  List.iter (fun stmt -> ignore(sstmt the_state.b stmt)) sprogram;
  ignore(L.build_ret (L.const_int i32_t 0) the_state.b);
  the_module