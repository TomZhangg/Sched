(* Code generation: translate takes a semantically
 * checked AST and produces LLVM IR *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)
let tstp str = Printf.eprintf "%s\n" str;()

(*type state = (L.llvalue StringMap.t) * L.llvalue * L.llbuilder*)
type state = {namespace: L.llvalue StringMap.t;
              func: L.llvalue;
              b: L.llbuilder}

(* translate : Sast.sprogram -> Llvm.module *)
let translate sprogram =
  let context = L.global_context () in
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module  = L.create_module context "Schedch"
  and i1_t        = L.i1_type       context
  and i8_t        = L.i8_type       context
  and i32_t       = L.i32_type      context
  and float_t     = L.double_type   context
  and void_t      = L.void_type     context in

  (* Return the LLVM type for a Schedch type *)
  (* TODO: Add types here.
  let ltype_of_typ = function
          ...
  in
  *)
  let ltype_of_typ = function
    A.Int   -> i32_t
  | A.Bool  -> i1_t
  | A.Float -> float_t
  | A.Void  -> void_t
	in

	let init t = match t with
		A.Float -> L.const_float (ltype_of_typ t) 0.0
	| _ -> L.const_int (ltype_of_typ t) 0
	in

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

	let lookup n ns= StringMap.find n ns
	in
  (* Construct code for an expression; return its value. *)
  let rec sxpr the_state e =
			let builder = the_state.b in
			let namespace = the_state.namespace in
			match e with
      (A.String, SStrLit s) -> L.build_global_stringptr s "" builder
      (* A String literal should result in a defined constant being
       * added to the LLVM module and a pointer to that constant. *)
    | (A.Bool, SBoolLit b)  -> L.const_int i1_t (if b then 1 else 0)
    | (A.Int, SIntLit i) -> L.const_int i32_t i
    | (A.Float, SFLit l) -> L.const_float_of_string float_t l
		| (Void, SBIND (t,s)) ->
			let t' = ltype_of_typ t in
			namespace = StringMap.add s (L.build_alloca t' s builder) namespace; L.undef t'
			(* StringMap.add s (L.define_global s (init t) the_module) namespace; L.undef t' *)
		| (t, SAssign (s, e)) -> tstp "enter assign";
			let e' = sxpr the_state e in tstp "pass checkpoint";
			ignore(L.build_store e' (lookup s namespace) builder); e'
    | (A.Bool, SBinop (e1, op, e2)) ->
         	  let e1' = sxpr the_state e1
         	  and e2' = sxpr the_state e2 in
         	  (match op with
         	  | A.And     -> L.build_and
         	  | A.Or      -> L.build_or
         	  | A.Equal   -> ( match e1 with
                  	  (A.Int, _) -> L.build_icmp L.Icmp.Eq
                        | (A.Float, _) -> L.build_fcmp L.Fcmp.Oeq )
         	  | A.Neq     -> ( match e1 with
                  	  (A.Int, _) -> L.build_icmp L.Icmp.Ne
                        | (A.Float, _) -> L.build_fcmp L.Fcmp.One )
		  | A.Less    -> ( match e1 with
                  	  (A.Int, _) -> L.build_icmp L.Icmp.Slt
                        | (A.Float, _) -> L.build_fcmp L.Fcmp.Olt )
         	  | A.Leq     -> ( match e1 with
                  	  (A.Int, _) -> L.build_icmp L.Icmp.Sle
                        | (A.Float, _) -> L.build_fcmp L.Fcmp.Ole )
         	  | A.Greater -> ( match e1 with
                  	  (A.Int, _) -> L.build_icmp L.Icmp.Sgt
                        | (A.Float, _) -> L.build_fcmp L.Fcmp.Ogt )
         	  | A.Geq     -> ( match e1 with
                  	  (A.Int, _) -> L.build_icmp L.Icmp.Sge
                        | (A.Float, _) -> L.build_fcmp L.Fcmp.Oge )
       	    ) e1' e2' "tmp" builder
    | (A.Int, SBinop (e1, op, e2)) ->
     	  let e1' = sxpr the_state e1
     	  and e2' = sxpr the_state e2 in
     	  (match op with
     	    A.Add     -> L.build_add
     	  | A.Sub     -> L.build_sub
     	  | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
     	  (*| A.Equal   -> L.build_icmp L.Icmp.Eq
     	  | A.Neq     -> L.build_icmp L.Icmp.Ne
     	  | A.Less    -> L.build_icmp L.Icmp.Slt
     	  | A.Leq     -> L.build_icmp L.Icmp.Sle
     	  | A.Greater -> L.build_icmp L.Icmp.Sgt
     	  | A.Geq     -> L.build_icmp L.Icmp.Sge *)
        ) e1' e2' "tmp" builder
    | (A.Float, SBinop (e1, op, e2)) ->
     	  let e1' = sxpr the_state e1
     	  and e2' = sxpr the_state e2 in
     	  (match op with
     	    A.Add     -> L.build_fadd
     	  | A.Sub     -> L.build_fsub
     	  | A.Mult    -> L.build_fmul
          | A.Div     -> L.build_fdiv
     	  (*| A.Equal   -> L.build_fcmp L.Fcmp.Oeq
     	  | A.Neq     -> L.build_fcmp L.Fcmp.One
     	  | A.Less    -> L.build_fcmp L.Fcmp.Olt
     	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
     	  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
     	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge *)
        ) e1' e2' "tmp" builder
    | (A.Float, SUnop (op, e)) ->
          let e' = sxpr the_state e in
	  (match op with
	  A.Neg                  -> L.build_fneg) e' "tmp" builder
    | (A.Int, SUnop (op, e)) ->
          let e' = sxpr the_state e in
	  (match op with
	  A.Neg                  -> L.build_neg) e' "tmp" builder
    | (A.Bool, SUnop (op, e)) ->
          let e' = sxpr the_state e in
	  (match op with
          A.Not                  -> L.build_not) e' "tmp" builder
      | (A.Void, SCall("print", [sx])) ->
          let sx' = sxpr the_state sx in
          L.build_call printf_func [| str_format_str ; sx' |] "printf" builder
      | _ -> raise (Failure "sxpr codegen type not implemented yet.")
  in

  let rec sstmt the_state = function
      SExpr(sx) -> ignore (sxpr the_state sx); the_state
    | _ -> raise (Failure "sstmt codegen type not implemented yet.")
  in

  List.iter (fun stmt -> ignore(sstmt the_state stmt)) sprogram;
  ignore(L.build_ret (L.const_int i32_t 0) the_state.b);
  the_module
