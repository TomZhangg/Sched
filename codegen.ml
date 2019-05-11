(* Code generation: translate takes a semantically
 * checked AST and produces LLVM IR *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)
let tstp str = Printf.eprintf "%s\n" str;()
type sym_tab = {
	mutable scope: L.llvalue StringMap.t;
	parent: sym_tab option;
}

(*type state = (L.llvalue StringMap.t) * L.llvalue * L.llbuilder*)
type state = {mutable namespace:sym_tab;
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
	let st = {scope=StringMap.empty;parent=None} in
	st.scope <- StringMap.add "print" printf_func st.scope;
  let the_state:state = {namespace=st;
                         func=main_func;
                         b=main_builder} in

	let rec lookup n (ns: sym_tab) =
		try
			StringMap.find n ns.scope
		with Not_found ->
			match ns.parent with
				Some(parent) -> lookup n parent
			| _ -> raise Not_found

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
			the_state.namespace.scope <- StringMap.add s (L.build_alloca t' s builder) namespace.scope; L.undef t'
			(* StringMap.add s (L.define_global s (init t) the_module) namespace; L.undef t' *)
		| (t, SAssign (s, e)) ->
			let e' = sxpr the_state e in
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
			| (t, SCall(name, args)) ->
				let the_function = lookup name namespace in
				let llargs = Array.of_list (List.rev (List.map (sxpr the_state) (List.rev args))) in
				let result = (match t with
				                        A.Void -> ""
				                      | _ -> name ^ "_result") in
				         L.build_call the_function llargs result the_state.b
      | _ -> raise (Failure "sxpr codegen type not implemented yet.")
  in

  let add_terminal the_state instr =
		let builder = the_state.b in
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) in

	let change_builder_state old_state b =
      {namespace=old_state.namespace;func=old_state.func;b=b}
  in

  let rec sstmt the_state = function
      SBlock(sl) ->
			let new_scope = StringMap.empty in
			let new_st = {scope=new_scope; parent=Some(the_state.namespace)} in
			let new_state = {namespace=new_st; func=the_state.func; b = the_state.b} in
			List.fold_left sstmt new_state sl; the_state
    | SExpr(sx) -> ignore (sxpr the_state sx); the_state
    | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = sxpr the_state predicate in
      let then_bb = L.append_block context "then" main_func in
			let new_state1 = change_builder_state the_state (L.builder_at_end context then_bb) in
      ignore (sstmt new_state1 then_stmt);
      let else_bb = L.append_block context "else" main_func in
			let new_state2 = change_builder_state the_state (L.builder_at_end context else_bb) in
      ignore (sstmt new_state2 else_stmt);

      let end_bb = L.append_block context "if_end" main_func in
      let build_br_end = L.build_br end_bb in (* partial function *)
      add_terminal new_state1 build_br_end;
      add_terminal new_state2 build_br_end;

      ignore(L.build_cond_br bool_val then_bb else_bb the_state.b);
      let new_state = change_builder_state the_state (L.builder_at_end context end_bb) in new_state
		| SFunc(fdecl) ->
			let ns = the_state.namespace in
			let scope = ns.scope in
			let name = fdecl.sfname in
			let caster b =
				match b with A.Bind(t,name) -> (t,name)
			in

			let casted = (List.map caster fdecl.sformals) in
			let formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) casted) in
			let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
			the_state.namespace.scope <- StringMap.add name (L.define_function name ftype the_module) scope;
			let func_sstmt state = function
				| SRt e ->
					ignore(match fdecl.styp with
									(* Special "return nothing" instr *)
									A.Void -> L.build_ret_void state.b
									(* Build return statement *)
								| _ -> L.build_ret (sxpr state e) state.b );
					the_state
				| s -> sstmt state s
			in

			let new_scope = StringMap.empty in
			let new_st = {scope=new_scope; parent=Some(the_state.namespace)} in
    	let the_function = lookup fdecl.sfname new_st in
    	let buidler = L.builder_at_end context (L.entry_block the_function) in
			let new_state = {namespace=new_st; func=the_state.func; b = buidler} in
			let end_state = List.fold_left func_sstmt new_state fdecl.sbody in
 			the_state
    | _ -> raise (Failure "sstmt codegen type not implemented yet.")
  in

  (*List.iter (fun stmt -> ignore(sstmt the_state.b stmt)) sprogram;
  ignore(L.build_ret (L.const_int i32_t 0) the_state.b); *)
  let final_state = sstmt the_state (SBlock(sprogram)) in
  ignore(L.build_ret (L.const_int i32_t 0) final_state.b);
  the_module
