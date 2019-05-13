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
  let the_module  = L.create_module     context "Schedch"
  and i1_t        = L.i1_type           context
  and i8_t        = L.i8_type           context
  and i32_t       = L.i32_type          context
  and float_t     = L.double_type       context
  and str_ptr_t = L.pointer_type (L.i8_type context)
  and void_t      = L.void_type         context in

  let i8_tp       = L.pointer_type i8_t in
  (* A Schedule Item Attribute has 3 fields:
   * 1. Name: Points to a string that is its name.
   * 2. Value: Points to the value of the attribute.
   * 3. Next: Points to the next attribute in the linked list.
   *)
  let attr_t = L.named_struct_type context "item_attr" in
  let attr_tp = L.pointer_type attr_t in
  ignore (L.struct_set_body attr_t [| i8_tp; i8_tp; attr_tp |] false);
  (* A Schedule Item struct has 3 fields:
   * 1. Name: Points to a string that is its name (if it has one).
   * 2. Date Time Info: Points to a string that is its date time info.
   * 3. Attributes: A linked list of the item's attributes.
   * 4. Next: Points to the next item in the linked list.
   *)
  let item_t = L.named_struct_type context "item" in
  let item_tp = L.pointer_type item_t in
  ignore (L.struct_set_body item_t [| i8_tp; i8_tp; attr_tp; item_tp |] false);
  (* A Schedule struct has 5 fields:
   * 1. Name: Points to a string that is its name.
   * 2. Start Date: Points to a string that is its start date (for now).
   * 3. n_items: 8-bit int that gives the number of items in the Schedule.
   * 4. Items Linked List: Pointer to the head in a linked list of item.
   *)
  let sched_t     = L.struct_type context
    [| i8_tp; i8_tp ; i8_t ; item_tp |] in

  let ltype_of_typ = function
    A.Int   -> i32_t
  | A.Bool  -> i1_t
  | A.Float -> float_t
  | A.Void  -> void_t
  | A.String -> str_ptr_t
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
  let str_format_str = L.build_global_stringptr "%s\n" "fmt" main_builder
  and int_format_str = L.build_global_stringptr "%d\n" "fmt" main_builder
  and float_format_str = L.build_global_stringptr "%g\n" "fmt" main_builder in
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
    | (A.CId, SId s) -> L.build_global_stringptr s "" builder
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
      | (A.Void, SCall("printi", [sx])) | (A.Void, SCall("printb", [sx])) ->
          let sx' = sxpr the_state sx in
	  L.build_call printf_func [| int_format_str ; sx' |]
	    "printf" builder
      | (A.Void, SCall ("printf", [sx])) -> 
          let sx' = sxpr the_state sx in
	  L.build_call printf_func [| float_format_str ; sx' |]
	    "printf" builder
      | (A.Void, SNoexpr) -> L.const_int i32_t 0
      | (_, SId s)       -> L.build_load (lookup s namespace) s builder
			| (t, SCall(name, args)) ->
				let the_function = lookup name namespace in
				let llargs = Array.of_list (List.rev (List.map (sxpr the_state) (List.rev args))) in
				let result = (match t with
				                        A.Void -> ""
				                      | _ -> name ^ "_result") in
				         L.build_call the_function llargs result the_state.b
      | _ -> raise (Failure "sxpr codegen type not implemented yet.")
  in

  let rec sattr the_state = function
  (* Return a Schedule Item Attribute. *)
    ((A.String, SId(name)), (A.String, SStrLit(val_))) ->
    let attr_ptr = L.build_alloca attr_t "" the_state.b in
    let name_ptr = sxpr the_state (A.String, SStrLit(name)) in
    let aname_ptr = L.build_struct_gep attr_ptr 0 "" the_state.b in
    let val_ptr = sxpr the_state (A.String, SStrLit(val_)) in
    let aval_ptr = L.build_struct_gep attr_ptr 1 "" the_state.b in

    L.build_store name_ptr aname_ptr the_state.b;
    L.build_store val_ptr aval_ptr the_state.b;
    attr_ptr
  | _ -> raise (Failure "sattr not implemented yet.")
  in

  let rec sattrs_linked_list the_state = function
  (* Return the head of a linked list of item attributes. *)
    [] -> L.const_null attr_tp
  | str :: strs -> 
    (* 1. Generate the code for the head of the list.
     * 2. Recursively process the tail of the list, and receive,
     *    a pointer to the start of the tail.
     * 3. Link the head to the tail.
     * 4. Return a pointer to the head.
     *)
    let head_ptr = sattr the_state str in
    let tail_ptr = sattrs_linked_list the_state strs in
    let hnext_ptr = L.build_struct_gep head_ptr 2 "" the_state.b in

    L.build_store tail_ptr hnext_ptr the_state.b;
    head_ptr
  in

  let rec sitem_spec the_state = function
    (* Semantic Checks:
     * 1. Allocate the space for an item. An item consists of a *)
    SAnon(kind, dt_opt, strs) ->
      (* An anonymous schedule item.
       * 1. Allocate space for the item. *)
      let item = L.build_alloca item_t "" the_state.b in
      let dt_ptr =
        (match dt_opt with
          Some(dt_sx) -> sxpr the_state dt_sx
        | _ -> sxpr the_state (A.String, SStrLit "None"))
      in
      let idate_ptr = L.build_struct_gep item 1 "" the_state.b in
      let strs_ptr = sattrs_linked_list the_state strs in
      let istrs_ptr = L.build_struct_gep item 2 "" the_state.b in

      L.build_store dt_ptr idate_ptr the_state.b;
      L.build_store strs_ptr istrs_ptr the_state.b;
      item
    | _ -> raise (Failure "sitem_spec not implemented yet.")
  in

  let rec sitems_linked_list the_state = function
  (* Return the head of a linked list of items. *)
    [] -> L.const_null item_tp
  | item :: items ->
    let head_ptr = sitem_spec the_state item in
    let tail_ptr = sitems_linked_list the_state items in
    let hnext_ptr = L.build_struct_gep head_ptr 3 "" the_state.b in

    L.build_store tail_ptr hnext_ptr the_state.b;
    head_ptr
  in

  let rec ssched_spec the_state = function
    SNamed(kind, sx_opt, sx, sil_items) ->
      (* Create a global string constant for the name. *)
      let name_ptr = sxpr the_state sx in
      (* Allocate space on the stack for the name. *)
      let sched = L.build_alloca sched_t "" the_state.b in
      (* Store value of name_ptr to the name field of this Schedule. *)
      let sname_ptr = L.build_struct_gep sched 0 "" the_state.b in
      (* Store value of start date info (if given) *)
      let date_ptr =
        (match sx_opt with
          Some(dt_sx) -> sxpr the_state dt_sx
        | _ -> sxpr the_state (A.String, SStrLit "None"))
      in
      let sdate_ptr = L.build_struct_gep sched 1 "" the_state.b in
      let n_items = L.const_int i8_t (List.length sil_items) in
      let sn_items_ptr = L.build_struct_gep sched 2 "" the_state.b in
      let items_ptr = sitems_linked_list the_state sil_items in
      let sitems_ptr = L.build_struct_gep sched 3 "" the_state.b in

      L.build_store name_ptr sname_ptr the_state.b;
      L.build_store date_ptr sdate_ptr the_state.b;
      L.build_store n_items sn_items_ptr the_state.b;
      L.build_store items_ptr sitems_ptr the_state.b 
  | _ -> raise (Failure "ssched_spec case not implemented yet.")
  in

  let rec scstmt the_state = function
    SSchedule(spec) ->
      ignore (ssched_spec the_state spec); the_state
  | SType(spec) ->
      (* No code is generated for a user defined type; the definition
       * is used for semantic checks.
       * Return the_state.
       *)
      the_state
  | _ -> raise (Failure "screate_stmt case not implemented yet.")
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
			let new_state = List.fold_left sstmt new_state sl in
			let end_state = change_builder_state the_state new_state.b in end_state
    | SExpr(sx) -> ignore (sxpr the_state sx); the_state
    | SCS(cs) -> ignore (scstmt the_state cs); the_state
    | SIf (predicate, then_stmt, else_stmt) ->
      let bool_val = sxpr the_state predicate in
      let merge_bb = L.append_block context "merge" the_state.func in  
      let build_br_merge = L.build_br merge_bb in 
      let then_bb = L.append_block context "then" the_state.func in
      let new_state1 = change_builder_state the_state (L.builder_at_end context then_bb) in
      add_terminal (sstmt new_state1 then_stmt) build_br_merge;
      let else_bb = L.append_block context "else" the_state.func in
      let new_state2 = change_builder_state the_state (L.builder_at_end context else_bb) in
      add_terminal (sstmt new_state2 else_stmt) build_br_merge;
      ignore(L.build_cond_br bool_val then_bb else_bb the_state.b);
      let new_state = change_builder_state the_state (L.builder_at_end context merge_bb) in new_state

    | SWhile (predicate, body) ->
      let pred_bb = L.append_block context "while" the_state.func in
        ignore(L.build_br pred_bb the_state.b);
      let body_bb = L.append_block context "while_body" the_state.func in
      let new_state = change_builder_state the_state (L.builder_at_end context body_bb) in
        add_terminal (sstmt new_state body) (L.build_br pred_bb);
      let pred_builder = L.builder_at_end context pred_bb in
      let new_state = change_builder_state the_state (L.builder_at_end context pred_bb) in
      let bool_val = sxpr new_state predicate in
      let merge_bb = L.append_block context "merge" the_state.func in
        ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
      let new_state = change_builder_state the_state (L.builder_at_end context merge_bb) in
        new_state
    | SFor (e1, e2, e3, body) -> sstmt the_state
      ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
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
					state
				| s -> sstmt state s
				| _ -> raise (Failure "failed to match func_sstmt")
			in

			let new_scope = StringMap.empty in
			let new_st = {scope=new_scope; parent=Some(the_state.namespace)} in
    	let the_function = lookup fdecl.sfname new_st in
    	let buidler = L.builder_at_end context (L.entry_block the_function) in
			let new_state = {namespace=new_st; func=the_state.func; b = buidler} in
			let end_state = List.fold_left func_sstmt new_state fdecl.sbody in
			add_terminal end_state (match fdecl.styp with
	        A.Void -> L.build_ret_void
	      | A.Float -> L.build_ret (L.const_float float_t 0.0)
	      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0));
 			the_state
    | _ -> raise (Failure "sstmt codegen type not implemented yet.")
  in

  (*List.iter (fun stmt -> ignore(sstmt the_state.b stmt)) sprogram;
  ignore(L.build_ret (L.const_int i32_t 0) the_state.b); *)
  let final_state = sstmt the_state (SBlock(sprogram)) in
  ignore(L.build_ret (L.const_int i32_t 0) final_state.b);
  the_module
