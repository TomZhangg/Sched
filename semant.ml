(* Semantic checking for the Schedch compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

exception Check_not_implemented of string

type symtable = {
	mutable tb: sstmt StringMap.t;
	parent: symtable option;
}





let check_assign lvaluet rvaluet err =
	       if lvaluet = rvaluet then lvaluet else raise (Failure err)

(* symbol table helper functions *)
let get_parent st =
	match st.parent with
		Some p -> p
	| _ -> raise (Failure ("no parent to refer to"))

let rec check_exist s st =
	if StringMap.mem s st.tb then true
	else match st.parent with
		None -> false
	| _ -> check_exist s (get_parent st)

let rec lookup s st =
	try
		StringMap.find s st.tb
	with Not_found ->
		match st.parent with
			Some(parent) -> lookup s parent
		| _ -> raise Not_found

let type_of_identifier s sym_tab=
	if check_exist s sym_tab
	then let SExpr(t, s) = lookup s sym_tab in t
	else  raise (Failure ("undeclared identifier " ^ s))

let rec check_expr (xpr : expr)
                   (sym_tab : symtable)
                   : (sexpr * symtable) option =
  match xpr with
  Call(f, args) ->
    (* For a function call, we need to check that f is defined,
     * there is an entry in the symbol table for f, and that
     * entry agrees that f is a function. *)
    (* We must also execute the semantic checks on the arguments
     * to f. The number and type of the arguments should match
     * that indicated by f's definition. *)
    (* For our print "Hello, World!" example, this means that the
     * argument should have a printable type (Bool or String). *)
    let fdecl = lookup f sym_tab in
    (* Check that the args is a valid list of sexprs. *)
    let sarg_opts = List.map (fun arg -> check_expr arg sym_tab) args in
    let nargs = List.length sarg_opts in
		(match fdecl with SFunc sf ->
    let neargs = List.length sf.sformals in
    if nargs = neargs then
      (* Go through each of the arguments and check that
       * 1. The semantic checks succeeded.
       * 2. The type matches the formal argument type. *)
      let sarg_check (pair : bind * ((sexpr * symtable) option))
                     (acc : bool * sexpr list)
                     : bool * sexpr list =
        let (formal, sarg_opt) = pair in
        let (flag, args) = acc in
        if not flag then (flag, args) else
          (match sarg_opt with
            Some(sxpr, st) ->
              let (styp, sx) = sxpr in
							(match formal with
								Bind(ftyp,fid) ->
	              let argsp = sxpr::args in
	              if styp = ftyp then (true, argsp) else (false, argsp))
          | None -> (false, []))
      in
      let zipped = List.combine sf.sformals sarg_opts in
      let (flag, sargs) = List.fold_right sarg_check zipped (true, []) in
      if flag then Some((sf.styp, SCall(f, sargs)), sym_tab) else None
    else None)
  | TimeLit(lit) ->
      (* TODO: Implement TimeLit. For now (4/28/19) will just
       * treat like a string for implementing Create statement. *)
      Some((String, SStrLit(lit)), sym_tab)
  | StrLit(lit) ->
    (* Need to check that the type for this expression is a String. *)
    Some((String, SStrLit(lit)), sym_tab)
  | BoolLit l -> Some((Bool, SBoolLit l), sym_tab)
  | IntLit l -> Some((Int, SIntLit l), sym_tab)
  | FLit l -> Some((Float, SFLit l), sym_tab)
	| BIND b ->
		(match b with
			Bind(t,s) ->
			let se = SExpr (t, SNoexpr) in
			sym_tab.tb <- StringMap.add s se sym_tab.tb;
			Some((Void, SBIND(t,s)), sym_tab))
	| Assign (var,e) as ex ->
		let lt = type_of_identifier var sym_tab
		and r = check_expr e sym_tab in
			(match r with Some((rt, e'), st) ->
				let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
				string_of_typ rt ^ " in " ^ string_of_expr ex
		in Some((check_assign lt rt err, SAssign(var, (rt, e'))), sym_tab))
	| BinAssign (b,a) ->
		let b' = BIND b in
		let r1 = check_expr b' sym_tab in
		(match r1 with Some(_,new_tab) ->
			let r2 = check_expr a new_tab in
			r2)
  | Binop (e1, op, e2) as e -> (
    let x = check_expr e1 sym_tab in
    let y = check_expr e2 sym_tab in
    (match x with
      Some ((t1, e1'), _) ->
        match y with
          Some((t2, e2'), _) ->
          let same = t1 = t2 in
    let ty = match op with
      Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
    | Add | Sub | Mult | Div | Mod when same && t1 = Float -> Float
    | Equal | Neq            when same               -> Bool
    | Less | Leq | Greater | Geq
      when same && (t1 = Int || t1 = Float) -> Bool
    | And | Or when same && t1 = Bool -> Bool
    | _ -> raise (
        Failure ("illegal binary operator " ^
                 string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                 string_of_typ t2 ^ " in " ^ string_of_expr e))
    in Some((ty, SBinop((t1, e1'), op, (t2, e2'))), sym_tab))       )
  | Unop(op, e) as ex ->
    ( let x = check_expr e sym_tab in
    match x with
       Some ((t, e'), _) ->
       let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
    in Some((ty, SUnop(op, (t, e'))), sym_tab)   )
	| Noexpr -> Some((Void, SNoexpr), sym_tab)
  | _ -> raise (Check_not_implemented ("Ast.expr type" ^ (string_of_expr xpr)))






let rec check_sattr (att : attr)
                    (ikind : Ast.item_kind)
                    sym_tab : sattr =
  (* Semantic Checks:
   * 1. Recursively check semantics of value.
   * 2. Check that the name of the attribute is defined for the
   *    Item Kind.
   * 3. Check that attribute and value types are the same.
   *
   * Note att is of the form:
   *   (Id(name), expr)
   *)
  let Id(name), value = att in
  let sv_ty, sv =
    (match check_expr value sym_tab with
      Some(sx, st) -> sx
    | None -> raise (Failure "attr value invalid expression"))
  in
  let ik_key =
    (match ikind with
      Id(knm) -> knm
    | _ -> Ast.string_of_item_kind ikind)
  in
  (* The kind declaration has already been checked. *)
  let SKind(decl) = lookup ik_key sym_tab in
  let prop_t, prop_nm = List.find (fun (t, n) -> n = name) decl.sprops in
  if prop_t = sv_ty
  then (prop_t, SId(name)), (prop_t, sv)
  else raise (Failure "property type does not match expression type")

let rec check_sitem_spec (item : item_spec)
                         sym_tab
                         : sitem_spec =
  match item with
    Anon(kind, dt_opt, attrs_opt) ->
    (* Semantic checks:
     * 1. The item kind is valid, either built-in or defined.
     * 2. The dt_opt is valid.
     * 3. The attrs_opt is valid. *)
    let ikind =
      (match kind with
        Id(kid) ->
          let SKind(decl) = lookup kid sym_tab in
          if decl.sdtype = SchedItem
            then kind
            else raise (Failure "Item Kind is not defined")
      | _ -> kind)
    in
    let sdt_opt =
      (match dt_opt with
        None -> None
      | Some(dt) ->
          (match check_expr dt sym_tab with
            Some(sx, st) -> Some(sx)
          | None -> None))
    in
    let sattrs_ =
      (match attrs_opt with
        Some(attrs_) ->
          List.map (fun att -> check_sattr att ikind sym_tab) attrs_
      | None -> [])
    in
    SAnon(ikind, sdt_opt, sattrs_)
  | _ -> raise (Failure "check_sitem_spec case not implemented yet.")

let check_il_items (items : item_spec list)
                   sym_tab
                   : (sitem_spec list) =
  (* TODO: Figure out if it is necessary to return an updated
   * symbol table too. *)
  List.map (fun item -> check_sitem_spec item sym_tab) items

let check_create_stmt (cstmt : create_stmt)
                      (sym_tab : symtable)
                      : (screate_stmt * symtable) option =
  match cstmt with
    Schedule(spec) ->
    (match spec with
      Named(kind, st_date_opt, id, il_items_opt) ->
      (* Semantic checks:
       * 1. Verify that kind is a Schedule Kind, either a built-in
       *    or user-defined kind.
       * 2. Verify that st_date_opt gives only date information.
       * 3. Verify that id is not already defined.
       * 4. TODO: Add semantic checks for il_items_opt *)
      let skind =
        (match kind with
          Id(kid) ->
            (* Curious if this is a compilation error or potential
             * runtime error. *)
            let SKind(decl) = lookup kid sym_tab in
            if decl.sdtype = Schedule
              then kind
              else raise (Failure "Schedule Kind is not defined")
        | _ -> kind)
      in
      let (has_date, sst_date_opt)  =
        (match st_date_opt with
          None -> false, None
        | Some(expr) ->
            (match (check_expr expr sym_tab) with
              Some(sx, st) -> true, Some(sx)
            | None -> false, None))
      in
      (* TODO: Figure out where the id should be entered into the symbol table. *)
      let defined =
        (match id with
          Id(sid) ->
            try ignore(lookup sid sym_tab); true
            with Not_found -> false
        | _ -> raise (Failure "Invalid id."))
      in
      let sid = 
        (match id with
          Id(str) -> CId, SId(str)
        | _ -> raise (Failure "Named Schedule without a name!"))
      in
      let sitems = 
        (match il_items_opt with
          Some(items) -> check_il_items items sym_tab
        | None -> [])
      in
      (* TODO: Add inline items option.
       * For now (4/25/19) the SNamed is variant has only an Ast.sched_kind,
       * an sexpr_opt and sexpr. So at this point, we have verified that
       * the Schedule Kind is defined, recursively checked the optional
       * date information, and ensured that the id is not already defined. *)
      Some( SSchedule(SNamed(kind, sst_date_opt, sid, sitems)), sym_tab )
    | _ -> raise (Check_not_implemented "Ast.sched_spec variant"))
  | _ -> raise (Check_not_implemented "Ast.create_stmt variant")

let rec check_stmt (stmt : stmt)
                   (sym_tab : symtable)
                   : (sstmt * symtable) option =
  (* Given an Ast.stmt, we need to dispatch the semantic
   * check to the appropriate statement type. *)
  match stmt with
    Expr(expr) ->
      let sexpr = check_expr expr sym_tab in
    ( match sexpr with
        None -> None
      | Some(sexpr, st) -> Some(SExpr(sexpr), st) )
  | CS(cstmt) ->
      let scstmt = check_create_stmt cstmt sym_tab in
      (match scstmt with
        None -> None
      | Some(sstmt, st) -> Some(SCS(sstmt), st))
  | If(p, b1, b2) ->
  let check_bool_expr e sym_tab = (
  let e' = check_expr e sym_tab
  and err = "expected Boolean expression in " ^ string_of_expr e in
  match e' with
    None -> raise (Failure err)

  | Some((t', _), st) ->
    if t' != Bool then raise (Failure err) else e'  ) in

      let p' = check_bool_expr p sym_tab in
     (match p' with
      | Some(sexpr, st) ->
      let sstmt1 = check_stmt b1 sym_tab in
     (match sstmt1 with
        None -> None
      | Some (x1, st1) ->
      let sstmt2 = check_stmt b2 sym_tab in
     (match sstmt2 with
        None -> None
      | Some (x2, st2) -> Some(SIf(sexpr, x1, x2), st) )))
  | Block(sl) ->
		let new_tab = {tb=StringMap.empty;parent=Some(sym_tab)} in
    let slist = check_stmt_list sl new_tab in
    ( match slist with
      Some(sl', st) -> Some(SBlock(sl'), get_parent st)
      | None -> None )
	| DEC(t,s,a,b) ->
		let new_tab = {tb=StringMap.empty;parent=Some(sym_tab)} in
		let add_formal st formal =
			(match formal with
				Bind(t,s) ->
				let se = SExpr (t, SNoexpr) in
				st.tb <- StringMap.add s se st.tb;)
		in
		let tmp_fdecl = {
			styp = t;
			sfname = string_of_id s;
			sformals = a;
			sbody = [];
		} in
		let tf = SFunc(tmp_fdecl) in
		sym_tab.tb <- StringMap.add (string_of_id s) tf sym_tab.tb;
		List.iter (add_formal new_tab) a;
		let slist_unrefined = check_stmt_list b new_tab in
		(match slist_unrefined with
			Some(sl,st)->
				let new_fdecl = {
					styp = t;
					sfname = string_of_id s;
					sformals = a;
					sbody = sl;
				} in
		let sf = SFunc(new_fdecl) in
		sym_tab.tb <- StringMap.add (string_of_id s) sf sym_tab.tb;
		Some(sf, sym_tab))
	| Rt e ->
		(match check_expr e sym_tab with
			Some(se,_) ->
				Some(SRt(se), sym_tab))
  | _ -> raise (Check_not_implemented "Ast.stmt type")


and check_stmt_list (sl : stmt list)
              (sym_tab : symtable)
              : (sstmt list * symtable) option =
  match sl with
    stmt :: stmts ->
    let sstmt = check_stmt stmt sym_tab in
    (match sstmt with
       None -> None
     | Some (x, st) ->
       let sstmts = check_stmt_list stmts st in
       ( match sstmts with
           None -> None
         | Some (xs, stf) -> Some (x::xs, stf) ) )
  | [] -> Some ([], sym_tab)





let rec check (prog : program)
              (sym_tab : symtable)
              : (sprogram * symtable) option =
  (* We begin with an Ast.program, and should return an
   * option type of Some Sast.sprogram if the semantic
   * checks succeed, and None otherwise. *)
  (* We must recursively check every Ast.stmt in prog,
   * passing along symbol table information. If the semantic
   * checks on any Ast.stmt fails, then return None,
   * otherwise return the Sast.program that is a list of
   * Sast.sstmt *)
  (* Each recursive call must therefore return an option
   * type of Some of Sast.sstmt * StringMap if the semantic
   * check succeeds, and None otherwise. *)
  match prog with
    stmt :: stmts ->
      let sstmt = check_stmt stmt sym_tab in
      (match sstmt with
         Some (x, st) ->
           let sstmts = check stmts st in
           match sstmts with
             Some (xs, stf) -> Some (x::xs, stf)
           | None -> None
       | None -> None)
  | [] -> Some ([], sym_tab)

(*
let print_decl = SFunc({
  styp = Void;
  sfname = "print";
  sformals = [Bind(String, "text")];
  sbody = [];
<<<<<<< HEAD
})

let event_decl = SKind({
  sdtype = SchedItem;
  skname = "Event";
  sprops = [(String, "desc")];
})

let st1 = StringMap.add "print" print_decl StringMap.empty
let init_st = StringMap.add "Event" event_decl st1
=======
}
let pf = SFunc(print_fdecl)
let init_st_tb = StringMap.add "print" pf StringMap.empty
let init_st = {tb=init_st_tb;parent=None}
>>>>>>> master
*)

let print_decl = SFunc({
  styp = Void;
  sfname = "print";
  sformals = [Bind(String, "text")];
  sbody = [];
})

let event_decl = SKind({
  sdtype = SchedItem;
  skname = "Event";
  sprops = [(String, "desc")];
})


let st1 = StringMap.add "print" print_decl StringMap.empty
let st2 = StringMap.add "Event" event_decl st1
(* let pf = SFunc(print_fdecl) *)
(* let init_st_tb = StringMap.add "print" pf st2 *)
let init_st_tb = st2

let init_st = {tb=init_st_tb;parent=None}
