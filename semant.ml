(* Semantic checking for the Schedch compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

exception Check_not_implemented of string



let rec check_expr (xpr : expr)
                   (sym_tab : 'a StringMap.t)
                   : (sexpr * 'a StringMap.t) option =
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
    let SFunc(fdecl) = StringMap.find f sym_tab in
    (* Check that the args is a valid list of sexprs. *)
    let sarg_opts = List.map (fun arg -> check_expr arg sym_tab) args in
    let nargs = List.length sarg_opts in
    let neargs = List.length fdecl.sformals in
    if nargs = neargs then
      (* Go through each of the arguments and check that
       * 1. The semantic checks succeeded.
       * 2. The type matches the formal argument type. *)
      let sarg_check (pair : bind * ((sexpr * 'a StringMap.t) option))
                     (acc : bool * sexpr list)
                     : bool * sexpr list =
        let (formal, sarg_opt) = pair in
        let (flag, args) = acc in
        if not flag then (flag, args) else
          match sarg_opt with
            Some(sxpr, st) ->
              let (styp, sx) = sxpr in
              let (ftyp, fid) = formal in
              let argsp = sxpr::args in
              if styp = ftyp then (true, argsp) else (false, argsp)
          | None -> (false, [])
      in
      let zipped = List.combine fdecl.sformals sarg_opts in
      let (flag, sargs) = List.fold_right sarg_check zipped (true, []) in
      if flag then Some((fdecl.styp, SCall(f, sargs)), sym_tab) else None
    else None
  | StrLit(lit) ->
    (* Need to check that the type for this expression is a String. *)
    Some((String, SStrLit(lit)), sym_tab)
  | BoolLit l -> Some((Bool, SBoolLit l), sym_tab)
  | Binop (e1, op, e2) as e ->
    let x = check_expr e1 sym_tab in
    let y = check_expr e2 sym_tab in
    match x with
      Some ((t1, e1'), _) ->
        match y with
          Some((t2, e2'), _) ->
          let same = t1 = t2 in
    let ty = match op with
      Add | Sub | Mult | Div when same && t1 = Int   -> Int
    (* | Add | Sub | Mult | Div when same && t1 = Float -> Float *)
    | Equal | Neq            when same               -> Bool
    (* | Less | Leq | Greater | Geq
      when same && (t1 = Int || t1 = Float) -> Bool *)
    | And | Or when same && t1 = Bool -> Bool
    | _ -> raise (
        Failure ("illegal binary operator " ^
                 string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                 string_of_typ t2 ^ " in " ^ string_of_expr e))
    in Some((ty, SBinop((t1, e1'), op, (t2, e2'))), sym_tab)
  | _ -> raise (Check_not_implemented "Ast.expr type")

let check_create_stmt (cstmt : create_stmt)
                      (sym_tab  : 'a StringMap.t)
                      : (screate_stmt * 'a StringMap.t) option =
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
            let SKind(decl) = StringMap.find kid sym_tab in
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
            try ignore(StringMap.find sid sym_tab); true
            with Not_found -> false
        | _ -> raise (Failure "Invalid id."))
      in
      let sid = 
        (match id with
          Id(str) -> CId, SId(str)
        | _ -> raise (Failure "Named Schedule without a name!"))
      in
      (* TODO: Add inline items option.
       * For now (4/25/19) the SNamed is variant has only an Ast.sched_kind,
       * an sexpr_opt and sexpr. So at this point, we have verified that
       * the Schedule Kind is defined, recursively checked the optional
       * date information, and ensured that the id is not already defined. *)
      Some( SSchedule(SNamed(kind, sst_date_opt, sid)), sym_tab )
    | _ -> raise (Check_not_implemented "Ast.sched_spec variant"))
  | _ -> raise (Check_not_implemented "Ast.create_stmt variant")

let rec check_stmt (stmt : stmt)
                   (sym_tab : 'a StringMap.t)
                   : (sstmt * 'a StringMap.t) option =
  (* Given an Ast.stmt, we need to dispatch the semantic
   * check to the appropriate statement type. *)
  match stmt with
    Expr(expr) ->
      let sexpr = check_expr expr sym_tab in
      (match sexpr with
        None -> None
      | Some(sexpr, st) -> Some(SExpr(sexpr), st))
  | CS(cstmt) ->
      let scstmt = check_create_stmt cstmt sym_tab in
      (match scstmt with
        None -> None
      | Some(sstmt, st) -> Some(SCS(sstmt), st))
  | _ -> raise (Check_not_implemented "Ast.stmt type")

let rec check (prog : program)
              (sym_tab : 'a StringMap.t)
              : (sprogram * 'a StringMap.t) option =
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

let print_decl = SFunc({
  styp = Void;
  sfname = "print";
  sformals = [(String, "text")];
  slocals = [];
  sbody = [];
})

let init_st = StringMap.add "print" print_decl StringMap.empty
