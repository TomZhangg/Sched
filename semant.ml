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
    let fdecl = StringMap.find f sym_tab in
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
  | IntLit l -> Some((Int, SIntLit l), sym_tab)
  | FLit l -> Some((Float, SFLit l), sym_tab)
  | Binop (e1, op, e2) as e -> (
    let x = check_expr e1 sym_tab in
    let y = check_expr e2 sym_tab in
    match x with
      Some ((t1, e1'), _) ->
        match y with
          Some((t2, e2'), _) ->
          let same = t1 = t2 in
    let ty = match op with
      Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
    | Add | Sub | Mult | Div | Mod when same && t1 = Float -> Float
    | Equal | Neq            when same               -> Bool
    (*| Less | Leq | Greater | Geq
      when same && (t1 = Int || t1 = Float) -> Bool*)
    | And | Or when same && t1 = Bool -> Bool
    | _ -> raise (
        Failure ("illegal binary operator " ^
                 string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                 string_of_typ t2 ^ " in " ^ string_of_expr e))
    in Some((ty, SBinop((t1, e1'), op, (t2, e2'))), sym_tab)       )
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
  | _ -> raise (Check_not_implemented "Ast.expr type")

let rec check_stmt (stmt : stmt)
                   (sym_tab : 'a StringMap.t)
                   : (sstmt * 'a StringMap.t) option =
  (* Given an Ast.stmt, we need to dispatch the semantic
   * check to the appropriate statement type. *)
  match stmt with
    Expr(expr) ->
      let sexpr = check_expr expr sym_tab in
      match sexpr with
        None -> None
      | Some(sexpr, st) -> Some(SExpr(sexpr), st)
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

let print_fdecl = {
  styp = Void;
  sfname = "print";
  sformals = [(String, "text")];
  slocals = [];
  sbody = [];
}
let init_st = StringMap.add "print" print_fdecl StringMap.empty
