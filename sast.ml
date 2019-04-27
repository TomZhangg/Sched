(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

let rec indent lvl =
  if lvl = 0 then "" else "  " ^ indent (lvl - 1)

(* Note: sx should in the end have one constructor for each
 * constructor in ast.expr. Add constructors as needed. *)
type sexpr = typ * sx
and sx =
  SId of string
| SCall of string * sexpr list
| SStrLit of string
| SBoolLit of bool
| SBinop of sexpr * op * sexpr

let rec string_of_sexpr lvl sxpr =
  let idnt = indent lvl in
  let (tp, sxx) = sxpr in
  let tp_string = string_of_typ tp in
  match sxx with
    SId(id) -> idnt ^ "typ: " ^ tp_string ^ ", sx: SId(" ^ id ^ ")"
  | SCall(id, sxprs) ->
    let prefix = idnt ^ "typ: " ^ tp_string ^ ", sx: SCall(" ^ id ^ ")" in
    let snd_line = idnt ^ "  SExpr List:" in
    let suffix = idnt ^ ")" in
    let lines = List.map (string_of_sexpr (lvl + 1)) sxprs in
    String.concat "\n" ([prefix; snd_line] @ lines @ [suffix])
  | SStrLit(lit) -> idnt ^ "typ: " ^ tp_string ^ ", sx: " ^ lit
	| SBoolLit(true) -> "true"
	| SBoolLit(false) -> "false"
  | SBinop((i1,e1), o, (i2,e2)) ->
    (string_of_sexpr (lvl + 1) (i1, e1)) ^ " " ^ (string_of_op o) ^ " " ^
    (string_of_sexpr (lvl + 1) (i2, e2))
  | _ -> raise (Failure "string_of_sexpr case not implemented yet.")

type sexpr_opt =
  Some of sexpr
| None
let string_of_sexpr_opt lvl opt =
  let idnt = indent lvl in
  match opt with
    Some(sx) -> string_of_sexpr lvl sx
  | None -> idnt ^ "None"

type ssched_spec = 
  (* TODO: Add sil_items_opt *)
  SNamed of Ast.sched_kind * sexpr_opt * sexpr
let string_of_ssched_spec lvl spec =
  let idnt = indent lvl in
  match spec with
    SNamed(kind, sx_opt, sx) ->
      let prefix = idnt ^ "SNamed(" in
      let suffix = idnt ^ ")" in
      String.concat "\n" [prefix;
                          (Ast.pp_sched_kind (lvl + 1) kind);
                          (string_of_sexpr_opt (lvl + 1) sx_opt);
                          (string_of_sexpr (lvl + 1) sx);
                          suffix]
  | _ -> raise (Failure "string_of_ssched_spec case not implemented yet.")

type screate_stmt =
  SSchedule of ssched_spec
let string_of_screate_stmt lvl sc_stmt =
  let idnt = indent lvl in
  match sc_stmt with
    SSchedule(spec) ->
      let prefix = idnt ^ "SSchedule(" in
      let suffix = idnt ^ ")" in
      String.concat "\n" [prefix;
                          (string_of_ssched_spec (lvl + 1) spec);
                          suffix]
  | _ -> raise (Failure "string_of_screate_stmt case not implemented yet.")

type sstmt =
  SExpr of sexpr
| SCS of screate_stmt
let string_of_sstmt lvl sstmt =
  let idnt = indent lvl in
  match sstmt with
    SExpr(sxpr) ->
      let prefix = idnt ^ "SExpr(" in
      let suffix = idnt ^ ")" in
      String.concat "\n" [prefix;
                          (string_of_sexpr (lvl + 1) sxpr);
                          suffix]
  | SCS(cstmt) ->
      let prefix = idnt ^ "SCS(" in
      let suffix = idnt ^ ")" in
      String.concat "\n" [prefix;
                          (string_of_screate_stmt (lvl + 1) cstmt);
                          suffix]
  | _ -> raise (Failure "string_of_sstmt case not implemented yet.")

type sfunc_decl = {
  styp : typ;
  sfname : string;
  sformals : bind list;
  slocals : bind list;
  sbody : sstmt list;
}

type definable_type = Schedule | SchedItem

type skind_decl = {
  sdtype : definable_type;
  skname : string;
  sprops : bind list;
}

type sdecl =
  SFunc of sfunc_decl
| SKind of skind_decl

type sprogram = sstmt list

let string_of_sprogram sprog =
  let sstmts = List.map (string_of_sstmt 0) sprog in
  String.concat "\n" sstmts

