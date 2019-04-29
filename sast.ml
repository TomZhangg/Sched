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
| SFLit of string
| SIntLit of int
| SBinop of sexpr * op * sexpr
| SUnop of uop * sexpr

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
  | SIntLit(lit) -> idnt ^ "typ: " ^ tp_string ^ ", sx: " ^ string_of_int lit
  | SFLit(lit) -> idnt ^ "typ: " ^ tp_string ^ ", sx: " ^ lit
  | SBinop((i1,e1), o, (i2,e2)) ->
    (string_of_sexpr (lvl + 1) (i1, e1)) ^ " " ^ (string_of_op o) ^ " " ^
    (string_of_sexpr (lvl + 1) (i2, e2))
  | SUnop(o, (i,e)) -> "(" ^ (string_of_uop o) ^ ")" ^ " " ^ (string_of_sexpr (lvl + 1) (i, e))
  | _ -> raise (Failure "string_of_sexpr case not implemented yet.")

type sstmt =
    SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SBlock of sstmt list

let rec string_of_sstmt lvl sstmt =
  let idnt = indent lvl in
  match sstmt with
    SExpr(sxpr) ->
      let prefix = idnt ^ "SExpr(" in
      let suffix = idnt ^ ")" in
      String.concat "\n" [prefix;
                          (string_of_sexpr (lvl + 1) sxpr);
                          suffix]
  | SBlock(sl) -> "{\n" ^ (String.concat "" (List.map (fun stmt -> string_of_sstmt lvl stmt) sl)) ^ "}\n"
  | SIf(e, s, SBlock([])) -> "SIf (" ^ string_of_sexpr lvl e ^ ")\n" ^ (string_of_sstmt lvl s)
  | SIf(e, s1, s2) ->  "SIf (" ^ string_of_sexpr lvl e ^ ")\n" ^
      (string_of_sstmt lvl s1) ^ "SElse\n" ^ (string_of_sstmt lvl s2)
  | _ -> raise (Failure "string_of_sstmt case not implemented yet.")

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    slocals : bind list;
    sbody : sstmt list;
}

type sprogram = sstmt list

let string_of_sprogram sprog =
  let sstmts = List.map (string_of_sstmt 0) sprog in
  String.concat "\n" sstmts

(*
type sattr = sexpr * sexpr
type sattrs = sattr list

type sattrs_opt =
  SSome of sattrs
| SNone

type sitem_spec =
  SNamed of sitem_kind * sdt_info_opt * sid * sattrs_opt
| SAnon of sitem_kind * sdt_info_opt * sattrs_opt

(* TODO: List the item_spec semantic checks that have to be done. *)
type sil_items_opt =
  SSome of sitem_spec list
| SNone

type ssched_spec =
  SNamed of ssched_kind * sstart_date_opt * sid * sil_items_opt
| Anon of ssched_kind * sstart_date_opt * sil_items_opt

type screate_stmt =
  SSchedule of ssched_spec
| SItem of sitem_spec

  SCS of screate_stmt
| SIS of sinsert_stmt
| SSS of sset_stmt

*)
