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
| SAssign of string * sexpr
| SBIND of typ * string
| SBinAssign of (typ * string) * sexpr
| SNoexpr
and sfunc_decl = {
	    styp : typ;
	    sfname : string;
	    sformals : bind list;
	    sbody : sstmt list;
}
and definable_type = Schedule | SchedItem
and skind_decl = {
  sdtype : definable_type;
  skname : string;
  sprops : (typ * string) list;
}
and sstmt =
    SExpr of sexpr
  | SCS of screate_stmt
  | SIf of sexpr * sstmt * sstmt
  | SBlock of sstmt list
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SFunc of sfunc_decl
  | SRt of sexpr
  | SKind of skind_decl
and screate_stmt =
  SSchedule of ssched_spec
| SItem of sitem_spec
and ssched_spec = 
  (* TODO: Add sil_items_opt *)
  SNamed of Ast.sched_kind * sexpr_opt * sexpr * sil_items
and sitem_spec =
  SAnon of Ast.item_kind * sexpr_opt * sattrs
and sexpr_opt =
  Some of sexpr
| None
and sil_items = sitem_spec list
and sattr = sexpr * sexpr
and sattrs = sattr list

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
  | SAssign(s, e) -> s ^ " = " ^ "\n" ^ string_of_sexpr (lvl+1) e
	| SBIND (t,s) -> (indent lvl) ^ "(" ^ string_of_typ t ^ ", " ^ s ^ ")"
	| SBinAssign (b,a) ->
			(match b with (t,s)->
				"(" ^ string_of_typ t ^ ", " ^ s ^ ")" ^ string_of_sexpr (lvl+1) a)
	| SNoexpr -> ""
  | _ -> raise (Failure "string_of_sexpr case not implemented yet.")

let string_of_sexpr_opt lvl opt =
  let idnt = indent lvl in
  match opt with
    Some(sx) -> string_of_sexpr lvl sx
  | None -> idnt ^ "None"

let string_of_sattr lvl sttr =
  let idnt = indent lvl in
  let prefix = idnt ^ "sattr(" in
  let suffix = idnt ^ ")" in
  let e1, e2 = sttr in
  let e1_str = string_of_sexpr (lvl + 1) e1 in
  let e2_str = string_of_sexpr (lvl + 1) e2 in
  String.concat "\n" [prefix; e1_str; e2_str; suffix]

let string_of_sattrs lvl sttrs =
  let idnt = indent lvl in
  let prefix = idnt ^ "sattrs(" in
  let suffix = idnt ^ ")" in
  let sattr_strings = List.map (fun sttr -> string_of_sattr (lvl + 1) sttr) sttrs in
  String.concat "\n" (List.concat [[prefix]; sattr_strings; [suffix]])

let string_of_sitem_spec lvl spec =
  let idnt = indent lvl in
  match spec with
    SAnon(kind, sx_opt, sttrs) ->
    let prefix = idnt ^ "SAnon(" in
    let suffix = idnt ^ ")" in
    String.concat "\n" [prefix;
                        (Ast.pp_item_kind (lvl + 1) kind);
                        (string_of_sexpr_opt (lvl + 1) sx_opt);
                        (string_of_sattrs (lvl + 1) sttrs);
                        suffix]
  | _ -> raise (Failure "string_of_sitem_spec case not implemented yet.")

let string_of_sil_items lvl items =
  let idnt = indent lvl in
  let prefix = idnt ^ "sil_items(" in
  let suffix = idnt ^ ")" in
  let item_strings = List.map (fun spec -> string_of_sitem_spec (lvl + 1) spec) items in
  String.concat "\n" (List.concat [[prefix]; item_strings; [suffix]])

let string_of_ssched_spec lvl spec =
  let idnt = indent lvl in
  match spec with
    SNamed(kind, sx_opt, sx, sil_items) ->
      (* TODO: Add string for items. *)
      let prefix = idnt ^ "SNamed(" in
      let suffix = idnt ^ ")" in
      String.concat "\n" [prefix;
                          (Ast.pp_sched_kind (lvl + 1) kind);
                          (string_of_sexpr_opt (lvl + 1) sx_opt);
                          (string_of_sexpr (lvl + 1) sx);
                          (string_of_sil_items (lvl + 1) sil_items);
                          suffix]
  | _ -> raise (Failure "string_of_ssched_spec case not implemented yet.")

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

let rec string_of_sstmt lvl sstmt =
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
  | SBlock(sl) -> "{\n" ^ (String.concat "" (List.map (fun stmt -> string_of_sstmt lvl stmt) sl)) ^ "}\n"
  | SIf(e, s, SBlock([])) -> "SIf (" ^ string_of_sexpr lvl e ^ ")\n" ^ (string_of_sstmt lvl s)
  | SIf(e, s1, s2) ->  "SIf (" ^ string_of_sexpr lvl e ^ ")\n" ^
      (string_of_sstmt lvl s1) ^ "SElse\n" ^ (string_of_sstmt lvl s2)
	| SFunc sf ->
		let idnt = indent lvl in
		let idnt2 = indent (lvl+1) in
		let id_pp = idnt2 ^ sf.sfname in
		let sub_tree = (String.concat "\n" (List.map (fun stmt -> string_of_sstmt (lvl + 1) stmt) sf.sbody)) in
		idnt ^ "<function-definition>\n"
		^ idnt2 ^ string_of_typ sf.styp ^ "\n"
		^ id_pp ^ "\n"
		^ idnt2 ^ "<parameters>: " ^ String.concat ", " (List.map string_of_bind  sf.sformals) ^ "\n"
		^ idnt2 ^ "<body>: " ^ sub_tree
	| SRt e ->
		let idnt = indent lvl in
		let idnt2 = indent (lvl+1) in
		idnt ^ "return: \n" ^ string_of_sexpr (lvl+1) e
  | SFor(e1, e2, e3, s) ->
          "SFor (" ^ string_of_sexpr lvl e1  ^ " ; " ^ string_of_sexpr lvl e2 ^ " ; " ^
          string_of_sexpr lvl e3  ^ ")\n" ^ (string_of_sstmt lvl s)
  | SWhile(e, s) -> "SWhile (" ^ string_of_sexpr lvl e ^ ")\n" ^ (string_of_sstmt lvl s)  
  | _ -> raise (Failure "string_of_sstmt case not implemented yet.")


type sprogram = sstmt list

let string_of_sprogram sprog =
  let sstmts = List.map (string_of_sstmt 0) sprog in
  String.concat "\n" sstmts

