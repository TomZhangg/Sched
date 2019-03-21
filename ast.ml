type typ = Sched | SchedItem | SchedCollection

type schedtyp = Day | Week | Month | Year

type subtype = schedtyp

type schedspec =
  Empty

type spec = schedspec

type expr =
  Id of string
| StringLit of string
| IntLit of int

type create_stmt =
  Create of typ * subtype * expr * spec

type stmt =
  create_stmt

type program = stmt list

(* Pretty printing functions *)

let string_of_spec spec =
  match spec with
    Empty -> "<None>"

let string_of_expr expr =
  match expr with
    Id(str) -> "Id(" ^ str ^ ")"
  | StringLit(str) -> "StringLit(" ^ str ^ ")"
  | IntLit(x) -> "IntLit(" ^ (string_of_int x) ^ ")"

let string_of_typ typ =
  match typ with
    Sched -> "Schedule"
  | SchedItem -> "Schedule Item"
  | SchedCollection -> "Schedule Collection"

let string_of_stmt stmt =
  match stmt with
    Create (typ, subtype, expr, spec) -> "<Create Statement> Type: " ^ (string_of_typ typ) ^ " Expr: " ^ (string_of_expr expr) ^ " Spec: " ^ (string_of_spec spec)

let string_of_program prog =
  String.concat "\n" (List.map string_of_stmt prog)

