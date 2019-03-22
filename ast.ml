let rec indent lvl =
  if lvl = 0 then "" else "  " ^ indent (lvl - 1)

type typ = Sched | SchedItem | SchedCollection

type sched_kind = Day | Week | Month | Year
let pp_sched_kind lvl kind =
  let prefix = (indent lvl) ^ "<sched-kind>: " in
    match kind with
      Day -> prefix ^ "Day"
    | Week -> prefix ^ "Week"
    | Month -> prefix ^ "Month"
    | Year -> prefix ^ "Year"

type expr =
  Id of string
| StringLit of string
| IntLit of int
| TimeLit of string

let string_of_expr expr =
  match expr with
    Id(str) -> "Id(" ^ str ^ ")"
  | StringLit(str) -> "StringLit(" ^ str ^ ")"
  | IntLit(x) -> "IntLit(" ^ (string_of_int x) ^ ")"
  | TimeLit(str) -> "TimeLit(" ^ str ^ ")"

type date = expr
let pp_date lvl date =
  let idnt = indent lvl in
    idnt ^ string_of_expr date

type id = expr
let pp_id lvl id =
  let prefix = (indent lvl) ^ "<id>: " in
    prefix ^ string_of_expr id

type start_date_opt =
  Some of expr
| None

let pp_start_date_opt lvl start_date_opt =
  let idnt = indent lvl in
  match start_date_opt with
    Some date -> idnt ^ "<start-date-opt>: " ^ (string_of_expr date)
  | None -> idnt ^ "<start-date-opt> None"

type sched_spec =
  Named of sched_kind * start_date_opt * id (* TODO: inline_items_opt *)
  (* TODO: Add anonymous schedule spec. *)
| Anon of sched_kind * start_date_opt (* TODO: Add inline_items_opt *)
let pp_sched_spec lvl sched_spec =
  let idnt = indent lvl in
  match sched_spec with
    Named(kind, opt, id) ->
      let kind_sub_tree = pp_sched_kind (lvl + 1) kind in
      let opt_sub_tree = pp_start_date_opt (lvl + 1) opt in
      let id_sub_tree = pp_id (lvl + 1) id in
        String.concat "\n" [idnt ^ "<named-sched-spec>";
                            kind_sub_tree;
                            opt_sub_tree;
                            id_sub_tree]
  | Anon(kind, opt) ->
      let kind_sub_tree = pp_sched_kind (lvl + 1) kind in
      let opt_sub_tree = pp_start_date_opt (lvl + 1) opt in
        String.concat "\n" [idnt ^ "<anon-sched-spec>";
                            kind_sub_tree;
                            opt_sub_tree]

type create_stmt =
  Schedule of sched_spec
  (* TODO: Add Item insert statements *)
let pp_create_stmt lvl create_stmt =
  match create_stmt with
    Schedule(spec) ->
      let idnt = indent lvl in
      let sub_tree = pp_sched_spec (lvl + 1) spec in
        idnt ^ "<create-sched-statement>\n" ^ sub_tree

type insert_stmt = 
    Schedule_src_dst of expr * sched_spec
  (* TODO: Add Collection and Item create statements *)
let pp_insert_stmt lvl insert_stmt =
  match insert_stmt with
    Schedule_src_dst(spec1*spec2) ->
      let idnt = indent lvl in
      let sub_tree2 = pp_sched_spec (lvl + 1) spec in
        idnt ^ "<insert-sched-statement>\n" ^ sub_tree

type stmt =
  create_stmt
| insert_stmt

let pp_stmt lvl stmt =
  match stmt with
    create_stmt ->
      let idnt = indent lvl in
      let sub_tree = pp_create_stmt (lvl + 1) create_stmt in
        idnt ^ "<create-statement>\n" ^ sub_tree
    insert_stmt ->
      let idnt = indent lvl in
      let sub_tree = pp_insert_stmt (lvl + 1) insert_stmt in
        idnt ^ "<insert-statement>\n" ^ sub_tree

type program = stmt list
let pp_program prog =
  (String.concat "\n" (List.map (fun stmt -> pp_stmt 0 stmt) prog)) ^ "\n"
