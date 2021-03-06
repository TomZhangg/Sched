let rec indent lvl =
  if lvl = 0 then "" else "  " ^ indent (lvl - 1)

type op = Equal | Neq | And | Or | Add | Sub | Mult | Div | Mod | Less | Leq | Greater | Geq

type uop = Not | Neg

type typ = Sched | SchedItem | SchedCollection | Bool | String | Int | Void | Float | CId | Array of typ | Time

type bind = Bind of typ * string


type sched_kind = Day | Week | Month | Year | Id of string

type item_kind = Event | Deadline | Id of string
let pp_item_kind lvl kind =
  let prefix = (indent lvl) ^ "<item-kind>: " in
    match kind with
      Event -> prefix ^ "Event"
    | Deadline -> prefix ^ "Deadline"
    | Id(id) -> prefix ^ id
let string_of_item_kind = function
  Event -> "Event"
| Deadline -> "Deadline"
| Id(name) -> name

type src_dst = STC | ITS | SFC | IFS | COC | SOS | IOI

let pp_sched_kind lvl kind =
  let prefix = (indent lvl) ^ "<sched-kind>: " in
    match kind with
      Day -> prefix ^ "Day"
    | Week -> prefix ^ "Week"
    | Month -> prefix ^ "Month"
    | Year -> prefix ^ "Year"

let string_of_op o =
  match o with
    Equal -> "=="
  | Neq -> "!="
  | And -> "&&"
  | Or -> "||"
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"


let string_of_uop o =
  match o with
  Not -> "!"
  | Neg -> "-"

let rec string_of_typ t =
  match t with
    Sched
  | SchedItem
  | SchedCollection
  | Bool -> "bool"
  | String -> "str"
  | Int -> "int"
  | Void -> "void"
  | CId -> "CId"
  | Float -> "float"
  | Array(t) -> (string_of_typ t) ^ "[]"
  | Time -> "time"

let string_of_bind b =
		match b with
			Bind(a,b) -> "(" ^ string_of_typ a ^ ", "^ b ^ ")"

(* Timelit is year,month,date,... in order *)
type expr =
  Id of string
| IntLit of int
| FLit of string
| TimeLit of int*int*int*int*int*int
| BoolLit of bool
| StrLit of string
| Binop of expr * op * expr
| Unop of uop * expr
| Assign of string * expr
| Call of string * expr list
| BIND of bind
| BinAssign of bind * expr
| Noexpr
| ArrayLit of expr list
| Index of string * expr

let string_of_tl tl =
  match tl with
    (y,mo,d,h,mi,s) ->
    "<" ^ string_of_int y ^ "-" ^ string_of_int mo ^ "-" ^string_of_int d ^ "T" ^
    string_of_int h ^ ":" ^ string_of_int mi ^ ":" ^ string_of_int s ^ ">"
  | _ -> ""

let rec string_of_expr expr = (
  match expr with
    Id(str) -> "Id(" ^ str ^ ")"
  | FLit(str) -> "FLit(" ^ str ^ ")"
  | StrLit(str) -> "StrLit(" ^ str ^ ")"
  | IntLit(x) -> "IntLit(" ^ (string_of_int x) ^ ")"
  | TimeLit(y,mo,d,h,mi,s) -> "TimeLit(" ^
    string_of_tl (y,mo,d,h,mi,s) ^
    ")"
  | BoolLit(true) -> "BoolLit(" ^ "true" ^ ")"
  | BoolLit(false) -> "BoolLit(" ^ "false" ^ ")"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(s, e) -> s ^ " = " ^ string_of_expr e
	| Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	| BIND b -> string_of_bind b
	| BinAssign (b,a) ->
		(match b with Bind(t,s)->
			string_of_bind b ^ string_of_expr a)
	| Noexpr -> ""
  | ArrayLit(l) -> "[" ^ String.concat "," (List.map string_of_expr (l)) ^ "]"
  | Index(s, e) -> s ^ "[" ^ (string_of_expr e) ^ "]" )

let string_of_id i =
	match i  with Id(s) -> s

type date = expr

type time = expr

type id = expr
let pp_id lvl id =
  let prefix = (indent lvl) ^ "<id>: " in
    prefix ^ string_of_expr id

type dt_info_opt =
  Some of expr
| None
let pp_start_date_opt lvl start_date_opt =
  let idnt = indent lvl in
  match start_date_opt with
    Some date -> idnt ^ "<start-date-opt>: " ^ (string_of_expr date)
  | None -> idnt ^ "<start-date-opt> None"


let pp_dt_info_opt lvl opt =
  let idnt = indent lvl in
  match opt with
    Some expr -> idnt ^ "<date-time-info-opt>: " ^ (string_of_expr expr)
  | None -> idnt ^ " <date-time-info-opt>: None"

type attr = expr * expr
type attrs = attr list

type attrs_opt =
  Some of attrs
| None
let pp_attrs_opt lvl opt =
  let idnt = indent lvl in
  match opt with
    Some attrs ->
      let sub_idnt = indent (lvl + 1) in
      let sub_tree_list =
        (List.map (fun (e1, e2) -> sub_idnt ^
                                   "Type: " ^ (string_of_expr e1) ^ ", Name: " ^
                                   (string_of_expr e2)) attrs) in
        String.concat "\n" ((idnt ^ "<attrs-opt>:")::sub_tree_list)
  | None -> idnt ^ "<attrs-opt>: None"

type item_spec =
  Named of item_kind * dt_info_opt * id * attrs_opt
| Anon of item_kind * dt_info_opt * attrs_opt
let pp_item_spec lvl item_spec =
  let idnt = indent lvl in
  match item_spec with
    Named(kind, opt, id, attrs_opt) ->
      let kind_sub_tree = pp_item_kind (lvl + 1) kind in
      let opt_sub_tree = pp_dt_info_opt (lvl + 1) opt in
      let id_sub_tree = pp_id (lvl + 1) id in
      let attrs_sub_tree = pp_attrs_opt (lvl + 1) attrs_opt in
        String.concat "\n" [idnt ^ "<named-item-spec>";
                            kind_sub_tree;
                            opt_sub_tree;
                            id_sub_tree;
                            attrs_sub_tree]
  | Anon(kind, opt, attrs_opt) ->
      let kind_sub_tree = pp_item_kind (lvl + 1) kind in
      let opt_sub_tree = pp_dt_info_opt (lvl + 1) opt in
      let attrs_sub_tree = pp_attrs_opt (lvl + 1) attrs_opt in
        String.concat "\n" [idnt ^ "<anon-item-spec>";
                            opt_sub_tree;
                            kind_sub_tree;
                            attrs_sub_tree]

type il_items_opt =
  Some of item_spec list
| None
let pp_items_opt lvl opt =
  let idnt = indent lvl in
  match opt with
    None -> idnt ^ "<inline-items>: None"
  | Some items ->
    let sub_items_list = List.map (pp_item_spec (lvl + 1)) items in
    String.concat "\n" ((idnt ^ "<inline-items>:")::sub_items_list)

type sched_spec =
  Named of sched_kind * dt_info_opt * id * il_items_opt
| Anon of sched_kind * dt_info_opt * il_items_opt
let pp_sched_spec lvl sched_spec =
  let idnt = indent lvl in
  match sched_spec with
    Named(kind, opt, id, items) ->
      let kind_sub_tree = pp_sched_kind (lvl + 1) kind in
      let opt_sub_tree = pp_start_date_opt (lvl + 1) opt in
      let id_sub_tree = pp_id (lvl + 1) id in
      let items_sub_tree = pp_items_opt (lvl + 1) items in
        String.concat "\n" [idnt ^ "<named-sched-spec>";
                            kind_sub_tree;
                            opt_sub_tree;
                            id_sub_tree;
                            items_sub_tree]
  | Anon(kind, opt, items) ->
      let kind_sub_tree = pp_sched_kind (lvl + 1) kind in
      let opt_sub_tree = pp_start_date_opt (lvl + 1) opt in
      let items_sub_tree = pp_items_opt (lvl + 1) items in
        String.concat "\n" [idnt ^ "<anon-sched-spec>";
                            kind_sub_tree;
                            opt_sub_tree;
                            items_sub_tree]

(* TODO: Add support for existing properties. *)
type property =
  New of typ * string
let pp_property lvl prop =
  let ot_idnt = indent lvl in
  let in_idnt = indent (lvl + 1) in
  match(prop) with
    New(type_, name) ->
      String.concat "\n" [ot_idnt ^ "<new-property>";
                          in_idnt ^ name;
                          in_idnt ^ (string_of_typ type_)]

type type_spec =
  ItemType of string * property list
let pp_type_spec lvl type_spec =
  let ot_idnt = indent lvl in
  let in_idnt = indent (lvl + 1) in
  match(type_spec) with
    ItemType(name, properties) ->
      let props_lines = List.map (fun prop -> pp_property (lvl + 2) prop) properties in
      String.concat "\n" ([ot_idnt ^ "<item-type-spec>";
                           in_idnt ^ "Name: " ^ name;
                           in_idnt ^ "Properties:"]
                        @ props_lines )

type create_stmt =
  Schedule of sched_spec
| Item of item_spec
| Type of type_spec
let pp_create_stmt lvl create_stmt =
  let idnt = indent lvl in
  match create_stmt with
    Schedule(spec) ->
      let sub_tree = pp_sched_spec (lvl + 1) spec in
        idnt ^ "<create-sched-statement>\n" ^ sub_tree
  | Item(spec) ->
      let sub_tree = pp_item_spec (lvl + 1) spec in
        idnt ^ "<create-item-statement>\n" ^ sub_tree
  | Type(spec) ->
      let sub_tree = pp_type_spec (lvl + 1) spec in
        idnt ^ "<create-type-statement>\n" ^ sub_tree



type insert_stmt =
    Ids of src_dst * id * id
  (* TODO: Add Collection and Item insert statements *)
let pp_insert_stmt lvl insert_stmt =
  match insert_stmt with
    Ids (STC,src,dst) ->
      let idnt = indent lvl in
      let expr1 = pp_id (lvl + 1) src in
      let expr2 = pp_id (lvl + 1) dst in
        idnt ^ "<insert-sched-to-coll-statement>\n" ^ expr1 ^ "\n" ^ expr2
  | Ids (ITS,src,dst) ->
      let idnt = indent lvl in
      let expr1 = pp_id (lvl + 1) src in
      let expr2 = pp_id (lvl + 1) dst in
        idnt ^ "<insert-item-to-sched-statement>\n" ^ expr1 ^ "\n" ^ expr2

type drop_stmt =
Ids of src_dst * id * id
let pp_drop_stmt lvl drop_stmt =
  match drop_stmt with
    Ids (SFC,src,dst) ->
      let idnt = indent lvl in
      let expr1 = pp_id (lvl + 1) src in
      let expr2 = pp_id (lvl + 1) dst in
        idnt ^ "<drop-sched-from-coll-statement>\n" ^ expr1 ^ "\n" ^ expr2
  | Ids (IFS,src,dst) ->
      let idnt = indent lvl in
      let expr1 = pp_id (lvl + 1) src in
      let expr2 = pp_id (lvl + 1) dst in
        idnt ^ "<drop-item-from-sched-statement>\n" ^ expr1 ^ "\n" ^ expr2

type set_stmt =
  AIE of id * id * expr
let pp_set_stmt lvl set_stmt =
  match set_stmt with
    AIE (aid,id,texpr) ->
      let idnt = indent lvl in
      let expr1 = pp_id (lvl + 1) aid in
      let expr2 = pp_id (lvl + 1) id in
      let expr3 = string_of_expr texpr in
      idnt ^ "<attribute id>"
      ^ expr1 ^ "\n" ^ idnt ^ "<destination id>" ^ expr2 ^ "\n" ^ idnt ^ "<expression>: " ^ expr3

type copy_stmt =
Ids of src_dst * id * id
let pp_copy_stmt lvl copy_stmt =
  match copy_stmt with
    Ids (COC,src,dst) ->
      let idnt = indent lvl in
      let expr1 = pp_id (lvl + 1) src in
      let expr2 = pp_id (lvl + 1) dst in
        idnt ^ "<copy-coll-of-coll-statement>\n" ^ expr1 ^ "\n" ^ expr2
  | Ids (SOS,src,dst) ->
      let idnt = indent lvl in
      let expr1 = pp_id (lvl + 1) src in
      let expr2 = pp_id (lvl + 1) dst in
        idnt ^ "<copy-sched-of-sched-statement>\n" ^ expr1 ^ "\n" ^ expr2
  | Ids (IOI,src,dst) ->
      let idnt = indent lvl in
      let expr1 = pp_id (lvl + 1) src in
      let expr2 = pp_id (lvl + 1) dst in
        idnt ^ "<copy-item-of-item-statement>\n" ^ expr1 ^ "\n" ^ expr2



type args = expr list

type stmt =
  CS of create_stmt
  | IS of insert_stmt
  | SS of set_stmt
  | DS of drop_stmt
  | CPS of copy_stmt
  | Expr of expr
  | DEC of typ * id * bind list * stmt list
  | Block of stmt list
  | If of expr * stmt * stmt
	| Rt of expr
  | For of expr * expr * expr * stmt
  | While of expr * stmt

let rec pp_stmt lvl stmt =
  match stmt with
    CS create_stmt ->
      let idnt = indent lvl in
      let sub_tree = pp_create_stmt (lvl + 1) create_stmt in
        idnt ^ "<create-statement>\n" ^ sub_tree
  | IS insert_stmt ->
      let idnt = indent lvl in
      let sub_tree = pp_insert_stmt (lvl + 1) insert_stmt in
      idnt ^ "<insert-statement>\n" ^ sub_tree
  | SS set_stmt ->
    let idnt = indent lvl in
    let sub_tree = pp_set_stmt (lvl + 1) set_stmt in
      idnt ^ "<set-statement>\n" ^ sub_tree
  | DS drop_stmt ->
    let idnt = indent lvl in
    let sub_tree = pp_drop_stmt (lvl + 1) drop_stmt in
    idnt ^ "<drop-statement>\n" ^ sub_tree
  | CPS copy_stmt ->
    let idnt = indent lvl in
    let sub_tree = pp_copy_stmt (lvl + 1) copy_stmt in
    idnt ^ "<copy-statement>\n" ^ sub_tree
  | Expr(expr) -> string_of_expr expr ^ ";"
  | DEC(t,i,a,b) ->
    let idnt = indent lvl in
    let idnt2 = indent (lvl+1) in
    let id_pp = idnt2 ^ string_of_expr i in
    let sub_tree = (String.concat "\n" (List.map (fun stmt -> pp_stmt (lvl + 1) stmt) b)) in
    idnt ^ "<function-definition>\n"
		^ idnt2 ^ (string_of_typ t) ^  "\n"
    ^ id_pp ^ "\n"
    ^ idnt2 ^ "<parameters>: " ^String.concat ", " (List.map string_of_bind a) ^ "\n"
    ^ idnt2 ^ "<body>: " ^ sub_tree
  | Block(sl) -> "{\n" ^ (String.concat "" (List.map (fun stmt -> pp_stmt lvl stmt) sl)) ^ "}\n"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ (pp_stmt lvl s)
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      (pp_stmt lvl s1) ^ "else\n" ^ (pp_stmt lvl s2)
	| Rt e ->
    let idnt = indent lvl in
    let idnt2 = indent (lvl+1) in
		idnt ^ "<return>" ^ "\n" ^ idnt2 ^ string_of_expr e
  | For(e1, e2, e3, s) ->
          "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
          string_of_expr e3  ^ ")\n" ^ (pp_stmt lvl s)
  | While(e, s) -> "while (" ^ string_of_expr e ^ ")\n" ^ (pp_stmt lvl s)


type program = stmt list
let pp_program prog =
  (String.concat "\n" (List.map (fun stmt -> pp_stmt 0 stmt) prog)) ^ "\n"
