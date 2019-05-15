%{ open Ast

(* convert the timelits to six-tuple.
datelits will be automatically have 0,0,0 for their hour,minute, and second fields *)
let convert_timelit tl =
  let y = int_of_string (String.sub tl 1 4) in
  let mo = int_of_string (String.sub tl 6 2) in
  let d = int_of_string (String.sub tl 9 2) in
  let h = int_of_string (String.sub tl 12 2) in
  let mi = int_of_string (String.sub tl 15 2) in
  let s = int_of_string (String.sub tl 18 2) in
  TimeLit(y,mo,d,h,mi,s)

let convert_datelit dl =
  let y = int_of_string (String.sub dl 1 4) in
  let mo = int_of_string (String.sub dl 6 2) in
  let d = int_of_string (String.sub dl 9 2) in
  TimeLit(y,mo,d,0,0,0)
%}


%token SEMI COL COMMA CREATE INSERT DROP COPY ITEM ITEMS SCHED INTO FROM COLLECTION SET OF TO LT GT INDENT LEQ GEQ LBRACE RBRACE IF ELSE LBRACK RBRACK FOR WHILE RETURN TYPE DEFSCHEDITEM NEW
%token FUNC ASSIGN NOT EQ NEQ AND OR LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE MOD
%token DAY WEEK MONTH YEAR
%token EVENT DEADLINE
%token BOOL STRING INT FLOAT TIME VOID
%token <string> DATELIT
%token <string> TIMELIT
%token <string> ID
%token <bool> BLIT
%token <string> SLIT FLIT
%token <int> ILIT
%token EOF


%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left MOD
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT



%%

program:
  stmts EOF { $1 }

stmts:
  /* nothing */ { [] }
| stmt stmts { $1 :: $2 }

stmt:
  create_stmt   { CS($1) }
| insert_stmt   { IS($1) }
| set_stmt      { SS($1) }
| drop_stmt     { DS($1) }
| copy_stmt     { CPS($1) }
| expr SEMI     { Expr $1 }
| typ id LPAREN params RPAREN LBRACE stmts RBRACE { DEC($1, $2, $4, $7)}
| LBRACE stmts RBRACE                 { Block($2)    }
| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
| RETURN expr SEMI { Rt($2)}
| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                          { For($3, $5, $7, $9)   }
| WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

indent_stmts:
  INDENT stmt   { [$2] }
| INDENT stmt indent_stmts { $2 :: $3 }
| INDENT indent_stmts { $2 }

typ:
  | BOOL  { Bool  }
  | STRING { String }
  | INT    { Int }
  | FLOAT  { Float }
  | typ LBRACK RBRACK {Array($1)}
  | TIME    { Time }
  | VOID    { Void }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
  BLIT               { BoolLit($1)            }
| SLIT               { StrLit ($1) }
| ID                 { Id($1) }
| ILIT               { IntLit($1)}
| FLIT               { FLit($1)           }
| expr EQ     expr   { Binop($1, Equal, $3)   }
| expr NEQ    expr   { Binop($1, Neq,   $3)   }
| expr LT     expr   { Binop($1, Less,  $3)   }
| expr LEQ    expr   { Binop($1, Leq,   $3)   }
| expr GT     expr   { Binop($1, Greater, $3) }
| expr GEQ    expr   { Binop($1, Geq,   $3)   }
| expr AND    expr   { Binop($1, And,   $3)   }
| expr OR     expr   { Binop($1, Or,    $3)   }
| expr PLUS   expr   { Binop($1, Add,   $3)   }
| expr MINUS  expr   { Binop($1, Sub,   $3)   }
| expr TIMES  expr   { Binop($1, Mult,  $3)   }
| expr DIVIDE expr   { Binop($1, Div,   $3)   }
| expr MOD    expr   { Binop($1, Mod,   $3)   }
| NOT expr           { Unop(Not, $2)          }
| MINUS expr %prec NOT { Unop(Neg, $2)      }
| typ ID						 { BIND(Bind($1,$2))		}
| ID ASSIGN expr		 { Assign($1,$3)				}
| typ ID ASSIGN expr { BinAssign(Bind($1,$2),$4)	}
| ID LPAREN args_opt RPAREN { Call($1, $3)  }
| dt                 {$1}
| LPAREN expr RPAREN { $2                   }
| LBRACK args_list RBRACK { ArrayLit(List.rev $2) }
| ID LBRACK expr RBRACK      { Index($1,$3) }
/*
| assignable ASSIGN expr		 { Assign($1,$3)				}
| typ assignable ASSIGN expr { BinAssign(Bind($1,$2),$4)	}

assignable:
  ID LBRACK expr RBRACK      { Index($1,$3) }
*/

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

create_stmt:
  CREATE SCHED sched_spec SEMI  { Schedule($3) }
| CREATE ITEM item_spec SEMI    { Item($3) }
| CREATE TYPE type_spec SEMI    { Type($3) }

type_spec:
  DEFSCHEDITEM ID properties    { ItemType($2, $3) }

properties:
  /* nothing */                         { [] }
| LPAREN property RPAREN properties     { $2 :: $4 }

property:
  NEW ID typ     { New($3, $2) }

insert_stmt:
  insert_stc { $1 }
| insert_its { $1 }

insert_stc:
  INSERT SCHED id INTO SCHED COLLECTION id SEMI{ Ids(STC,$3, $7) }

insert_its:
  INSERT SCHED ITEM id INTO SCHED id SEMI{ Ids(ITS,$4, $7) }

drop_stmt:
  drop_sfc { $1 }
| drop_ifs { $1 }

drop_sfc:
  DROP SCHED id FROM SCHED COLLECTION id SEMI{ Ids(SFC,$3, $7) }

drop_ifs:
  DROP SCHED ITEM id FROM SCHED id SEMI{ Ids(IFS,$4, $7) }

set_stmt:
  SET id OF id TO expr SEMI{ AIE($2, $4, $6) }

copy_stmt:
  copy_coc { $1 }
| copy_sos { $1 }
| copy_ioi { $1 }

copy_coc:
  COPY SCHED COLLECTION id OF SCHED COLLECTION id SEMI{ Ids(COC,$4, $8) }

copy_sos:
  COPY SCHED id OF SCHED id SEMI{ Ids(SOS,$3, $6) }

copy_ioi:
  COPY SCHED ITEM id OF SCHED ITEM id SEMI{ Ids(IOI,$4, $8) }

sched_spec:
  named_sched_spec      { $1 }
| anon_sched_spec       { $1 }

named_sched_spec:
  sched_kind dt_opt id il_items_opt  { Named($1, $2, $3, $4) }

anon_sched_spec:
  sched_kind dt_opt il_items_opt     { Anon($1, $2, $3) }

il_items_opt:
  /* nothing */ { None }
| SCHED ITEMS items     { Some $3 }

items:
  /* nothing */ { [] }
| LPAREN anon_item_spec RPAREN items { $2 :: $4 }

sched_kind:
  DAY   { Day }
| WEEK  { Week }
| MONTH { Month }
| YEAR  { Year }
| ID    { Id($1) }


item_spec:
  named_item_spec       { $1 }
| anon_item_spec        { $1 }

named_item_spec:
  item_kind dt_opt id attrs_opt   { Named($1, $2, $3, $4) }

anon_item_spec:
  item_kind dt_opt attrs_opt      { Anon($1, $2, $3) }

item_kind:
  EVENT    { Event }
| DEADLINE { Deadline }
| ID       { Id($1) }

dt:
| TIMELIT       { convert_timelit $1 }
| DATELIT       { convert_datelit $1 }

dt_opt:
  /* nothing */ { None }
| TIMELIT       { Some (convert_timelit $1) }
| DATELIT       { Some (convert_datelit $1) }

attrs_opt:
  /* nothing */ { None }
| attrs         { Some $1 }

attrs:
  /* nothing */ { [] }
| COMMA attrs { $2 }
| ID COL expr attrs { (Id($1), $3)::$4 }

params:
  /* nothing */ { [] }
| param_list  { List.rev $1 }

param_list:
  typ ID                    { [Bind($1,$2)] }
| param_list COMMA typ ID { Bind($3,$4) :: $1 }

id:
  ID    { Id($1) }
