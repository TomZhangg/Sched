%{ open Ast %}


%token SEMI COL COMMA CREATE INSERT DROP ITEM ITEMS SCHED INTO FROM COLLECTION SET OF TO LT GT INDENT
%token FUNC ASSIGN NOT EQ NEQ AND OR LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE MOD
%token DAY WEEK MONTH YEAR
%token EVENT DEADLINE
%token BOOL STRING INT
%token <string> DATELIT
%token <string> TIMELIT
%token <string> ID
%token <bool> BLIT
%token <string> SLIT
%token <int> ILIT
%token EOF


%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left MOD
%left PLUS MINUS
%left TIMES DIVIDE
%left EQ NEQ
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
| expr SEMI     { Expr $1 }
| FUNC id LPAREN params RPAREN COL indent_stmts { DEC($2, $4, $7)}

indent_stmts:
  INDENT stmt   { [$2] }
| INDENT stmt indent_stmts { $2 :: $3 }
| INDENT indent_stmts { $2 }

typ:
  | BOOL  { Bool  }
  | STRING { String }
  | INT    { Int }

expr:
  BLIT               { BoolLit($1)            }
| SLIT               { StrLit ($1) }
| ID                 { Id($1) }
| ILIT                { IntLit($1)}
| expr EQ     expr   { Binop($1, Equal, $3)   }
| expr NEQ    expr   { Binop($1, Neq,   $3)   }
| expr AND    expr   { Binop($1, And,   $3)   }
| expr OR     expr   { Binop($1, Or,    $3)   }
| expr PLUS   expr   { Binop($1, Add,   $3)   }
| expr MINUS  expr   { Binop($1, Sub,   $3)   }
| expr TIMES  expr   { Binop($1, Mult,  $3)   }
| expr DIVIDE expr   { Binop($1, Div,   $3)   }
| expr MOD    expr   { Binop($1, Mod,   $3)   }
| NOT expr           { Unop(Not, $2)          }
| typ ID						 { BIND(Bind($1,$2))}
| typ ID ASSIGN expr { BIND(Bind($1,$2)); Assign($2, $4)     }
| ID LPAREN args_opt RPAREN { Call($1, $3)  }
| LPAREN expr RPAREN { $2                   }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

create_stmt:
  CREATE SCHED sched_spec SEMI  { Schedule($3) }
| CREATE ITEM item_spec SEMI    { Item($3) }

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

sched_spec:
  named_sched_spec      { $1 }
| anon_sched_spec       { $1 }

named_sched_spec:
  sched_kind start_date_opt id il_items_opt  { Named($1, $2, $3, $4) }

anon_sched_spec:
  sched_kind start_date_opt il_items_opt     { Anon($1, $2, $3) }

il_items_opt:
  /* nothing */ { None }
| SCHED ITEMS items     { Some $3 }

items:
  /* nothing */ { [] }
| COMMA items   { $2 }
| anon_item_spec items { $1 :: $2 }

sched_kind:
  DAY   { Day }
| WEEK  { Week }
| MONTH { Month }
| YEAR  { Year }

start_date_opt:
  /* nothing */ { None }
| DATELIT          { Some (TimeLit($1)) }

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

dt_opt:
  /* nothing */ { None }
| TIMELIT       { Some (TimeLit($1)) }

attrs_opt:
  /* nothing */ { None }
| attrs         { Some $1 }

attrs:
  /* nothing */ { [] }
| COMMA attrs { $2 }
| ID COL ID attrs { (Id($1), Id($3))::$4 }

params:
  /* nothing */ { [] }
| param_list  { List.rev $1 }

param_list:
  typ ID                    { [BIND(Bind($1,$2))] }
| param_list COMMA typ ID { BIND(Bind($3,$4)) :: $1 }

id:
  ID    { Id($1) }
