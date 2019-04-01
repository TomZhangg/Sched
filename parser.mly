%{ open Ast %}

%token SEMI COL COMMA CREATE INSERT ITEM ITEMS SCHED INTO COLLECTION LT GT
%token DAY WEEK MONTH YEAR
%token EVENT DEADLINE
%token <string> DATELIT
%token <string> TIMELIT
%token <string> ID
%token EOF


%start program
%type <Ast.program> program

%%

program:
  stmts EOF { $1 }

stmts:
  /* nothing */ { [] }
| stmt stmts { $1 :: $2 }

stmt:
  create_stmt   { CS($1) }
| insert_stmt   { $1 }

create_stmt:
  CREATE SCHED sched_spec SEMI  { Schedule($3) }
| CREATE ITEM item_spec SEMI    { Item($3) }

insert_stmt:
  insert_stc { IS($1) }
| insert_its { IS($1) }

insert_stc:
  INSERT SCHED id INTO SCHED COLLECTION id SEMI{ Ids(STC,$3, $7) }

insert_its:
  INSERT SCHED ITEM id INTO SCHED id SEMI{ Ids(ITS,$4, $7) }

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

id:
  ID    { Id($1) }

