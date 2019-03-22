%{ open Ast %}

%token SEMI CREATE SCHED LT GT
%token DAY WEEK MONTH YEAR
%token <string> DATELIT
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
  create_stmt   { $1 }

create_stmt:
  CREATE SCHED sched_spec SEMI  { Schedule($3) }

sched_spec:
  named_sched_spec      { $1 }
| anon_sched_spec       { $1 }

named_sched_spec:
  sched_kind start_date_opt id  { Named($1, $2, $3) }

anon_sched_spec:
  sched_kind start_date_opt     { Anon($1, $2) }

sched_kind:
  DAY   { Day }
| WEEK  { Week }
| MONTH { Month }
| YEAR  { Year }

start_date_opt:
  /* nothing */ { None }
| DATELIT          { Some (TimeLit($1)) }

id:
  ID    { Id($1) }
