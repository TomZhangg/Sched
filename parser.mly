%{ open Ast %}

%token SEMI CREATE SCHED DAY
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
  CREATE SCHED schedtyp ID schedspec SEMI  { Create(Sched, $3, Id($4), $5) }

schedspec:
  /* nothing */ { Empty }

schedtyp:
  DAY   { Day }
