{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "#("          { comment 1 lexbuf }      (* Comments *)
| ';'           { SEMI }
| "Create"      { CREATE }
| "Schedule"    { SCHED }
| "Day"         { DAY }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof           { EOF }

and comment lvl = parse
  ")#"  { if lvl = 1 then token lexbuf else comment (lvl - 1) lexbuf }
| "#("  { comment (lvl + 1) lexbuf }
| _     { comment lvl lexbuf }
