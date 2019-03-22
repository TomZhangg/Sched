{ open Parser }

let digits = ['0'-'9']
let year = (digits)digits(digits)digits
let month = (digits)digits
let day = (digits)digits

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "#("          { comment 1 lexbuf }      (* Comments *)
| ';'           { SEMI }
| "Create"      { CREATE }
| "Insert"      { INSERT }
| "Schedule"    { SCHED }
| "Item"        { ITEM }
| "Collection"  { COLLECTION }
| "Into"        { INTO }
| "Day"         { DAY }
| "Week"        { WEEK }
| "Month"       { MONTH }
| "Year"        { YEAR }
| '<' year '-' month '-' day '>' as lit  { DATELIT(lit) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof           { EOF }

and comment lvl = parse
  ")#"  { if lvl = 1 then token lexbuf else comment (lvl - 1) lexbuf }
| "#("  { comment (lvl + 1) lexbuf }
| _     { comment lvl lexbuf }
