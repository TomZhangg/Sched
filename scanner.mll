{ open Parser }

let digits = ['0'-'9']
let year = (digits)digits(digits)digits
let month = (digits)digits
let day = (digits)digits
let hour = (digits)digits
let minute = (digits)digits
let second = (digits)digits

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "#("          { comment 1 lexbuf }      (* Comments *)
| '('           { LPAREN }
| ')'           { RPAREN }
| ';'           { SEMI }
| ':'           { COL }
| ','           { COMMA }
| '='           { ASSIGN }
| "=="          { EQ }
| "!="          { NEQ }
| "&&"          { AND }
| "||"          { OR }
| "!"           { NOT }
| "Create"      { CREATE }
| "Insert"      { INSERT }
| "Schedule"    { SCHED }
| "Item"        { ITEM }
| "Items"       { ITEMS }
| "Collection"  { COLLECTION }
| "Into"        { INTO }
| "Set"         { SET }
| "Of"          { OF }
| "To"          { TO }
| "Day"         { DAY }
| "Week"        { WEEK }
| "Month"       { MONTH }
| "Year"        { YEAR }
| "Event"       { EVENT }
| "Deadline"    { DEADLINE }
| "bool"        { BOOL }
| "str"         { STRING }
| "True"        { BLIT(true)  }
| "False"       { BLIT(false) }
| "print"       { PRINT }
| '"'([^'"']*)'"' as s { SLIT(s) }
| "func"        { FUNC }
| '<' year '-' month '-' day '>' as lit  { DATELIT(lit) }
| '<' year '-' month '-' day 'T' hour ':' minute ':' second '>' as lit  { TIMELIT(lit) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof           { EOF }

and comment lvl = parse
  ")#"  { if lvl = 1 then token lexbuf else comment (lvl - 1) lexbuf }
| "#("  { comment (lvl + 1) lexbuf }
| _     { comment lvl lexbuf }

and func = Parser
  ""
