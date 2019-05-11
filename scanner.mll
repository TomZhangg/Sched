{ open Parser }

let digits = ['0'-'9']
let year = (digits)digits(digits)digits
let month = (digits)digits
let day = (digits)digits
let hour = (digits)digits
let minute = (digits)digits
let second = (digits)digits

rule token = parse
  [' ' '\r' '\n' '\t'] { token lexbuf } (* Whitespace *)
| "#("          { comment 1 lexbuf }      (* Comments *)
| '('           { LPAREN }
| ')'           { RPAREN }
| '{'           { LBRACE }
| '}'           { RBRACE }
| ';'           { SEMI }
| ':'           { COL }
| ','           { COMMA }
| '='           { ASSIGN }
| "=="          { EQ }
| "!="          { NEQ }
| '<'           { LT }
| "<="          { LEQ }
| ">"           { GT }
| ">="          { GEQ }
| "&&"          { AND }
| "||"          { OR }
| "!"           { NOT }
| "+"			{ PLUS }
| "-"			{ MINUS }
| "*"			{ TIMES }
| "/"			{ DIVIDE }
| "%"			{ MOD }
| "if"          { IF }
| "else"        { ELSE }
| "Create"      { CREATE }
| "Insert"      { INSERT }
| "Drop"		{ DROP }
| "Copy"		{ COPY }
| "Schedule"    { SCHED }
| "Item"        { ITEM }
| "Items"       { ITEMS }
| "Collection"  { COLLECTION }
| "Into"        { INTO }
| "From"		{ FROM }
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
| "int"			{ INT }
| "str"         { STRING }
| "float"       {FLOAT}
| "True"        { BLIT(true)  }
| "False"       { BLIT(false) }
| "return"			{ RETURN }
| digits+ as lxm { ILIT(int_of_string lxm) }
| digits+ '.' as lxm { FLIT(lxm^"0") }
| '.' digits+ as lxm { FLIT("0"^lxm) }
| ((digits+ '.' digits+) | (digits '.'  digits* ( ['e' 'E'] ['+' '-']? digits )?) ) as lxm { FLIT(lxm) }
| '"'([^'"']*)'"' as s { SLIT(s) }
| "func"        { FUNC }
| '<' year '-' month '-' day '>' as lit  { DATELIT(lit) }
| '<' year '-' month '-' day 'T' hour ':' minute ':' second '>' as lit  { TIMELIT(lit) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof           { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment lvl = parse
  ")#"  { if lvl = 1 then token lexbuf else comment (lvl - 1) lexbuf }
| "#("  { comment (lvl + 1) lexbuf }
| _     { comment lvl lexbuf }
