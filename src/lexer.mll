{
  open Parser
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id = letter (letter | digit | '_')*

rule token = parse
  | [' ' '\t' '\n' '\r']+ { token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '*' { STAR }
  | '=' { EQUALS }
  | '<' { LT }
  | '>' { GT }
  | "<=" { LE }
  | ">=" { GE }
  | "!=" { NEQ }
  | "and" | "AND" { AND }
  | "or" | "OR" { OR }
  | ";" { SC }
  | "select" | "SELECT" { SELECT }
  | "from" | "FROM" { FROM }
  | "insert" | "INSERT" { INSERT }
  | "into" | "INTO" { INTO }
  | "values" | "VALUES" { VALUES }
  | "delete" | "DELETE" { DELETE }
  | "update" | "UPDATE" { UPDATE }
  | "create" | "CREATE" { CREATE }
  | "table" | "TABLE" { TABLE }
  | "drop" | "DROP" { DROP }
  | "where" | "WHERE" { WHERE }
  | "set" | "SET" { SET }
  | "database" | "DATABASE" { DATABASE }
  | "as" | "AS" { AS }
  | "inner" | "INNER" { INNER }
  | "left" | "LEFT" { LEFT }
  | "right" | "RIGHT" { RIGHT }
  | "join" | "JOIN" { JOIN }
  | "on" | "ON" { ON }
  | "full" | "FULL" { FULL }
  | "INT" | "int" { TINT }
  | "STR" | "str" { TSTR }
  | "FLOAT" | "float" { TFLOAT }
  | digit+ as i { INT(int_of_string i) }
  | digit+ '.' digit* as f { FLOAT(float_of_string f) }
  | '"' [^'"']* '"' as s { STR(String.sub s 1 (String.length s - 2)) }
  | (id as p1) '.' (id as p2) { PAIR(p1, p2)}
  | id as identifier { ID(identifier) }
  | _ as c { failwith ("unexpected character " ^ String.make 1 c) }
