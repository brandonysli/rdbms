module I = Parser.MenhirInterpreter
open Parser

let string_of_token (token : token) =
  match token with
  | WHERE -> "where"
  | VALUES -> "values"
  | UPDATE -> "update"
  | TABLE -> "table"
  | STR s -> "\"" ^ String.escaped s ^ "\""
  | STAR -> "*"
  | SET -> "set"
  | SELECT -> "select"
  | RPAREN -> ")"
  | OR -> "or"
  | NEQ -> "!="
  | LT -> "<"
  | LPAREN -> "("
  | LE -> "<="
  | INTO -> "into"
  | INT i -> string_of_int i
  | INSERT -> "insert"
  | ID s -> String.escaped s
  | GT -> ">"
  | GE -> ">="
  | FROM -> "from"
  | FLOAT f -> string_of_float f
  | EQUALS -> "="
  | SC -> ";"
  | DROP -> "drop"
  | DELETE -> "delete"
  | DATABASE -> "database"
  | CREATE -> "create"
  | COMMA -> ","
  | AND -> "and"

let parse s =
  let lexbuf = Lexing.from_string s in
  try
    let ast = Parser.prog Lexer.token lexbuf in
    ast
  with
  | Failure s -> failwith ("Parse error: " ^ s)
  | Parser.Error ->
      failwith ("Parser error: " ^ string_of_token (Lexer.token lexbuf))
