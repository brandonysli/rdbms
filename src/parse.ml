module I = Parser.MenhirInterpreter
open Parser

let string_of_token (token : token) =
  match token with
  | WHERE -> "WHERE"
  | VALUES -> "VALUES"
  | UPDATE -> "UPDATE"
  | TABLE -> "TABLE"
  | STR s -> "\"" ^ String.escaped s ^ "\""
  | STAR -> "*"
  | SET -> "SET"
  | SELECT -> "SELECT"
  | RPAREN -> ")"
  | OR -> "or"
  | NEQ -> "!="
  | LT -> "<"
  | LPAREN -> "("
  | LE -> "<="
  | INTO -> "INTO"
  | INT i -> string_of_int i
  | INSERT -> "INSERT"
  | ID s -> String.escaped s
  | GT -> ">"
  | GE -> ">="
  | FROM -> "FROM"
  | FLOAT f -> string_of_float f
  | EQUALS -> "="
  | SC -> ";"
  | DROP -> "DROP"
  | DELETE -> "DELETE"
  | DATABASE -> "DATABASE"
  | CREATE -> "CREATE"
  | COMMA -> ","
  | AND -> "and"
  | AS -> "as"
  | PAIR (s1, s2) -> "(" ^ s1 ^ ", " ^ s2 ^ ")"

let parse s =
  let lexbuf = Lexing.from_string s in
  try
    let ast = Parser.prog Lexer.token lexbuf in
    ast
  with
  | Failure s -> failwith ("Parse error: " ^ s)
  | Parser.Error ->
      failwith ("Parser error: " ^ string_of_token (Lexer.token lexbuf))

let token lexbuf =
  let result = Lexer.token lexbuf in
  let () = print_endline (string_of_token result) in
  result
