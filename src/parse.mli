val parse : string -> Ast.stmt
(** [parse s] converts a string to its corresponding ast using the lexer
    and parser. Example: "SELECT * FROM "Brandon" as b WHERE b.id =
    "id1";" -> SELECT ([ "*"], "Brandon", Some "b", Some (EQ (PAIR ("b",
    "id"), STR "id1")), None, None) *)

val token : Lexing.lexbuf -> Parser.token
(** [token lexbuf] converts the lexbuf into tokens and prints it to the
    console. *)

val string_of_token : Parser.token -> string
(** [string_of_token s] converts a token to a string. *)
