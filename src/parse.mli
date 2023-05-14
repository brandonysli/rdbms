(** [parse s] converts a string to its corresponding ast using the lexer and
    parser. [Example: "SELECT * FROM “Brandon” as b WHERE b.id = “id1”;" -> 
    (SELECT ( [ "*" ], "Brandon", Some "b", Some (EQ (PAIR ("b", "id"), 
    STR "id1")), None, None))] *)
val parse : string -> Ast.stmt

(** [token lexbuf] converts the lexbuf into tokens and prints it to the console.*)
val token : Lexing.lexbuf -> Parser.token

(** [string_of_token s] converts a token to a string. *)
val string_of_token : Parser.token -> string
