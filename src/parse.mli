(** [parse s] converts a string to its corresponding ast using a the lexer and
    parser. *)
val parse : string -> Ast.stmt

(** [token lexbuf] converts the lexbuf into tokens and prints it to the console.*)
val token : Lexing.lexbuf -> Parser.token

(** [string_of_token s] converts a token to a string. *)
val string_of_token : Parser.token -> string
