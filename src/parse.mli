(** [parse s] converts a string to its corresponding ast using a the lexer and
    parser. *)
val parse : string -> Ast.stmt