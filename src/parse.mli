val parse : string -> Ast.stmt
(** [parse s] converts a string to its corresponding ast using the lexer
    and parser.
    [Example: "SELECT * FROM Brandon as b, Brandon2 as b2 INNER JOIN 
    Brandon3 as b3 ON b3.id = b2.id INNER JOIN Brandon4 as b4 ON b4.id = b2.id 
    WHERE b.id = b2.id;" -> 
    (SELECT
      ( \[ "*" \],
        \[ TBL ("Brandon", Some "b"); TBL ("Brandon2", Some "b2") \],
        Some
          \[
            JOIN
              ( INNER,
                TBL ("Brandon3", Some "b3"),
                EQ (PAIR ("b3", "id"), PAIR ("b2", "id")) );
            JOIN
              ( INNER,
                TBL ("Brandon4", Some "b4"),
                EQ (PAIR ("b4", "id"), PAIR ("b2", "id")) );
          \],
        Some (EQ (PAIR ("b", "id"), PAIR ("b2", "id"))) ));] *)

val token : Lexing.lexbuf -> Parser.token
(** [token lexbuf] converts the lexbuf into tokens and prints it to the
    console. *)

val string_of_token : Parser.token -> string
(** [string_of_token s] converts a token to a string. *)
