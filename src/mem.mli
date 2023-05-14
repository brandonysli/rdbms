open Database

val select :
  string list -> string -> 'a -> 'b -> 'c -> string -> unit -> string

val insert :
  string -> string list -> value list -> string -> unit -> unit

val add_table :
  string -> (string * value) list -> string -> unit -> unit

val remove_table : string -> string -> unit -> unit
val create_database : string -> string -> unit
val drop_database : string -> unit
