open Database

type cond = Database.condition

val select :
  string list -> string -> 'a -> 'b -> 'c -> string -> unit -> string

val insert :
  string -> string list -> value list -> string -> unit -> unit

val add_table :
  string -> (string * value) list -> string -> unit -> unit

val drop_table : string -> string -> unit -> unit
val create_database : string -> string -> unit
val drop_database : string -> unit
val list_databases : unit -> unit
val is_database : string -> bool
val delete_from_table : string -> cond -> bool -> string -> unit -> unit
