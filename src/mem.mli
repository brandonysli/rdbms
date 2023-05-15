open Database

type cond = Database.condition

val insert :
  string -> string list -> value list -> string -> unit -> unit
(**[insert name col_list val_list d] inserts [val_list] into [col_list]
   in table [name] in database [d]*)

val add_table :
  string -> (string * value) list -> string -> unit -> unit
(**[add_table  name col_list d] adds a table [name] to database [d] with
   attributes [col_list]*)

val drop_table : string -> string -> unit -> unit
(**[drop_table name d ()] drops table [name] in database [d]*)

val create_database : string -> string -> unit
(**[create_database name owner] creates a database with name [name] and
   owner [owner]*)

val drop_database : string -> unit
(**[drop_database name] drops datbase [name]*)

val list_databases : unit -> unit
(**[list_databases ()] pretty prints the databases*)

val is_database : string -> bool
(**[is_database s] returns true of [s] is a stored database*)

val delete_from_table : string -> cond -> bool -> string -> unit -> unit
(**[delete_from_table name cond has_cond d] deletes records from table
   [name] that satisfy condition [cond] if [has_cond]. Otherwise, all
   reords in table [name] are deleted*)

val clear_databases : unit -> unit
(**[clear_databases ()] clears all databases*)

val select : string list -> string list -> string -> unit -> unit
(**[select tables columns d ()] selects columns [columns] from tables
   [tables] in database [d]*)

val print_tables : string -> unit -> unit
(**[print_tables d] pretty prints tables in database [d]*)
