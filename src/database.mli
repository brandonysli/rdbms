type value =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Null

type row = value list
type table

type database = {
  db_name : string;
  db_owner : string;
  tables : table list;
}
(** will make abstract later *)

val empty : database
val get_table : string -> database -> table option
val get_name : table -> string
val add_table : string -> (string * value) list -> database -> database

val insert_into_table :
  string -> string list -> value list -> database -> database

val pp_table : string -> database -> unit
val pp_database : database -> unit
val pp_databases : database list -> unit
