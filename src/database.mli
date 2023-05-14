type value =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Null

type condition =
  | And of condition * condition
  | Or of condition * condition
  | GE of string * value
  | LE of string * value
  | GT of string * value
  | LT of string * value
  | EQ of string * value
  | NE of string * value
  | None

type row = value list
type table

type database
(** will make abstract later *)

val get_database_name : database -> string
val get_database_owner : database -> string
val empty : database
val make_database : string -> string -> database
val get_table : string -> database -> table option
val get_name : table -> string
val add_table : string -> (string * value) list -> database -> database
val remove_table : string -> database -> database

val insert_into_table :
  string -> string list -> value list -> database -> database

val pp_table : string -> database -> string
val pp_database : database -> unit
val pp_databases : database list -> unit
val update : database -> unit
val database_from_file : string -> database
val remove_database : string -> unit
val delete_from_table : string -> condition -> database -> database
val delete_all_from_table : string -> database -> database
val select_from_table : string -> string list -> database -> unit
