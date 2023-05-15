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
(** [get_database_name d] returns the name of database [d]*)

val get_database_owner : database -> string
(** [get_database_owner d] returns the owner of database [d]*)

val empty : database
(** [empty] is the empty database*)

val make_database : string -> string -> database
(** [make_database name owner] makes a new database with name [name] and
    owner [owner]*)

val add_table : string -> (string * value) list -> database -> database
(** [add_table name cols d] adds a new table [name] with attributes
    [cols] in database [d]*)

val remove_table : string -> database -> database
(** [remove_table name d] removes table [name] in database [d]*)

val insert_into_table :
  string -> string list -> value list -> database -> database
(** [insert_into_table name cols vals d] inserts [vals] into their
    respective attributes [cols] in table [name] in database [d]*)

val pp_table : string -> database -> string
(** [pp_table name d] pretty prints table [name] in database [d]*)

val pp_database : database -> unit
(** [pp_database d] pretty prints database [d]*)

val pp_databases : database list -> unit
(** [pp_databases databases] pretty prints databases [d]*)

val update : database -> unit
(** [update d] rewrites a database [d] into the json files*)

val database_from_file : string -> database
(** [database_from_file name] returns the database [name]*)

val remove_database : string -> unit
(** [remove_database name] removes the database name*)

val delete_from_table : string -> condition -> database -> database
(** [delete_from_table name cond d] deletes elements from table [name]
    in database [d] that satisfy condition [cond]*)

val delete_all_from_table : string -> database -> database
(** [delete_all_from_table name d] deletes all elements in table [name]
    in database [d], but keeps the table*)

val select_from_table : string -> string list -> database -> unit
(** [select_from_table name cols d] selects columns cols in table name
    in database d*)
