open Database

type t
(** Scuffed storage of a singular database *)

val empty : t
val mem : t
val get_database : t -> database
val insert : string -> string list -> value list -> t -> unit -> unit
val add_table : string -> (string * value) list -> t -> unit -> unit
val pp_mem : string
