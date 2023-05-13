open Database

type t
(** Scuffed storage of a singular database *)

val empty : t
val mem : t
val get_database : string -> database
val select : string list -> string -> 'a -> 'b -> 'c -> unit -> string
val insert : string -> string list -> value list -> unit -> t
val add_table : string -> (string * value) list -> unit -> t
val pp_mem : t -> string
