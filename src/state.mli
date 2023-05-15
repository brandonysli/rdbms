(* represents the state of the program*)

type t

val empty : t
val update_state : string -> string -> t
val make_owner : string -> t
val get_database : t -> string
val get_owner : t -> string
