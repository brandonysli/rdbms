(** Represents the state of the program. *)

type t

val empty : t
(** The initial state. *)

val update_state : string -> string -> t
(** [update_state s1 s2] updates the state using [s1] and [s2] and
    returns the new state. *)

val make_owner : string -> t
(** [make_owner s] creates a new owner [s] and returns the updated
    state. *)

val get_database : t -> string
(** [get_database t] gets the database from the state [t]. *)

val get_owner : t -> string
(** [get_owner t] gets the owner from the state [t]. *)
