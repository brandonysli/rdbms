(** Represents the state of the program. *)

type t
(** Abstract type representing the current state (database and user)*)

val empty : t
(** The initial state. *)

val update_state : string -> string -> t
(** [update_state database user] returns the new state with database
    [database] and user [user] *)

val make_owner : string -> t
(** [make_owner owner] returns a new state with owner [owner]*)

val get_database : t -> string
(** [get_database state] extracts the database of state [state]. *)

val get_owner : t -> string
(** [get_owner owner] extracts the owner of state [state]. *)
