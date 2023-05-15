(** Parse a table from JSON file. *)

type t

exception FileException

val from_file : string -> t

exception ParseException

val parse : t -> Table.t
