(** Parse a table from JSON file. *)

type t
(** abstract type representing a json file to be parsed *)

exception FileException

val from_file : string -> t
(** [from_file str] returns the json of file [str] *)

exception ParseException

val parse : t -> Table.t
(** [parse j] parses a json type [j] into a [Table.t] *)
