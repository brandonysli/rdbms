(** Pandas style table for storing abstract data. *)

type t
(** Abstract type representing a table. *)

type data =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null  (** Stores the data for each record-attribute pair. *)

type record
(** Stores data about a given entry in the table. *)

val empty : t
(** [empty] is a table with no attributes (columns) nor records (rows). *)

val make : (string * data) list -> t
(** [make attrs] is a table of attributes with names [attrs] and no
    records. Example: [make \[\]] = [empty] *)

exception UnknownAttribute of string
(** Raised when an attribute of the given name is not in the table. *)

exception UnknownRecord of string
(** Raised when a record with the given value is not in the attribute. *)

val insert_attr : string -> t -> data -> t
(** [insert_attr attr tbl] adds a new attribute to table [tbl].
    Requires: [attr] is not already an attribute of [tbl]. *)

val update_attr : string -> string -> t -> t
(** [update_attr old_a new_a tbl] changes the name of attribute [old_a]
    to [new_a]. Raises: [UnknownAttribute] if [old_a] is not an
    attribute in [tbl]. *)

val delete_attr : string -> t -> t
(** [delete_attr attr tbl] removes the attribute with name [attr] from
    [tbl]. Raises: [UnknownAttribute] if [attr] is not an attribute in
    [tbl]. *)

val insert_rec : string -> data -> t -> t
(** [insert_rec attr dat tbl] adds a record with data [dat] for given
    attribute [attr]. *)

val insert_full_rec : (string * data) list -> t -> t
(** [insert_full_rec attr_dat tbl] adds a record with data binded to a
    given attribute in assocation list [attr_dat]. Requires: [attr_dat]
    is an association list of the form
    [\[(attr1, dat1), (attr2, dat2)...\]] where [attri] is an element of
    [attributes tbl]. Raises: [UnknownAttribute] if any [attri] is not
    in attributes. *)

val update_data : record -> string -> data -> t -> t
(** [update_rec r attr dat tbl] changes the value of the data in [r] at
    [attr] to [dat]. Raises: [UnkownRecord] if [r] is not a record in
    [tbl]. *)

val delete_rec : record -> t -> t
(** [delete_rec r tbl] removes the record [r] in [tbl]. Raises:
    [UnknownRecord] if [r] is not a record in [tbl]. *)

val get_record : string -> data -> t -> record
(** [get_record attr dat tbl] gets the record that has data [dat] for
    attribute [attr]. Requires: All data for attribute [attr] in [tbl]
    is unique. *)

val get_data : string -> record -> data
(** [get_data attr r tbl] gets the data from the record [r] with
    attribute [attr]. Note: May be None. *)

val attributes : t -> string list
(** [attributes tbl] is a list of every attribute in [tbl]. *)

val records : t -> record list
(** [records tbl] is a list of every record in [tbl]. *)

val columns : t -> int
(** [columns tbl] is the number of attributes (columns) in table [tbl]. *)

val rows : t -> int
(** [rows tbl] is the number of records (rows) the table [tbl]. *)

val pp_data : data -> string
(** [pp_data data] returns the data as a string. *)

val pp : t -> string
(** [pp tbl] return the table as a string. *)

val write_json_to_file : string -> t -> string -> unit
(** [write_json_to_file filename t database_name] writes a table into a
    json file*)

val read_json_file : string -> t
(** [read_json_file filename] reads a json file into a table*)

val delete_from_table : (record -> bool) -> t -> t
(** [read_json_file table f] deletes records stored in table that
    satisfy condition f*)
