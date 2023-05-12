(** Pandas style table for storing abstract data. *)

type data =
  | String of string
  | Int of int
  | Float of float
      (** Stores the data for each record-attribute pair. *)

type record
(** Stores data about a given entry in the table. *)

type t
(** Abstract type representing a table. *)

val empty : unit -> t
(** [empty ()] is a table with no attributes (columns) nor records
    (rows). *)

val make : string list -> t
(** [make attrs] is a table of attributes with names [attrs] and no
    records. Example: [make \[\]] = [empty] *)

exception UnknownAttribute of string
(** Raised when an attribute of the given name is not in the table. *)

exception UnknownRecord of string
(** Raised when a record with the given value is not in the attribute. *)

val insert_attr : t -> string -> t
(** [insert_attr tbl attr] adds a new attribute to table [tbl].
    Requires: [attr] is not already an attribute of [tbl]. *)

val update_attr : t -> string -> string -> t
(** [update_attr tbl old_a new_a] changes the name of attribute [old_a]
    to [new_a]. Raises: [UnknownAttribute] if [old_a] is not an
    attribute in [tbl]. *)

val delete_attr : t -> string -> t
(** [delete_attr tbl attr] removes the attribute with name [attr] from
    [tbl]. Raises: [UnknownAttribute] if [attr] is not an attribute in
    [tbl]. *)

val insert_rec : t -> string -> data -> t
(** [insert_rec tbl attr dat] adds a record with data [dat] for given
    attribute [attr]. *)

val update_data : t -> record -> string -> data -> t
(** [update_rec tbl r attr dat] changes the value of the data in [r] at
    [attr] to [dat]. Raises: [UnkownRecord] if [r] is not a record in
    [tbl]. *)

val delete_rec : t -> record -> t
(** [delete_rec tbl r] removes the record [r] in [tbl]. Raises:
    [UnknownRecord] if [r] is not a record in [tbl]. *)

val get_record : t -> string -> data -> record
(** [get_record tbl attr dat] gets the record that has data [dat] for
    attribute [attr]. Requires: All data for attribute [attr] in [tbl]
    is unique. *)

val get_data : t -> record -> string -> data option
(** [get_data tbl r attr] gets the data from the record [r] with
    attribute [attr]. Note: May be None. *)

val attributes : t -> string list
(** [attributes tbl] is a list of every attribute in [tbl]. *)

val records : t -> record list
(** [records tbl] is a list of every record in [tbl]. *)

val columns : t -> int
(** [columns tbl] is the number of attributes (columns) in table [tbl]. *)

val rows : t -> int
(** [rows tbl] is the number of records (rows) the table [tbl]. *)
