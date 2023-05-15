(** Module for pandas-style tables for storing abstract data. *)

type t
(** The type of tables. *)

type data =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null
      (** The type of data that can be stored in each cell of the table. *)

type record
(** The type of records, which represent an entry in the table. *)

val empty : t
(** The empty table, which has no attributes (columns) and no records
    (rows). *)

val make : (string * data) list -> t
(** [make attrs] is a table with attributes named according to [attrs]
    and no records. Example: [make \[\]] is the same as [empty]. *)

exception UnknownAttribute of string
(** Raised when an operation is performed on an attribute that is not in
    the table. *)

exception UnknownRecord of string
(** Raised when an operation is performed on a record that is not in the
    table. *)

val insert_attr : string -> t -> data -> t
(** [insert_attr attr tbl dat] adds a new attribute named [attr] to
    table [tbl], with default value [dat]. Requires: [attr] is not
    already an attribute of [tbl]. *)

val update_attr : string -> string -> t -> t
(** [update_attr old_a new_a tbl] renames the attribute [old_a] to
    [new_a] in table [tbl]. Raises: [UnknownAttribute] if [old_a] is not
    an attribute in [tbl]. *)

val delete_attr : string -> t -> t
(** [delete_attr attr tbl] removes the attribute named [attr] from table
    [tbl]. Raises: [UnknownAttribute] if [attr] is not an attribute in
    [tbl]. *)

val insert_rec : string -> data -> t -> t
(** [insert_rec attr dat tbl] adds a record with data [dat] under the
    attribute [attr] in table [tbl]. *)

val insert_full_rec : (string * data) list -> t -> t
(** [insert_full_rec attr_dat tbl] adds a complete record to [tbl]. The
    record is represented as a list [attr_dat] of (attribute, data)
    pairs. Requires: [attr_dat] is a list of the form
    [\[(attr1, dat1); (attr2, dat2); ...\]] where each [attri] is an
    attribute in [tbl]. Raises: [UnknownAttribute] if any attribute in
    [attr_dat] is not an attribute in [tbl]. *)

val update_data : record -> string -> data -> t -> t
(** [update_data r attr dat tbl] changes the data in record [r] at
    attribute [attr] to [dat] in table [tbl]. Raises: [UnknownRecord] if
    [r] is not a record in [tbl]. *)

val delete_rec : record -> t -> t
(** [delete_rec r tbl] removes the record [r] from table [tbl]. Raises:
    [UnknownRecord] if [r] is not a record in [tbl]. *)

val get_record : string -> data -> t -> record
(** [get_record attr dat tbl] gets the record that has data [dat] under
    attribute [attr] in table [tbl]. Requires: The data under each
    attribute in [tbl] is unique. *)

val get_data : string -> record -> t -> data
(** [get_data attr r tbl] gets the data from record [r] under attribute
    [attr] in table [tbl]. *)

val attributes : t -> string list
(** [attributes tbl] is a list of all attributes in table [tbl]. *)

val records : t -> record list
(** [records tbl] is a list of all records in table [tbl]. *)

val columns : t -> int
(** [columns tbl] is the number of attributes (columns) in table [tbl]. *)

val rows : t -> int
(** [rows tbl] is the number of records (rows) in table [tbl]. *)

val pp_data : data -> string
(** [pp_data dat] converts the data [dat] to a string. *)

val pp : t -> string
(** [pp tbl] converts the table [tbl] to a string. *)

val write_json_to_file : string -> t -> string -> unit
(** [write_json_to_file filename tbl dbname] writes the table [tbl] into
    a JSON file named [filename], under the database name [dbname]. *)

val read_json_file : string -> t
(** [read_json_file filename] reads a table from a JSON file named
    [filename]. *)

val delete_from_table : (record -> bool) -> t -> t
(** [delete_from_table f tbl] deletes all records from table [tbl] that
    satisfy the condition [f]. *)

val delete_all_from_table : t -> t
(** [delete_all_from_table tbl] deletes all records from table [tbl]. *)

val select_table : t -> string list -> t
(** [select_table tbl attrs] returns a new table that includes only the
    attributes listed in [attrs] from the original table [tbl]. *)
