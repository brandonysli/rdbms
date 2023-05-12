type value 
type row 
type table 
type t = {
  db_name : string;
  db_owner : string;
  tables : table list;
}
(** will make abstract later *)

val pp_databases : t list -> unit
val pp_table : Table.t -> unit