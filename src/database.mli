type value = Int of int | Float of float | String of string

type row = value list

type table = {
  table_name: string;
  rows: row list;
}

type database = {
  db_name: string;
  tables: table list;
}

val pretty_print : database list -> unit