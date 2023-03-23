type value = Int of int | Float of float | String of string
type row = value list
type table = { table_name : string; rows : row list }
type database = { db_name : string; tables : table list }

let pretty_print (databases : database list) =
  let print_db_name db = Printf.printf "%s\n" db.db_name in
  Printf.printf "List of Databases\n";
  Printf.printf "=================\n";
  List.iter print_db_name databases
