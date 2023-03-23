type value = Int of int | Float of float | String of string
type row = value list
type table = { table_name : string; rows : row list }
type database = { db_name : string; db_owner : string; tables : table list }

let rec db_name_list databases =
  match databases with
  | [] -> []
  | h :: t -> (h.db_name, h.db_owner) :: db_name_list t

let pp_databases databases =
  Printf.printf "\n*List of Databases*";
  Printf.printf "%-18s| %-18s\n" "\nName" "Owner";
  Printf.printf "-----------------+-----------------\n";
  List.iter
    (fun (name, owner) -> Printf.printf "%-17s| %-18s\n" name owner)
    (db_name_list databases)
