open Table

type value =
  | Int of int
  | Float of float
  | String of string

type row = value list

type table = {
  table_name : string;
  rows : row list;
}

type t = {
  db_name : string;
  db_owner : string;
  tables : table list;
}

let rec db_name_list databases =
  match databases with
  | [] -> []
  | h :: t -> (h.db_name, h.db_owner) :: db_name_list t

let pp_databases databases =
  Printf.printf "\n*List of Databases*";
  Printf.printf "%-18s|%-18s\n" "\nName" "Owner";
  Printf.printf "-----------------+-----------------\n";
  List.iter
    (fun (name, owner) -> Printf.printf "%-17s|%-18s\n" name owner)
    (db_name_list databases);
  print_endline ""

let pp_table table = raise (Failure "Not implemented")

(* let pp_table table = Printf.printf "\n*List of Tables*\n"; let rec
   pp_tables_aux n acc = if n = 0 then String.sub acc 0 (String.length acc - 1)
   ^ "" else pp_tables_aux (n - 1) (acc ^ "%-18s|") in let formatting =
   pp_tables_aux (List.length (attributes table)) "\n" in Printf.printf
   formatting (List.fold_left (fun acc x -> acc ^ x ^ "|") "" (attributes
   table)) *)
