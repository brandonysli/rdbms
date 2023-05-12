open Table

type value =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Null

type row = value list

type table = {
  table_name : string;
  attr : Table.t;
}

type database = {
  db_name : string;
  db_owner : string;
  tables : table list;
}

let to_data (v : value) : Table.data =
  match v with
  | Int i -> Int i
  | Float f -> Float f
  | String s -> String s
  | Bool b -> Bool b
  | Null -> Null

let get_name t = t.table_name

let add_table name cols d =
  {
    db_name = d.db_name;
    db_owner = d.db_owner;
    tables =
      {
        table_name = name;
        attr = Table.make (List.map (fun (x, y) -> (x, to_data y)) cols);
      }
      :: d.tables;
  }

let rec is_table name table = table.table_name = name

let rec update_tables name f tables =
  match tables with
  | ({ table_name = n; attr } as head) :: tail -> (
      match n = name with
      | true -> f attr :: tail
      | false -> head :: update_tables name f tail)
  | [] -> []

let insert_into_table name cols vals d =
  {
    db_name = d.db_name;
    db_owner = d.db_owner;
    tables =
      update_tables name
        (fun t ->
          {
            table_name = name;
            attr = Table.insert_rec cols (to_data vals) t;
          })
        d.tables;
  }

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
