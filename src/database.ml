open Table
open Array
open Yojson
open Yojson.Safe
open Yojson.Basic.Util
open Unix

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

let empty = { db_name = "Empty"; db_owner = "nobody"; tables = [] }

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

let rec get_table_helper name table_list =
  match table_list with
  | [] -> None
  | head :: tail ->
      if head.table_name = name then Some head
      else get_table_helper name tail

let update_table_json table d directory =
  Sys.remove (Filename.concat directory (table.table_name ^ ".json"));
  Table.write_json_to_file table.table_name table.attr d.db_name

let get_table name d = get_table_helper name d.tables

let rec update_tables name f tables =
  match tables with
  | ({ table_name = n; attr } as head) :: tail -> (
      match n = name with
      | true -> f attr :: tail
      | false -> head :: update_tables name f tail)
  | [] -> []

let rec to_pair_helper cols vals =
  match (cols, vals) with
  | h1 :: t1, h2 :: t2 -> ("" ^ h1, to_data h2) :: to_pair_helper t1 t2
  | _ -> []

let insert_into_table name cols vals d =
  {
    db_name = d.db_name;
    db_owner = d.db_owner;
    tables =
      update_tables name
        (fun t ->
          let t =
            {
              table_name = name;
              attr =
                Table.insert_full_rec
                  (List.sort_uniq
                     (fun (x, y) (x2, y2) -> compare x2 x)
                     (to_pair_helper cols vals))
                  t;
            }
          in
          update_table_json t d (Filename.concat "data" d.db_name);
          t)
        d.tables;
  }

let remove_table name d =
  {
    db_name = d.db_name;
    db_owner = d.db_owner;
    tables = List.filter (fun t -> t.table_name <> name) d.tables;
  }

let rec db_name_list databases =
  match databases with
  | [] -> []
  | h :: t -> (h.db_name, h.db_owner) :: db_name_list t

let pp_table name d =
  print_endline ("Table: " ^ name);
  match get_table name d with
  | Some t -> print_endline (Table.pp t.attr)
  | None -> ()

let pp_database d =
  Printf.printf "\n*List of Tables*";
  Printf.printf "%18s" "\nName";
  Printf.printf "\n-----------------\n";
  List.iter (fun t -> Printf.printf "%-17s\n" t.table_name) d.tables

let pp_databases databases =
  Printf.printf "\n*List of Databases*";
  Printf.printf "%-18s|%-18s\n" "\nName" "Owner";
  Printf.printf "-----------------+-----------------\n";
  List.iter
    (fun (name, owner) -> Printf.printf "%-17s|%-18s\n" name owner)
    (db_name_list databases);
  print_endline ""

let pp_table table d =
  match get_table table d with
  | Some t -> Table.pp t.attr
  | None -> "Table does not Exist"

(* let pp_table table = Printf.printf "\n*List of Tables*\n"; let rec
   pp_tables_aux n acc = if n = 0 then String.sub acc 0 (String.length
   acc - 1) ^ "" else pp_tables_aux (n - 1) (acc ^ "%-18s|") in let
   formatting = pp_tables_aux (List.length (attributes table)) "\n" in
   Printf.printf formatting (List.fold_left (fun acc x -> acc ^ x ^ "|")
   "" (attributes table)) *)

let rec select_helper table cols =
  match cols with
  | [] -> table
  | head :: tail -> Table.delete_attr head (select_helper table cols)

let select cols table d =
  Table.pp
    (select_helper
       (match get_table table d with
       | Some table -> table.attr
       | None -> Table.empty)
       cols)

(* Function to parse a database into a directory of JSON files *)
let rec update_helper tables d =
  match tables with
  | head :: tail ->
      Table.write_json_to_file head.table_name head.attr d.db_name;
      update_helper tail d
  | [] -> ()

let update d = update_helper d.tables d

(* Function to parse a directory of JSON files *)
let table_from_file dirname s =
  {
    table_name = s;
    attr =
      s ^ ".json" |> Filename.concat dirname |> Table.read_json_file;
  }

let parse_directory dirname =
  let files = Sys.readdir dirname in
  let tables =
    List.map
      (fun s -> s |> Filename.chop_extension |> table_from_file dirname)
      (List.filter
         (fun s -> s <> "metadata.json")
         (Array.to_list files))
  in
  tables

let data_of_json json =
  match json with `String s -> s | _ -> failwith "what"

let json_to_database json =
  let name_json = member "name" json in
  let owner_json = member "owner" json in
  (data_of_json name_json, data_of_json owner_json)

let database_from_file name =
  let name, owner =
    json_to_database
      (Yojson.Basic.from_file
         (Filename.concat "data" (Filename.concat name "metadata.json")))
  in
  {
    db_name = name;
    db_owner = owner;
    tables = parse_directory (Filename.concat "data" name);
  }

let value_to_json (v : value) : Yojson.Safe.t =
  match v with
  | Int i -> `Int i
  | Float f -> `Float f
  | String s -> `String s
  | Bool b -> `Bool b
  | Null -> `Null

let d_to_json d =
  `Assoc
    [
      ("name", value_to_json (String d.db_name));
      ("owner", value_to_json (String d.db_owner));
    ]

let write_database_to_file d =
  let json = d_to_json d in
  let channel = open_out ("data/" ^ d.db_name ^ "/metadata.json") in
  output_string channel (pretty_to_string json);
  close_out channel

let perms = 0o755

(* directory permissions *)

let make_database_dir d =
  try
    mkdir ("data/" ^ d.db_name) perms;
    write_database_to_file d
  with Unix_error (e, _, _) ->
    write_database_to_file d;
    prerr_endline ("Failed to create directory: " ^ Unix.error_message e)

let make_database name owner =
  let db = { db_name = name; db_owner = owner; tables = [] } in
  make_database_dir db;
  db

let remove_database name =
  Sys.command ("rm -r " ^ Filename.concat "data" name) |> ignore
