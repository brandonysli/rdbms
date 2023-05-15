open Database

type cond = Database.condition

let get_database s = Database.database_from_file s

let reset_database d =
  let files =
    Sys.readdir (Filename.concat "data" (Database.get_database_name d))
    |> Array.to_list
    |> List.filter (fun s -> s <> "metadata.json")
  in
  List.iter
    (fun file ->
      Sys.remove
        (Filename.concat
           (Filename.concat "data" (Database.get_database_name d))
           file))
    files

let update_database d =
  reset_database d;
  Database.update d

let insert name col_list val_list d () =
  let db =
    Database.insert_into_table name col_list val_list (get_database d)
  in
  update_database db

let add_table name col_list d () =
  let db = Database.add_table name col_list (get_database d) in
  update_database db

let delete_from_table name cond has_cond d () =
  if has_cond then
    let db = Database.delete_from_table name cond (get_database d) in
    update_database db
  else
    let db = Database.delete_all_from_table name (get_database d) in
    update_database db

let drop_table name d () =
  let db = Database.remove_table name (Database.database_from_file d) in
  update_database db

let create_database name owner =
  let db = Database.make_database name owner in
  update_database db

let drop_database name = Database.remove_database name

let list_databases () =
  print_endline "Databases: \n";
  let directory_path = "data" in
  let files = Sys.readdir directory_path in
  Array.iter
    (fun file ->
      let file_path = Filename.concat directory_path file in
      if Sys.is_directory file_path then Printf.printf "%s/\n" file)
    files;
  print_endline ""

let is_database database_name =
  let directory_path = "data" in
  let full_path = Filename.concat directory_path database_name in
  try
    let file_info = Unix.stat full_path in
    file_info.Unix.st_kind = Unix.S_DIR
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> false
  | _ -> false

let rec clear_databases () =
  let dir = "data" in
  let files = Sys.readdir dir in
  Array.iter
    (fun file -> if is_database file then drop_database file)
    files

let rec select_helper tables columns d =
  match tables with
  | head :: tail ->
      select_from_table head columns (get_database d);
      select_helper tail columns d
  | [] -> ()

let select tables columns d () = select_helper tables columns d
