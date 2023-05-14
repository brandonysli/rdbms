open Database

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

let select columns table _ _ _ d () =
  Database.select columns table (get_database d)

let insert name col_list val_list d () =
  let db =
    Database.insert_into_table name col_list val_list (get_database d)
  in
  update_database db

let add_table name col_list d () =
  let db = Database.add_table name col_list (get_database d) in
  update_database db

let remove_table name d () =
  let db = Database.remove_table name (Database.database_from_file d) in
  update_database db

let create_database name owner =
  let db = Database.make_database name owner in
  update_database db

let drop_database name = Database.remove_database name
