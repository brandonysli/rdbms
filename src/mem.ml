open Database

type t = database ref

let empty = ref Database.empty
let mem = empty
let get_database s = Database.database_from_file s

let reset_database =
  let files = Sys.readdir "src/database" in
  Array.iter
    (fun file -> Sys.remove (Filename.concat "src/database" file))
    files

let update_database d =
  reset_database;
  Database.update d

let pp_mem m =
  let d = !m in
  "\nTables in " ^ d.db_name ^ " | owned by " ^ d.db_owner ^ "\nName"
  ^ "\n-----------------\n"
  ^ String.concat "\n" (List.map Database.get_name !mem.tables)

let select columns table _ _ _ () = Database.select columns table !mem

let insert name col_list val_list () =
  mem := Database.insert_into_table name col_list val_list !mem;
  update_database !mem;
  mem

let add_table name col_list () =
  mem := Database.add_table name col_list !mem;
  update_database !mem;
  mem
