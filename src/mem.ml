open Database

type t = database ref

let empty = ref Database.empty
let mem = empty
let get_database t = !t

let pp_mem =
  let d = !mem in
  "\nTables in " ^ d.db_name ^ " | owned by " ^ d.db_owner ^ "\nName"
  ^ "\n-----------------\n"
  ^ String.concat "%-17s\n" (List.map Database.get_name !mem.tables)

let insert name col_list val_list m () =
  m := Database.insert_into_table name col_list val_list !m

let add_table name col_list m () =
  m := Database.add_table name col_list !m
