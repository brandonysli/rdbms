open Mem
open Ast

let eval (e : expr) : Database.value = failwith "unimplemented"

let interpret (s : stmt) =
  match s with
  | SELECT (col_list, table_name, where, join_table, join_cond) ->
      failwith "unimplemented"
  | INSERT (table, col_list, val_list) ->
      Mem.insert table col_list (List.map eval val_list)
  | DELETE (table, cond) -> failwith "unimplemented"
  | UPDATE (table, updates, cond) -> failwith "unimplemented"
  | TCREATE (name, columns) ->
      Mem.add_table name (List.map (fun (s, x) -> (s, eval x)) columns)
  | TDROP name -> failwith "unimplemented"
  | DCREATE name -> failwith "unimplemented"
  | DDROP name -> failwith "unimplemented"
