open Mem
open Ast
open State

let eval (e : expr) : Database.value = failwith "unimplemented"

let interpret (s : stmt) (state : State.t) =
  match s with
  | SELECT (col_list, table_name, where, join_table, join_cond) ->
      failwith "unimplemented"
  | INSERT (table, col_list, val_list) ->
      Mem.insert table col_list
        (List.map eval val_list)
        (State.get_database state)
        ()
  | DELETE (table, cond) -> failwith "unimplemented"
  | UPDATE (table, updates, cond) -> failwith "unimplemented"
  | TCREATE (name, columns) ->
      Mem.add_table name
        (List.map (fun (s, x) -> (s, eval x)) columns)
        (State.get_database state)
        ()
  | TDROP name -> Mem.remove_table name (State.get_database state) ()
  | DCREATE name -> Mem.create_database name (State.get_owner state)
  | DDROP name -> Mem.drop_database name
