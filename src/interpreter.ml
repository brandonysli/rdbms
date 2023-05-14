open Mem
open Ast
open State

let eval (e : expr) : Database.value =
  match e with
  | STR s -> String s
  | INT i -> Int i
  | FLOAT f -> Float f
  | _ -> Null

let rec eval_cond (c : cond) =
  match c with
  | EQ (e1, e2) -> eval e1 = eval e2
  | NEQ (e1, e2) -> eval e1 <> eval e2
  | LT (e1, e2) -> (
      match (e1, e2) with
      | INT i1, INT i2 -> i1 < i2
      | FLOAT i1, FLOAT i2 -> i1 < i2
      | _ -> failwith "How did we get here")
  | GT (e1, e2) -> (
      match (e1, e2) with
      | INT i1, INT i2 -> i1 > i2
      | FLOAT i1, FLOAT i2 -> i1 > i2
      | _ -> failwith "How did we get here")
  | LE (e1, e2) -> (
      match (e1, e2) with
      | INT i1, INT i2 -> i1 <= i2
      | FLOAT i1, FLOAT i2 -> i1 <= i2
      | _ -> failwith "How did we get here")
  | GE (e1, e2) -> (
      match (e1, e2) with
      | INT i1, INT i2 -> i1 >= i2
      | FLOAT i1, FLOAT i2 -> i1 >= i2
      | _ -> failwith "How did we get here")
  | AND (c1, c2) -> eval_cond c1 && eval_cond c2
  | OR (c1, c2) -> eval_cond c1 || eval_cond c2

let interpret (s : stmt) (state : State.t) =
  match s with
  | SELECT (col_list, table_name, alias, where, join_table, join_cond)
    ->
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
