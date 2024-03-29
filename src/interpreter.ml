open Mem
open Ast
open State

let eval (e : expr) : Database.value =
  match e with
  | STR s -> String s
  | INT i -> Int i
  | FLOAT f -> Float f
  | _ -> Null

let rec eval_cond (c : cond) : Database.condition =
  match c with
  | AND (c1, c2) -> And (eval_cond c1, eval_cond c2)
  | OR (c1, c2) -> Or (eval_cond c1, eval_cond c2)
  | GE (s, d) -> (
      match s with STR str -> GE (str, eval d) | _ -> failwith "?")
  | LE (s, d) -> (
      match s with STR str -> LE (str, eval d) | _ -> failwith "?")
  | GT (s, d) -> (
      match s with STR str -> GT (str, eval d) | _ -> failwith "?")
  | LT (s, d) -> (
      match s with STR str -> LT (str, eval d) | _ -> failwith "?")
  | EQ (s, d) -> (
      match s with STR str -> EQ (str, eval d) | _ -> failwith "?")
  | NEQ (s, d) -> (
      match s with STR str -> NE (str, eval d) | _ -> failwith "?")

let table_to_string table = match table with TBL (s, e) -> s

let interpret (s : stmt) (state : State.t) : State.t =
  match s with
  | SELECT (col_list, table, join_table, join_cond) ->
      Mem.select
        (List.map table_to_string table)
        col_list
        (State.get_database state)
        ();
      state
  | INSERT (table, col_list, val_list) ->
      Mem.insert table col_list
        (List.map eval val_list)
        (State.get_database state)
        ();
      state
  | DELETE (table, cond) -> (
      match cond with
      | Some c ->
          Mem.delete_from_table table (eval_cond c) true
            (State.get_database state)
            ();
          state
      | None ->
          Mem.delete_from_table table None false
            (State.get_database state)
            ();
          state)
  | UPDATE (table, updates, cond) -> failwith "unimplemented"
  | TCREATE (name, columns) ->
      Mem.add_table name
        (List.map
           (fun (s, x) ->
             let x' =
               match x with
               | TSTR -> Database.String ""
               | TINT -> Int 0
               | TFLOAT -> Float 0.0
               | TBOOL -> Bool false
             in
             (s, x'))
           columns)
        (State.get_database state)
        ();
      state
  | TDROP name ->
      Mem.drop_table name (State.get_database state) ();
      state
  | DCREATE name ->
      Mem.create_database name (State.get_owner state);
      State.update_state name (State.get_owner state)
  | DDROP name ->
      Mem.drop_database name;
      State.update_state "" (State.get_owner state)
