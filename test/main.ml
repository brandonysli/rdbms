open OUnit2
open Rdatabase
open Database
open Mem
open Interpreter
open Table

let pp_db_test name databases =
  name >:: fun _ -> ignore (pp_databases databases)

let pp_database_test name databases =
  name >:: fun _ -> ignore (pp_database databases)

let pp_table_test name table =
  name >:: fun _ -> ignore (print_endline (pp table))

let db1 = { db_name = "db1"; db_owner = "brandon"; tables = [] }
let db2 = { db_name = "db2"; db_owner = "edward"; tables = [] }
let db3 = { db_name = "database3"; db_owner = "justin"; tables = [] }
let m = Mem.empty
let db0 = Database.add_table "cock" [ ("hi", Int 1); ("bye", Int 0) ] !m
let tbl1 = Table.make [ ("hi", Int 1); ("bye", Int 0) ]
let data2 = Database.insert_into_table "cock" "hi" (Int 1) db0

let print_db =
  [
    pp_database_test "empty table" !m;
    pp_database_test "table" db0;
    pp_database_test "insert" data2;
  ]

let print_tbl =
  [ pp_table_test "empty" Table.empty; pp_table_test "table" tbl1 ]

let db_tests = [ pp_db_test "db123" [ db1; db2; db3 ] ]

let suite =
  "test suite for final"
  >::: List.flatten [ print_db (*print_tbl; db_tests*) ]

let _ = run_test_tt_main suite
