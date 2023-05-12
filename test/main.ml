open OUnit2
open Rdatabase
open Database
open Mem
open Interpreter

let pp_db_test name databases =
  name >:: fun _ -> ignore (pp_databases databases)

let pp_table_test name databases =
  name >:: fun _ -> ignore (pp databases)

let db1 = { db_name = "db1"; db_owner = "brandon"; tables = [] }
let db2 = { db_name = "db2"; db_owner = "edward"; tables = [] }
let db3 = { db_name = "database3"; db_owner = "justin"; tables = [] }
let m = Mem.empty
let db0 = Database.add_table "cock" [ "hi"; "bye" ] !m

let print_db =
  [ pp_table_test "empty table" !m; pp_table_test "table" db0 ]

let db_tests = [ pp_db_test "db123" [ db1; db2; db3 ] ]

let suite =
  "test suite for final" >::: List.flatten [ print_db; db_tests ]

let _ = run_test_tt_main suite
