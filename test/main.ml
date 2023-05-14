open OUnit2
open Rdatabase
open Database
open Mem
open Interpreter
open Table
open Parse
open Ast

let pp_db_test name databases =
  name >:: fun _ -> ignore (pp_databases databases)

let pp_database_test name =
  name >:: fun _ -> ignore (print_endline pp_mem)

let pp_table_test name table =
  name >:: fun _ -> ignore (print_endline (pp table))

let pp_table_in_database name table database =
  name >:: fun _ -> ignore (pp_table table database)

let pp_database name d = name >:: fun _ -> ignore (pp_database d)
let db1 = { db_name = "db1"; db_owner = "brandon"; tables = [] }
let db2 = { db_name = "db2"; db_owner = "edward"; tables = [] }
let db3 = { db_name = "database3"; db_owner = "justin"; tables = [] }
let m = Mem.empty

let add_to_m =
  Mem.add_table "Test" [ ("bools", Bool false); ("ints", Int 0) ] m

let db0 =
  Database.add_table "cock"
    [ ("hi", Int 1); ("bye", Int 0) ]
    Database.empty

let tbl1 = Table.make [ ("hi", Int 1); ("bye", Int 0) ]

let tbl2 =
  Database.insert_into_table "cock" [ "hi"; "bye" ] [ Int 1; Int 2 ] db0

let d =
  Mem.add_table "Test" [ ("bools", Bool false); ("ints", Int 0) ] m ()

let print_db =
  [
    pp_database_test "empty table";
    (let _ = d in
     pp_database_test "table");
  ]

let print_again = [ pp_database_test "poo" ]

let print_tbl =
  [
    pp_table_test "empty" Table.empty;
    pp_table_test "table" tbl1;
    pp_table_in_database "insert" "cock" tbl2;
  ]

let db_tests = [ pp_db_test "db123" [ db1; db2; db3 ] ]

let parse_test name str expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Parse.parse str)

let parse_tests =
  [
    parse_test "Standard select all query: SELECT * FROM Brandon;"
      "SELECT * FROM Brandon;"
      (SELECT ([ "*" ], "Brandon", None, None, None));
    parse_test "Select two columns from table: SELECT a,b FROM Brandon;"
      "SELECT a,b FROM Brandon;"
      (SELECT ([ "b"; "a" ], "Brandon", None, None, None));
    parse_test "Lowercase select and from: select a,b from Brandon;"
      "select a,b from Brandon;"
      (SELECT ([ "b"; "a" ], "Brandon", None, None, None));
    parse_test
      "Select with where clause and GT: SELECT a,b FROM Brandon WHERE a > 5;"
      "SELECT a,b FROM Brandon WHERE a > 5;"
      (SELECT ([ "b"; "a" ], "Brandon", Some (GT (STR "a", INT 5)), None, None));
    parse_test "CREATE DATABASE Brandon;" "CREATE DATABASE Brandon;"
      (DCREATE "Brandon");
  ]

let suite =
  "test suite for final"
  >::: List.flatten [ print_db; print_tbl; db_tests; parse_tests ]

let _ = run_test_tt_main suite
