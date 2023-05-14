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

let pp_database_test name mem =
  name >:: fun _ -> ignore (print_endline (pp_mem mem))

let pp_table_test name table =
  name >:: fun _ -> ignore (print_endline (pp table))

let pp_table_in_database name table database =
  name >:: fun _ -> ignore (pp_table table database)

let pp_database name d = name >:: fun _ -> ignore (pp_database d)
let db1 = Database.make_database "db1" "brandon"
let db2 = Database.make_database "db2" "brandon"
let db3 = Database.make_database "db3" "brandon"

let add_to_m =
  Mem.add_table "Test"
    [ ("bools", Bool false); ("ints", Int 0) ]
    "db1" ()

let db =
  let _ = Database.make_database "poopoo" "edward" in
  Mem.add_table "Test"
    [ ("bools", Bool false); ("ints", Int 0) ]
    "poopoo" ()

let db0 = Database.add_table "cock" [ ("hi", Int 1); ("bye", Int 0) ]
let tbl1 = Table.make [ ("hi", Int 1); ("bye", Int 0) ]

let tbl2 =
  Database.insert_into_table "cock" [ "hi"; "bye" ] [ Int 1; Int 2 ]
    Database.empty

let d1 =
  Mem.insert "Test" [ "bools"; "ints" ] [ Bool false; Int 12 ] "poopoo"
    ()

let rec d2 i =
  match i with
  | 0 -> ()
  | x ->
      let a = d2 (i - 1) in
      a;
      d1

let json_table =
  TableParse.parse (TableParse.from_file "data/table1.json")

let selection_table = Database.select [ "a1"; "a2"; "a5" ] ""
let print_table_test = [ pp_table_test "json" json_table ]
let select_tests = [ pp_table_test "json" ]

let print_db =
  [
    pp_database_test "empty table" "poopoo";
    pp_database_test "table" "poopoo";
    pp_database_test "table" "poopoo";
    pp_database_test "table"
      (let a = d2 5 in
       a;
       "poopoo");
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
    parse_test "SELECT * FROM Brandon;" "SELECT * FROM Brandon;"
      (SELECT ([ "*" ], "Brandon", None, None, None));
    parse_test "CREATE DATABASE Brandon;" "CREATE DATABASE Brandon;"
      (DCREATE "Brandon");
  ]

let json_test name file_name table : test =
  name >:: fun _ ->
  ignore (Table.write_json_to_file file_name table "testing")

let json_tests = [ json_test "json" "test" json_table ]

let suite =
  "test suite for final"
  >::: List.flatten
         [
           json_tests;
           print_table_test;
           print_db;
           print_tbl;
           db_tests;
           parse_tests;
         ]

let _ = run_test_tt_main suite
