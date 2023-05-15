(*Testing Strategy: Because most functions in Mem and Database modify
  the data/ directory, when tests are run in parallel some goofy things
  happen and some errors occur. Tests are designed such that we can
  check in the data/ directory for if the files are created and modified
  in the way that the functions are intended to. Mostly black box
  testing, just to check that the databases and tables are being written
  and read to and from files as they are supposed to be*)

open OUnit2
open Rdatabase
open Database
open Mem
open Interpreter
open Table
open Parse
open Ast

let get_database_name_test name d expected_value : test =
  name >:: fun _ -> assert_equal expected_value (get_database_name d)

let get_database_owner_test name d expected_value : test =
  name >:: fun _ -> assert_equal expected_value (get_database_owner d)

let db1 = Database.make_database "db1" "edward"
let db2 = Database.make_database "db2" "brandon"
let db3 = Database.make_database "db3" "chris"
let db4 = Database.make_database "db4" "justin"

let database_name_tests =
  [
    get_database_name_test "db1" db1 "db1";
    get_database_name_test "db2" db2 "db2";
    get_database_name_test "db3" db3 "db3";
    get_database_name_test "db4" db4 "db4";
  ]

let database_owner_tests =
  [
    get_database_owner_test "edward" db1 "edward";
    get_database_owner_test "brandon" db2 "brandon";
    get_database_owner_test "chris" db3 "chris";
    get_database_owner_test "justin" db4 "justin";
  ]

let database_add_table_test name table cols d : test =
  name >:: fun _ -> ignore (Mem.add_table table cols d ())

let json_table =
  TableParse.parse (TableParse.from_file "data/table1.json")

let add_table_test name file table database : test =
  name >:: fun _ ->
  ignore (Table.write_json_to_file file table database)

let database_add_table_tests =
  [
    database_add_table_test "table1" "table1"
      [ ("attr1", Int 1); ("attr2", Bool false); ("attr3", Float 0.1) ]
      "db1";
    add_table_test "json_table" "json_table" json_table "db1";
  ]

let suite =
  "test suite for final"
  >::: List.flatten
         [
           database_name_tests;
           database_owner_tests;
           database_add_table_tests;
         ]

let _ = run_test_tt_main suite
