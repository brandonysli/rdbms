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

let database_add_table_test name table cols d expected_value : test =
  name >:: fun _ -> assert_equal expected_value (add_table table cols d)

let database_owner_tests =
  [
    database_add_table_test "edward" "table1"
      [ ("attr1", Int 1); ("attr2", Bool false); ("attr3", Float 0.1) ];
    database_add_table_test "brandon" db2 "brandon";
    database_add_table_test "chris" db3 "chris";
    database_add_table_test "justin" db4 "justin";
  ]

let suite = "test suite for final" >::: List.flatten []
let _ = run_test_tt_main suite
