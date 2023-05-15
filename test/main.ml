(*Testing Strategy: Because most functions in Mem and Database modify
  the data/ directory, when tests are run in parallel some goofy things
  happen and some errors occur. Tests are designed such that we can
  check in the data/ directory for if the files are created and modified
  in the way that the functions are intended to. Mostly black box
  testing, just to check that the databases and tables are being written
  and read to and from files as they are supposed to be. [Table] tests
  were made to be easily expandable without remaking tables for every
  test. Every test function actually tests several different inputs (3-8
  per test) while allowing OUnit to deal with raised exceptions. *)

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

let get_data_rc key_attr key_dat attr tbl =
  let open Table in
  tbl |> get_record key_attr key_dat |> get_data attr

(* Table tests are done in batches to prevent uncaught exceptions. Each
   test is actually 3-8 separate tests. *)
let test_empty_table _ =
  let open Table in
  let tbl = empty in
  assert_equal (attributes tbl) [];
  assert_equal (records tbl) [];
  assert_equal (columns tbl) 0;
  assert_equal (rows tbl) 0;
  assert_equal empty (make [])

let test_insert_attr _ =
  let open Table in
  let tbl = empty in
  let tbl' = insert_attr "attr1" tbl (String "") in
  assert_equal (columns tbl') 1;
  assert_equal (rows tbl') 0;
  assert_equal (attributes tbl') [ "attr1" ]

let test_update_attr _ =
  let open Table in
  let tbl =
    make [ ("1", Int 0); ("2", Int 0); ("3", Int 0); ("4", Int 0) ]
  in
  let tbl' = tbl |> update_attr "1" "5" in
  assert_equal (rows tbl') 0;
  assert_equal (columns tbl') 4;
  assert_equal (attributes tbl') [ "5"; "2"; "3"; "4" ]

let test_delete_attr _ =
  let open Table in
  let tbl1 = make [ ("1", Int 0) ] in
  let tbl1' = delete_attr "1" tbl1 in
  let tbl2 =
    make [ ("1", Int 0); ("2", Int 0); ("3", Int 0); ("4", Int 0) ]
  in
  let tbl2' = delete_attr "2" tbl2 in
  assert_equal (columns tbl1') 0;
  assert_equal (columns tbl2') 3;
  assert_equal (attributes tbl2') [ "1"; "3"; "4" ]

let test_insert_rec _ =
  let open Table in
  let tbl = make [ ("a", String ""); ("b", String "") ] in
  assert_equal
    (tbl |> insert_rec "a" (Int 1) |> get_data_rc "a" (Int 1) "b")
    Null;
  assert_equal (tbl |> insert_rec "a" (Bool false) |> rows) 1;
  assert_equal
    (tbl
    |> insert_full_rec [ ("a", Float 1.0); ("b", Float 0.1) ]
    |> get_data_rc "b" (Float 0.1) "a")
    (Float 1.0)

let test_update_data _ =
  let open Table in
  let tbl =
    make [ ("a", String ""); ("b", Int 0) ]
    |> insert_full_rec [ ("a", Null); ("b", Null) ]
  in
  let r = tbl |> get_record "a" Null in
  let tbl1 = tbl |> update_data r "a" (String "a1") in
  let tbl2 = tbl |> update_data r "b" (Int 1) in
  let tbl3 =
    tbl1
    |> update_data
         (tbl1 |> get_record "a" (String "a1"))
         "b" (String "b1")
  in
  assert_equal (rows tbl1) (rows tbl2);
  assert_equal (rows tbl2) (rows tbl3);
  assert_equal (columns tbl1) (columns tbl2);
  assert_equal (columns tbl2) (columns tbl3);
  assert_equal (tbl1 |> get_data_rc "a" (String "a1") "b") Null;
  assert_equal (tbl2 |> get_data_rc "b" (Int 1) "a") Null;
  assert_equal (tbl3 |> get_data_rc "a" (String "a1") "b") (String "b1")

let test_delete_rec _ =
  let open Table in
  let tbl =
    make [ ("a1", Int 0); ("a2", Int 0); ("a3", Int 0) ]
    |> insert_full_rec [ ("a1", Int 1); ("a2", Int 2); ("a3", Int 3) ]
    |> insert_full_rec
         [ ("a1", Int 11); ("a2", Int 22); ("a3", Int 33) ]
  in
  let rec1 = get_record "a1" (Int 1) tbl in
  let rec2 = get_record "a2" (Int 22) tbl in
  let e_tbl = tbl |> delete_rec rec1 |> delete_rec rec2 in
  assert_equal (e_tbl |> rows) 0;
  assert_equal (e_tbl |> columns) 3;
  assert_equal (tbl |> delete_rec rec1 |> rows) 1;
  assert_equal (tbl |> delete_rec rec2 |> records) [ rec1 ]

let pp_data_test _ =
  let open Table in
  assert_equal (pp_data (String "string")) "string";
  assert_equal (pp_data (Int 25)) (string_of_int 25);
  assert_equal (pp_data (Float 38.05)) (string_of_float 38.05);
  assert_equal (pp_data (Bool true)) (string_of_bool true);
  assert_equal (pp_data Null) "null";
  assert_bool "bool is not false"
    (Bool false |> pp_data |> bool_of_string |> not)

let test_raises _ =
  let open Table in
  let tbl =
    make [ ("a1", Int 0); ("a2", Int 0); ("a3", Int 0) ]
    |> insert_full_rec [ ("a1", Int 1); ("a2", Int 2); ("a3", Int 3) ]
    |> insert_full_rec
         [ ("a1", Int 11); ("a2", Int 22); ("a3", Int 33) ]
  in
  assert_raises (UnknownAttribute "a4") (fun () ->
      get_record "a4" (Int 1) tbl);
  assert_raises UnknownRecord (fun () -> get_record "a1" (Int 2) tbl)

let table_tests =
  Table.
    [
      "empty" >:: test_empty_table;
      "insert attribute" >:: test_insert_attr;
      "insert rec" >:: test_insert_rec;
      "update attr" >:: test_update_attr;
      "update data" >:: test_update_data;
      "delete attr" >:: test_delete_attr;
      "delete rec" >:: test_delete_rec;
      "test errors" >:: test_raises;
      "pretty print data" >:: pp_data_test;
    ]

let testing_db = Database.make_database "testing" "edward"

let json_test name file_name table : test =
  name >:: fun _ ->
  ignore (Table.write_json_to_file file_name table "testing")

let json_tests = [ json_test "json" "test" json_table ]

let suite =
  "test suite for final"
  >::: List.flatten
         [
           database_name_tests;
           database_owner_tests;
           database_add_table_tests;
           json_tests;
           table_tests;
         ]

let _ = run_test_tt_main suite
