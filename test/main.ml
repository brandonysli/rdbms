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
  let _ = Database.make_database "database" "edward" in
  Mem.add_table "Test"
    [ ("bools", Bool false); ("ints", Int 0) ]
    "database" ()

let db0 = Database.add_table "cock" [ ("hi", Int 1); ("bye", Int 0) ]
let tbl1 = Table.make [ ("hi", Int 1); ("bye", Int 0) ]

let tbl2 =
  Database.insert_into_table "cock" [ "hi"; "bye" ] [ Int 1; Int 2 ]
    Database.empty

let d1 =
  Mem.insert "Test" [ "bools"; "ints" ] [ Bool false; Int 12 ]
    "database" ()

let rec d2 i =
  match i with
  | 0 -> ()
  | x ->
      let a = d2 (i - 1) in
      a;
      d1

let json_table =
  TableParse.parse (TableParse.from_file "data/table1.json")

let print_table_test = [ pp_table_test "json" json_table ]
let select_tests = [ pp_table_test "json" ]
let pp_database_test name db = name >:: fun _ -> ignore ()

let print_db =
  [
    pp_database_test "empty table" "database";
    pp_database_test "table" "database";
    pp_database_test "table" "database";
    pp_database_test "table"
      (let a = d2 5 in
       a;
       "database");
  ]

let get_data_rc key_attr key_dat attr tbl =
  let open Table in
  tbl |> get_record key_attr key_dat |> get_data attr

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
  let tbl = make [ ("1", (Int 0)); ("2", (Int 0)); ("3", (Int 0)); ("4", (Int 0)) ] in
  let tbl' = tbl |> update_attr "1" "5" in
  assert_equal (rows tbl') 0;
  assert_equal (columns tbl') 4;
  assert_equal (attributes tbl') [ "5"; "2"; "3"; "4" ]

let test_delete_attr _ =
  let open Table in
  let tbl1 = make [ ("1", (Int 0)) ] in
  let tbl1' = delete_attr "1" tbl1 in
  let tbl2 = make [ ("1", (Int 0)); ("2", (Int 0)); ("3", (Int 0)); ("4", (Int 0)) ] in
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
  let tbl = make [ ("a", String ""); ("b", Int 0) ] |> insert_full_rec [ ("a", Null); ("b", Null) ] in
  let r = tbl |> get_record "a" Null in
  let tbl1 = tbl |> update_data r "a" (String "a1") in
  let tbl2 = tbl |> update_data r "b" (Int 1) in
  let tbl3 =
    tbl1 |> update_data (tbl1 |> get_record "a" (String "a1")) "b" (String "b1")
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
    |> insert_full_rec [ ("a1", Int 11); ("a2", Int 22); ("a3", Int 33) ]
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
    |> insert_full_rec [ ("a1", Int 11); ("a2", Int 22); ("a3", Int 33) ]
  in
  let rec1 = get_record "a1" (Int 1) tbl in
  assert_raises (UnknownAttribute "a4") (fun () -> get_data "a4" rec1);
  assert_raises (UnknownAttribute "a4") (fun () -> get_record "a4" (Int 1) tbl);
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

let print_again = [ pp_database_test "poo" ]

let print_tbl =
  [
    pp_table_test "empty" Table.empty;
    pp_table_test "table" tbl1;
    pp_table_in_database "insert" "cock" tbl2;
  ]

let db_tests = [ pp_db_test "db123" [ db1; db2; db3 ] ]
let testing_db = Database.make_database "testing" "edward"

let json_test name file_name table : test =
  name >:: fun _ ->
  ignore (Table.write_json_to_file file_name table "testing")

let json_tests = [ json_test "json" "test" json_table ]

let suite =
  "test suite for final"
 
  >::: List.flatten
         [ json_tests; print_table_test; print_db; print_tbl; table_tests ]

let _ = run_test_tt_main suite
