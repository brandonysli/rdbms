open OUnit2
open Rdatabase
open Query
open Database
open Parse
open Ast

let column_to_string c =
  match c with
  | Distinct col -> "Distinct " ^ String.concat " " col
  | Nondistinct col -> "Nondistinct " ^ String.concat " " col

let selection_to_string s =
  "SELECT "
  ^
  match s with
  | Count column -> "Count " ^ column_to_string column
  | Column column -> "Column " ^ column_to_string column

let table_to_string t =
  match t with
  | From table -> " FROM " ^ table

let condition_to_string c =
  match c with
  | Greater (str, i) -> str ^ " > " ^ string_of_int i
  | Less (str, i) -> str ^ " < " ^ string_of_int i
  | Equal (str, i) -> str ^ " = " ^ string_of_int i

let condition_option_to_string c =
  match c with
  | None -> ""
  | Some c -> " WHERE " ^ condition_to_string c

let query_to_string q =
  selection_to_string q.selection
  ^ table_to_string q.table
  ^ condition_option_to_string q.condition

let query_test (name : string) (str : string) (expected_output : query) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse_query str) ~printer:query_to_string

let command_tests =
  [
    query_test "hi" "SELECT DISTINCT hi FROM world WHERE p > 4"
      {
        selection = Column (Distinct [ "hi" ]);
        table = From "world";
        condition = Some (Greater ("p", 4));
      };
    query_test "hi" "SELECT DISTINCT hi, bye, no FROM world WHERE p > 4"
      {
        selection = Column (Distinct [ "hi"; "bye"; "no" ]);
        table = From "world";
        condition = Some (Greater ("p", 4));
      };
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
  let tbl' = insert_attr "attr1" tbl in
  assert_equal (columns tbl') 1;
  assert_equal (rows tbl') 0;
  assert_equal (attributes tbl') [ "attr1" ]

let test_update_attr _ =
  let open Table in
  let tbl = make [ "1"; "2"; "3"; "4" ] in
  let tbl' = tbl |> update_attr "1" "5" in
  assert_equal (rows tbl') 0;
  assert_equal (columns tbl') 4;
  assert_equal (attributes tbl') [ "5"; "2"; "3"; "4" ]

let test_delete_attr _ =
  let open Table in
  let tbl1 = make [ "1" ] in
  let tbl1' = delete_attr "1" tbl1 in
  let tbl2 = make [ "1"; "2"; "3"; "4" ] in
  let tbl2' = delete_attr "2" tbl2 in
  assert_equal (columns tbl1') 0;
  assert_equal (columns tbl2') 3;
  assert_equal (attributes tbl2') [ "1"; "3"; "4" ]

let test_insert_rec _ =
  let open Table in
  let tbl = make [ "a"; "b" ] in
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
  let tbl = make [ "a"; "b" ] |> insert_full_rec [ ("a", Null); ("b", Null) ] in
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
    make [ "a1"; "a2"; "a3" ]
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
    make [ "a1"; "a2"; "a3" ]
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

let parse_test name str expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Parse.parse str)

let parse_tests =
  [
    parse_test "SELECT * FROM Brandon;" "SELECT * FROM Brandon;"
      (SELECT ([ "*" ], "Brandon", None, None, None));
    parse_test "CREATE DATABASE Brandon;" "CREATE DATABASE Brandon;"
      (DCREATE "Brandon");
  ]

let suite =
  "test suite for final"
  >::: List.flatten [ command_tests; parse_tests; table_tests ]

let _ = run_test_tt_main suite
