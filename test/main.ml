open OUnit2
open Rdatabase
open Query
open Database

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

let pp_db_test name databases =
  name >:: fun _ -> ignore (pp_databases databases)

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

let db1 = { db_name = "db1"; db_owner = "brandon"; tables = [] }
let db2 = { db_name = "db2"; db_owner = "edward"; tables = [] }
let db3 = { db_name = "database3"; db_owner = "justin"; tables = [] }
let db_tests = [ pp_db_test "db123" [ db1; db2; db3 ] ]
let suite = "test suite for final" >::: List.flatten [ command_tests; db_tests ]
let _ = run_test_tt_main suite
