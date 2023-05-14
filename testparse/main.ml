open OUnit2
open Rdatabase
open Database
open Mem
open Interpreter
open Table
open Parse
open Ast

let parse_test name str expected_output : test =
  name >:: fun _ -> assert_equal expected_output (Parse.parse str)

let token_test name str expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (string_of_token (Parse.token (Lexing.from_string str)))

let select_tests =
  [
    parse_test "Standard select all query: SELECT * FROM Brandon;"
      "SELECT * FROM Brandon;"
      (SELECT ([ "*" ], [ TBL ("Brandon", None) ], None, None));
    parse_test "Select two columns from table: SELECT a,b FROM Brandon;"
      "SELECT a,b FROM Brandon;"
      (SELECT ([ "a"; "b" ], [ TBL ("Brandon", None) ], None, None));
    parse_test "Lowercase select and from: select a,b from Brandon;"
      "select a,b from Brandon;"
      (SELECT ([ "a"; "b" ], [ TBL ("Brandon", None) ], None, None));
    parse_test
      "Select with where clause and GT: SELECT a,b FROM Brandon WHERE \
       a > 5;"
      "SELECT a,b FROM Brandon WHERE a > 5;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           None,
           Some (GT (STR "a", INT 5)) ));
    parse_test
      "Select with where clause and GT: SELECT a,b FROM Brandon WHERE \
       Brandon.id = \"id1\";"
      "SELECT a,b FROM Brandon WHERE Brandon.id = \"id1\";"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           None,
           Some (EQ (PAIR ("Brandon", "id"), STR "id1")) ));
    parse_test
      "Select all without where clause and alias: SELECT * FROM \
       Brandon as b;"
      "SELECT * FROM Brandon as b;"
      (SELECT ([ "*" ], [ TBL ("Brandon", Some "b") ], None, None));
    parse_test
      "Select all with where clase and alias: SELECT * FROM 'Brandon' \
       as b WHERE b.id = 'id1';"
      "SELECT * FROM Brandon as b WHERE b.id = \"id1\";"
      (SELECT
         ( [ "*" ],
           [ TBL ("Brandon", Some "b") ],
           None,
           Some (EQ (PAIR ("b", "id"), STR "id1")) ));
    parse_test "Select all with join and 2 aliases and where clause"
      "SELECT * FROM Brandon as b, Brandon2 as b2 INNER JOIN Brandon3 \
       as b3 ON b3.id = b2.id WHERE b.id = b2.id;"
      (SELECT
         ( [ "*" ],
           [ TBL ("Brandon", Some "b"); TBL ("Brandon2", Some "b2") ],
           Some
             [
               JOIN
                 ( INNER,
                   TBL ("Brandon3", Some "b3"),
                   EQ (PAIR ("b3", "id"), PAIR ("b2", "id")) );
             ],
           Some (EQ (PAIR ("b", "id"), PAIR ("b2", "id"))) ));
  ]

let create_db_tests =
  [
    parse_test "CREATE DATABASE Brandon;" "CREATE DATABASE Brandon;"
      (DCREATE "Brandon");
  ]

let token_tests =
  [
    token_test "token test SELECT" "SELECT" "SELECT";
    token_test "token test FROM" "FROM" "FROM";
    token_test "token test INSERT" "INSERT" "INSERT";
    token_test "token test INTO" "INTO" "INTO";
    token_test "token test VALUES" "VALUES" "VALUES";
    token_test "token test CREATE" "CREATE" "CREATE";
    token_test "token test DATABASE" "DATABASE" "DATABASE";
    token_test "token test UPDATE" "UPDATE" "UPDATE";
    token_test "token test SET" "SET" "SET";
    token_test "token test DELETE" "DELETE" "DELETE";
    token_test "token test DROP" "DROP" "DROP";
    token_test "token test TABLE" "TABLE" "TABLE";
    token_test "token test AS" "AS" "as";
    token_test "token test WHERE" "WHERE" "WHERE";
    token_test "token test Brandon.id" "Brandon.id" "(Brandon, id)";
    token_test "token test b.id" "b.id" "(b, id)";
    token_test "token test \"id1\"" "\"id1\"" "\"id1\"";
    token_test "token test alias b" "b" "b";
  ]

let suite =
  "parse test suite" >::: List.flatten [ select_tests; token_tests ]

let _ = run_test_tt_main suite
