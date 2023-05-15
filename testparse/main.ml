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
    parse_test
      "Select with where clause and LT: SELECT a,b FROM Brandon WHERE \
       a < 5;"
      "SELECT a,b FROM Brandon WHERE a < 5;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           None,
           Some (LT (STR "a", INT 5)) ));
    parse_test
      "Select with where clause and GE: SELECT a,b FROM Brandon WHERE \
       a >= 5;"
      "SELECT a,b FROM Brandon WHERE a >= 5;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           None,
           Some (GE (STR "a", INT 5)) ));
    parse_test
      "Select with where clause and LE: SELECT a,b FROM Brandon WHERE \
       a <= 5;"
      "SELECT a,b FROM Brandon WHERE a <= 5;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           None,
           Some (LE (STR "a", INT 5)) ));
    parse_test
      "Select with where clause and NEQ: SELECT a,b FROM Brandon WHERE \
       a != 5;"
      "SELECT a,b FROM Brandon WHERE a != 5;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           None,
           Some (NEQ (STR "a", INT 5)) ));
    parse_test
      "Select with AND in where clause: SELECT a,b FROM Brandon WHERE \
       a > 5 AND b < 10;"
      "SELECT a,b FROM Brandon WHERE a > 5 AND b < 10;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           None,
           Some (AND (GT (STR "a", INT 5), LT (STR "b", INT 10))) ));
    parse_test
      "Select with OR in where clause: SELECT a,b FROM Brandon WHERE a \
       > 5 OR b < 10;"
      "SELECT a,b FROM Brandon WHERE a > 5 OR b < 10;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           None,
           Some (OR (GT (STR "a", INT 5), LT (STR "b", INT 10))) ));
    parse_test
      "Select with complex where clause: SELECT a,b FROM Brandon WHERE \
       a > 5 AND b < 10 OR c = 3;"
      "SELECT a,b FROM Brandon WHERE a > 5 AND b < 10 OR c = 3;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           None,
           Some
             (AND
                ( GT (STR "a", INT 5),
                  OR (LT (STR "b", INT 10), EQ (STR "c", INT 3)) )) ));
    parse_test
      "Select with complex where clause (checking precedence): SELECT \
       a,b FROM Brandon WHERE b < 10 OR c = 3 AND a > 5;"
      "SELECT a,b FROM Brandon WHERE b < 10 OR c = 3 AND a > 5;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           None,
           Some
             (AND
                ( OR (LT (STR "b", INT 10), EQ (STR "c", INT 3)),
                  GT (STR "a", INT 5) )) ));
    parse_test
      "Select with left join: SELECT a,b FROM Brandon LEFT JOIN \
       Brandon2 ON Brandon.id = Brandon2.id;"
      "SELECT a,b FROM Brandon LEFT JOIN Brandon2 ON Brandon.id = \
       Brandon2.id;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           Some
             [
               JOIN
                 ( LEFT,
                   TBL ("Brandon2", None),
                   EQ (PAIR ("Brandon", "id"), PAIR ("Brandon2", "id"))
                 );
             ],
           None ));
    parse_test
      "Select with right join: SELECT a,b FROM Brandon RIGHT JOIN \
       Brandon2 ON Brandon.id = Brandon2.id;"
      "SELECT a,b FROM Brandon RIGHT JOIN Brandon2 ON Brandon.id = \
       Brandon2.id;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           Some
             [
               JOIN
                 ( RIGHT,
                   TBL ("Brandon2", None),
                   EQ (PAIR ("Brandon", "id"), PAIR ("Brandon2", "id"))
                 );
             ],
           None ));
    parse_test
      "Select with full join: SELECT a,b FROM Brandon FULL JOIN \
       Brandon2 ON Brandon.id = Brandon2.id;"
      "SELECT a,b FROM Brandon FULL JOIN Brandon2 ON Brandon.id = \
       Brandon2.id;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           Some
             [
               JOIN
                 ( FULL,
                   TBL ("Brandon2", None),
                   EQ (PAIR ("Brandon", "id"), PAIR ("Brandon2", "id"))
                 );
             ],
           None ));
    parse_test
      "Select with inner join and where clause: SELECT a,b FROM \
       Brandon INNER JOIN Brandon2 ON Brandon.id = Brandon2.id WHERE \
       Brandon.id > 5;"
      "SELECT a,b FROM Brandon INNER JOIN Brandon2 ON Brandon.id = \
       Brandon2.id WHERE Brandon.id > 5;"
      (SELECT
         ( [ "a"; "b" ],
           [ TBL ("Brandon", None) ],
           Some
             [
               JOIN
                 ( INNER,
                   TBL ("Brandon2", None),
                   EQ (PAIR ("Brandon", "id"), PAIR ("Brandon2", "id"))
                 );
             ],
           Some (GT (PAIR ("Brandon", "id"), INT 5)) ));
    parse_test
      "Select with inner join and where clause: SELECT a,b,c FROM \
       Brandon INNER JOIN Brandon2 ON Brandon.id = Brandon2.id WHERE \
       Brandon.id > 5;"
      "SELECT a,b,c FROM Brandon INNER JOIN Brandon2 ON Brandon.id = \
       Brandon2.id WHERE Brandon.id > 5;"
      (SELECT
         ( [ "a"; "b"; "c" ],
           [ TBL ("Brandon", None) ],
           Some
             [
               JOIN
                 ( INNER,
                   TBL ("Brandon2", None),
                   EQ (PAIR ("Brandon", "id"), PAIR ("Brandon2", "id"))
                 );
             ],
           Some (GT (PAIR ("Brandon", "id"), INT 5)) ));
    parse_test
      "Select all with join and 2 aliases and 2 inner joins and where \
       clause"
      "SELECT * FROM Brandon as b, Brandon2 as b2 INNER JOIN Brandon3 \
       as b3 ON b3.id = b2.id INNER JOIN Brandon4 as b4 ON b4.id = \
       b2.id WHERE b.id = b2.id;"
      (SELECT
         ( [ "*" ],
           [ TBL ("Brandon", Some "b"); TBL ("Brandon2", Some "b2") ],
           Some
             [
               JOIN
                 ( INNER,
                   TBL ("Brandon3", Some "b3"),
                   EQ (PAIR ("b3", "id"), PAIR ("b2", "id")) );
               JOIN
                 ( INNER,
                   TBL ("Brandon4", Some "b4"),
                   EQ (PAIR ("b4", "id"), PAIR ("b2", "id")) );
             ],
           Some (EQ (PAIR ("b", "id"), PAIR ("b2", "id"))) ));
  ]

let update_tests =
  [
    parse_test "UPDATE Brandon SET a = 5;" "UPDATE Brandon SET a = 5;"
      (UPDATE ("Brandon", [ ("a", INT 5) ], None));
    parse_test "UPDATE Brandon SET a = 5, b = 10;"
      "UPDATE Brandon SET a = 5, b = 10;"
      (UPDATE ("Brandon", [ ("a", INT 5); ("b", INT 10) ], None));
    parse_test "UPDATE Brandon SET a = 5 WHERE a = 10;"
      "UPDATE Brandon SET a = 5 WHERE a = 10;"
      (UPDATE ("Brandon", [ ("a", INT 5) ], Some (EQ (STR "a", INT 10))));
    parse_test "UPDATE Brandon SET a = 5, b = 10 WHERE a = 10;"
      "UPDATE Brandon SET a = 5, b = 10 WHERE a = 10;"
      (UPDATE
         ( "Brandon",
           [ ("a", INT 5); ("b", INT 10) ],
           Some (EQ (STR "a", INT 10)) ));
    parse_test
      "UPDATE Brandon SET a = 5, b = 10 WHERE a = 10 AND b = 5;"
      "UPDATE Brandon SET a = 5, b = 10 WHERE a = 10 AND b = 5;"
      (UPDATE
         ( "Brandon",
           [ ("a", INT 5); ("b", INT 10) ],
           Some (AND (EQ (STR "a", INT 10), EQ (STR "b", INT 5))) ));
    parse_test "UPDATE Brandon SET a = 5, b = 10 WHERE a = 10 OR b = 5;"
      "UPDATE Brandon SET a = 5, b = 10 WHERE a = 10 OR b = 5;"
      (UPDATE
         ( "Brandon",
           [ ("a", INT 5); ("b", INT 10) ],
           Some (OR (EQ (STR "a", INT 10), EQ (STR "b", INT 5))) ));
    parse_test
      "UPDATE Brandon SET a = 5, b = 10 WHERE a = 10 AND b = 5 OR c = \
       3;"
      "UPDATE Brandon SET a = 5, b = 10 WHERE a = 10 AND b = 5 OR c = \
       3;"
      (UPDATE
         ( "Brandon",
           [ ("a", INT 5); ("b", INT 10) ],
           Some
             (AND
                ( EQ (STR "a", INT 10),
                  OR (EQ (STR "b", INT 5), EQ (STR "c", INT 3)) )) ));
  ]

let insert_tests =
  [
    parse_test "INSERT INTO Brandon (a,b) VALUES (5,10);"
      "INSERT INTO Brandon (a,b) VALUES (5,10);"
      (INSERT ("Brandon", [ "a"; "b" ], [ INT 5; INT 10 ]));
    parse_test "INSERT INTO Brandon (a) VALUES (5);"
      "INSERT INTO Brandon (a) VALUES (5);"
      (INSERT ("Brandon", [ "a" ], [ INT 5 ]));
    parse_test "INSERT INTO Brandon (a,b,c) VALUES (5,10,15);"
      "INSERT INTO Brandon (a,b,c) VALUES (5,10,15);"
      (INSERT ("Brandon", [ "a"; "b"; "c" ], [ INT 5; INT 10; INT 15 ]));
    parse_test "INSERT INTO Brandon (a,b,c) VALUES (5.0,10.0,15.0);"
      "INSERT INTO Brandon (a,b,c) VALUES (5.0,10.0,15.0);"
      (INSERT
         ( "Brandon",
           [ "a"; "b"; "c" ],
           [ FLOAT 5.0; FLOAT 10.0; FLOAT 15.0 ] ));
    parse_test
      "INSERT INTO Brandon (a,b,c) VALUES (\"5\",\"10\",\"15\");"
      "INSERT INTO Brandon (a,b,c) VALUES (\"5\",\"10\",\"15\");"
      (INSERT
         ("Brandon", [ "a"; "b"; "c" ], [ STR "5"; STR "10"; STR "15" ]));
    parse_test "INSERT INTO Brandon (id, name) VALUES (1, \"John\");"
      "INSERT INTO Brandon (id, name) VALUES (1, \"John\");"
      (INSERT ("Brandon", [ "id"; "name" ], [ INT 1; STR "John" ]));
    parse_test
      "INSERT INTO Brandon (id, name, age) VALUES (1, \"John\", 30);"
      "INSERT INTO Brandon (id, name, age) VALUES (1, \"John\", 30);"
      (INSERT
         ( "Brandon",
           [ "id"; "name"; "age" ],
           [ INT 1; STR "John"; INT 30 ] ));
  ]

let delete_tests =
  [
    parse_test "DELETE FROM Brandon;" "DELETE FROM Brandon;"
      (DELETE ("Brandon", None));
    parse_test "DELETE FROM Brandon WHERE a = 5;"
      "DELETE FROM Brandon WHERE a = 5;"
      (DELETE ("Brandon", Some (EQ (STR "a", INT 5))));
    parse_test "DELETE FROM Brandon WHERE a = 5 AND b = 10;"
      "DELETE FROM Brandon WHERE a = 5 AND b = 10;"
      (DELETE
         ( "Brandon",
           Some (AND (EQ (STR "a", INT 5), EQ (STR "b", INT 10))) ));
    parse_test "DELETE FROM Brandon WHERE a = 5 OR b = 10;"
      "DELETE FROM Brandon WHERE a = 5 OR b = 10;"
      (DELETE
         ( "Brandon",
           Some (OR (EQ (STR "a", INT 5), EQ (STR "b", INT 10))) ));
    parse_test "DELETE FROM Brandon WHERE a = 5 AND b = 10 OR c = 15;"
      "DELETE FROM Brandon WHERE a = 5 AND b = 10 OR c = 15;"
      (DELETE
         ( "Brandon",
           Some
             (AND
                ( EQ (STR "a", INT 5),
                  OR (EQ (STR "b", INT 10), EQ (STR "c", INT 15)) )) ));
    parse_test "DELETE FROM Brandon WHERE a = 5 OR b = 10 AND c = 15;"
      "DELETE FROM Brandon WHERE a = 5 OR b = 10 AND c = 15;"
      (DELETE
         ( "Brandon",
           Some
             (AND
                ( OR (EQ (STR "a", INT 5), EQ (STR "b", INT 10)),
                  EQ (STR "c", INT 15) )) ));
  ]

let drop_table_tests =
  [
    parse_test "DROP TABLE Brandon;" "DROP TABLE Brandon;"
      (TDROP "Brandon");
    parse_test "DROP TABLE 23lk4j23k;" "DROP TABLE asdfasdf;"
      (TDROP "asdfasdf");
    parse_test "DROP TABLE DFDFlkdjfsd;" "DROP TABLE DFDFlkdjfsd;"
      (TDROP "DFDFlkdjfsd");
    parse_test "DROP TABLE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD;"
      "DROP TABLE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD;"
      (TDROP "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");
  ]

let drop_db_tests =
  [
    parse_test "DROP DATABASE Brandon;" "DROP DATABASE Brandon;"
      (DDROP "Brandon");
    parse_test "DROP DATABASE asdfasdf;" "DROP DATABASE asdfasdf;"
      (DDROP "asdfasdf");
    parse_test "DROP DATABASE DFDFlkdjfsd;" "DROP DATABASE DFDFlkdjfsd;"
      (DDROP "DFDFlkdjfsd");
  ]

let create_table_tests =
  [
    parse_test "CREATE TABLE Brandon (a INT);"
      "CREATE TABLE Brandon (a INT);"
      (TCREATE ("Brandon", [ ("a", TINT) ]));
    parse_test "CREATE TABLE Brandon (a INT, b FLOAT);"
      "CREATE TABLE Brandon (a INT, b FLOAT);"
      (TCREATE ("Brandon", [ ("a", TINT); ("b", TFLOAT) ]));
    parse_test "CREATE TABLE TestDB (id INT, name STR);"
      "CREATE TABLE TestDB (id INT, name STR);"
      (TCREATE ("TestDB", [ ("id", TINT); ("name", TSTR) ]));
    parse_test "CREATE TABLE TestDB2 (id INT, name STR, age INT);"
      "CREATE TABLE TestDB2 (id INT, name STR, age INT);"
      (TCREATE
         ("TestDB2", [ ("id", TINT); ("name", TSTR); ("age", TINT) ]));
    parse_test "CREATE TABLE TestDB3 (a FLOAT, b FLOAT, c FLOAT);"
      "CREATE TABLE TestDB3 (a FLOAT, b FLOAT, c FLOAT);"
      (TCREATE
         ("TestDB3", [ ("a", TFLOAT); ("b", TFLOAT); ("c", TFLOAT) ]));
  ]

let create_db_tests =
  [
    parse_test "CREATE DATABASE TestDB1;" "CREATE DATABASE TestDB1;"
      (DCREATE "TestDB1");
    parse_test "CREATE DATABASE TestDB2;" "CREATE DATABASE TestDB2;"
      (DCREATE "TestDB2");
    parse_test "CREATE DATABASE TestDB3;" "CREATE DATABASE TestDB3;"
      (DCREATE "TestDB3");
    parse_test "CREATE DATABASE DB123;" "CREATE DATABASE DB123;"
      (DCREATE "DB123");
    parse_test "CREATE DATABASE my_db;" "CREATE DATABASE my_db;"
      (DCREATE "my_db");
    parse_test "CREATE DATABASE dbName;" "CREATE DATABASE dbName;"
      (DCREATE "dbName");
    parse_test "CREATE DATABASE db_test;" "CREATE DATABASE db_test;"
      (DCREATE "db_test");
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
  "parse test suite"
  >::: List.flatten
         [
           select_tests;
           update_tests;
           insert_tests;
           delete_tests;
           drop_table_tests;
           drop_db_tests;
           create_db_tests;
           create_table_tests;
           token_tests;
         ]

let _ = run_test_tt_main suite
