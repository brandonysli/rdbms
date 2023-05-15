open Rdatabase
open Database
open Table

let db1 = Database.make_database "db1" "brandon"
let db2 = Database.make_database "db2" "edward"
let db3 = Database.make_database "database3" "justin"
let db4 = Database.make_database "db4" "chris"
let db5 = Database.make_database "test_database_5" "michael"
let databases1 = [ db1; db2; db3; db4; db5 ]

let databases2 =
  [
    db1;
    db2;
    db3;
    db4;
    db5;
    db1;
    db2;
    db3;
    db4;
    db5;
    db1;
    db2;
    db3;
    db4;
    db5;
    db1;
    db2;
    db3;
    db4;
    db5;
    db1;
    db2;
    db3;
    db4;
    db5;
  ]

let tb1 = make []

let _ =
  ignore (pp_databases databases1);
  print_endline "";
  ignore (pp_databases databases2)
