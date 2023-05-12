open Rdatabase
open Database
open Table

let db1 = { db_name = "db1"; db_owner = "brandon"; tables = [] }
let db2 = { db_name = "db2"; db_owner = "edward"; tables = [] }
let db3 = { db_name = "database3"; db_owner = "justin"; tables = [] }
let db4 = { db_name = "db4"; db_owner = "chris"; tables = [] }
let db5 = { db_name = "test_database_5"; db_owner = "michael"; tables = [] }
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

let tb1 = make [ "col1"; "col2"; "col3"; "col4"; "col5" ]

let _ =
  ignore (pp_databases databases1);
  print_endline "";
  ignore (pp_databases databases2)
