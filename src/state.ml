type t = {
  database : string;
  user : string;
}

let empty = { database = "db1"; user = "brandon" }
let update_state database user = { database; user }
let make_owner user = { database = ""; user }
let get_database t = t.database
let get_owner t = t.user
