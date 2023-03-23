type data = String of string | Int of int | Float of float
type record = (string * data option) list
type t = { attributes : string list; records : record list }
(* Every attribute record list should have the same length*)

let empty () : t = { attributes = []; records = [] }
let make attrs : t = { attributes = attrs; records = [] }

exception UnknownAttribute of string
exception UnknownRecord of string

let insert_attr tbl attr =
  {
    attributes = attr :: tbl.attributes;
    records =
      tbl.records |> List.map (fun r -> r @ [ (attr, None) ])
      (* data for new attribute for records is None *);
  }

let update_attr tbl old_a new_a =
  {
    attributes =
      tbl.attributes |> List.map (fun a -> if a = old_a then new_a else a);
    records =
      (* go through every record and change old_a in assoc list to new_a *)
      tbl.records
      |> List.map (fun r ->
             r
             |> List.map (fun pair ->
                    if fst pair = old_a then (new_a, snd pair) else pair));
  }

let delete_attr tbl attr = raise (Failure "Unimplemented: Table.delete_attr")
let insert_rec tbl attr r = raise (Failure "Unimplemented: Table.insert_rec")

let update_data tbl r attr dat =
  raise (Failure "Unimplemented: Table.update_data")

let delete_rec tbl r = raise (Failure "Unimplemented: Table.delete_rec")
let get_record tbl attr dat = raise (Failure "Unimplemented: Table.get_record")
let get_data tbl r attr = raise (Failure "Unimplemented: Table.get_data")
let attributes tbl = raise (Failure "Unimplemented: Table.attributes")
let records tbl = raise (Failure "Unimplemented: Table.records")
let columns tbl = tbl.attributes |> List.length
let rows tbl = tbl.records |> List.length
