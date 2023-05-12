type data =
  | String of string
  | Int of int
  | Float of float

type record = (string * data option) list

type t = {
  name : string;
  attributes : string list;
  records : record list;
}
(* Every attribute record list should have the same length*)

let empty name : t = { name; attributes = []; records = [] }
let make name attrs : t = { name; attributes = attrs; records = [] }

let empty_record attrs : record =
  let rec helper (r : record) (a : string list) =
    match a with
    | [] -> r
    | h :: t -> helper ((h, None) :: r) t
  in
  helper [] attrs

exception UnknownAttribute of string
exception UnknownRecord of string
exception EmptyTable of string

let insert_attr name tbl attr =
  {
    name;
    attributes = attr :: tbl.attributes;
    records =
      tbl.records |> List.map (fun r -> r @ [ (attr, None) ])
      (* data for new attribute for records is None *);
  }

let update_attr name tbl old_a new_a =
  {
    name;
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

let delete_attr name tbl attr =
  raise (Failure "Unimplemented: Table.delete_attr")

let insert_rec tbl attr dat =
  {
    tbl with
    records =
      tbl.records
      @ [
          tbl.attributes |> empty_record
          |> List.map (fun p -> if fst p = attr then (attr, Some dat) else p);
        ]
      (* append a new record with the data to the end of the records list *);
  }

let update_data tbl r attr dat =
  {
    tbl with
    records =
      tbl.records
      |> List.map (fun x ->
             if x = r then
               x
               |> List.map (fun p ->
                      if fst p = attr then (attr, Some dat) else p)
             else x);
  }

let delete_rec tbl r = raise (Failure "Unimplemented: Table.delete_rec")

let get_record tbl attr dat =
  tbl.records
  |> List.filter (fun r -> r |> List.assoc attr = Some dat)
  |> List.hd

let get_data tbl r attr =
  tbl.records |> List.find (fun x -> x = r) |> List.assoc attr

let attributes tbl = tbl.attributes
let records tbl = tbl.records
let columns tbl = tbl.attributes |> List.length
let rows tbl = tbl.records |> List.length
