open Yojson.Safe
open Yojson.Basic.Util

type data =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null

type record = (string * data) list

type t = {
  attributes : (string * data) list;
  records : record list;
}

let debug = false

(* RI: All records have the same number of attributes and all record
   attributes are the same. All attributes are unique.*)

(* [rep_ok tbl] checks that the RI holds for [tbl]. *)
let rec rep_ok (tbl : t) =
  if debug then
    let rep_ok_record r =
      let _ =
        assert (
          List.fold_left
            (fun acc p -> tbl.attributes |> List.mem p |> ( && ) acc)
            true r
          && List.(length tbl.attributes = length r))
      in
      r
    in
    let _ =
      match tbl.records with [] -> [] | h :: t -> rep_ok_record h
    in
    tbl
  else tbl

(* (* Production RI checker. *) let rep_ok = Fun.id *)

let empty : t = { attributes = []; records = [] } |> rep_ok
let make attrs : t = { attributes = attrs; records = [] } |> rep_ok

let empty_record attrs : record =
  let rec helper (r : record) (a : string list) =
    match a with [] -> r | h :: t -> helper ((h, Null) :: r) t
  in
  helper [] attrs

exception UnknownAttribute of string
exception UnknownRecord of string

let insert_attr attr tbl data =
  {
    attributes = (attr, data) :: tbl.attributes;
    records =
      tbl.records |> List.map (fun r -> r @ [ (attr, data) ])
      (* data for new attribute for records is None *);
  }
  |> rep_ok

let update_attr old_a new_a tbl =
  {
    attributes =
      tbl.attributes
      |> List.map (fun (a, b) ->
             if a = old_a then (new_a, b) else (a, b));
    records =
      (* go through every record and change old_a in assoc list to
         new_a *)
      tbl.records
      |> List.map (fun r ->
             r
             |> List.map (fun pair ->
                    if fst pair = old_a then (new_a, snd pair) else pair));
  }
  |> rep_ok

let delete_attr attr tbl =
  let new_attrs =
    List.filter (fun (x, _) -> x <> attr) tbl.attributes
  in
  let new_recs =
    List.map (List.filter (fun (a, _) -> a <> attr)) tbl.records
  in
  { attributes = new_attrs; records = new_recs } |> rep_ok

let insert_rec attr dat tbl =
  {
    tbl with
    records =
      (tbl.attributes
      |> List.map (fun (a, b) -> a)
      |> empty_record
      |> List.map (fun p -> if fst p = attr then (attr, dat) else p))
      :: tbl.records
      (* append a new record with the data to the end of the records
         list *);
  }
  |> rep_ok

let insert_full_rec attr_dat tbl =
  {
    tbl with
    records =
      (tbl.attributes
      |> List.map (fun (x, _) -> x)
      |> empty_record
      |> List.map (fun p ->
             if List.mem_assoc (fst p) attr_dat then
               (fst p, List.assoc (fst p) attr_dat)
             else p))
      :: tbl.records (* append to record list assuming RI holds *);
  }
  |> rep_ok

let update_data r attr dat tbl =
  {
    tbl with
    records =
      tbl.records
      |> List.map (fun x ->
             if x = r then
               x
               |> List.map (fun p ->
                      if fst p = attr then (attr, dat) else p)
             else x);
  }
  |> rep_ok

let delete_rec r tbl =
  { tbl with records = List.filter (fun r' -> r' <> r) tbl.records }
  |> rep_ok

let get_record attr dat tbl =
  tbl.records
  |> List.filter (fun r -> r |> List.assoc attr = dat)
  |> List.hd

let get_data attr r = List.assoc attr r
let attributes tbl = List.map (fun (x, _) -> x) tbl.attributes
let records tbl = tbl.records
let columns tbl = tbl.attributes |> List.length
let rows tbl = tbl.records |> List.length

let pp_data = function
  | String x -> x
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | Bool x -> string_of_bool x
  | Null -> "null"

let rec pad_right_char len char str =
  let pad_len = len - String.length str in
  if pad_len >= 1 then str ^ char |> pad_right_char len char else str

let pad_right len str = pad_right_char len " " str

let trim_or_pad
    (pad : int -> string -> string)
    (len : int)
    (str : string) : string =
  let str' =
    if String.length str > len then String.sub str 0 len else str
  in
  pad len str'

let pp_list
    (pp : 'a -> string)
    (len : int)
    (div : string)
    (lst : 'a list) =
  let rec pp' acc = function
    | [] -> acc
    | h :: t ->
        pp' (acc ^ (h |> pp |> trim_or_pad pad_right len) ^ div) t
  in
  let res = pp' "" lst in
  String.sub res 0 (String.length res - String.length div)

let order_rec_data (attrs : string list) (r : record) : data list =
  List.map (fun a -> r |> List.assoc a) attrs

let pp_records
    (attrs : string list)
    (len : int)
    (div : string)
    (r_list : record list) : string =
  let ordered_recs = r_list |> List.map (order_rec_data attrs) in
  let rec loop acc = function
    | [] -> acc
    | h :: t -> loop (acc ^ pp_list pp_data len div h ^ "\n") t
  in
  loop "" ordered_recs

let pp tbl =
  let attrs = List.map (fun (x, _) -> x) tbl.attributes in
  let max = 120 in
  if List.length attrs > 0 then
    let div = " | " in
    let div_len = String.length div in
    let max_per = (max / List.length attrs) - div_len in
    let recs = tbl.records in
    let attr_str = pp_list (fun x -> x) max_per div attrs in
    let h_line = pad_right_char max "-" "" in
    let rec_str = recs |> pp_records attrs max_per div in
    attr_str ^ "\n" ^ h_line ^ "\n" ^ rec_str
  else pad_right_char max "-" ""
(* attrs |> pp_attributes "" |> () in " " *)

(*JSON STUFF*)

let data_to_json (d : data) : Yojson.Safe.t =
  match d with
  | Int i -> `Int i
  | Float f -> `Float f
  | String s -> `String s
  | Bool b -> `Bool b
  | Null -> `Null

let data_of_json json =
  match json with
  | `Int i -> Int i
  | `Float f -> Float f
  | `String s -> String s
  | `Bool b -> Bool b
  | `Null -> Null
  | _ -> failwith "Invalid data value"

let find key list =
  let rec find_index index = function
    | [] -> raise Not_found
    | (k, _) :: xs ->
        if k = key then index else find_index (index + 1) xs
  in
  find_index 0 list

let record_to_json attrs record =
  `Assoc
    (List.sort
       (fun (key1, _) (key2, _) ->
         compare (find key1 attrs) (find key2 attrs))
       (List.map (fun (key, value) -> (key, data_to_json value)) record))

let t_to_json t =
  let attributes_json =
    `Assoc
      (List.map
         (fun (key, value) -> (key, data_to_json value))
         t.attributes)
  in
  let records_json =
    `List (List.map (record_to_json t.attributes) t.records)
  in
  `Assoc [ ("attributes", attributes_json); ("records", records_json) ]

let make_pretty t = pretty_to_string t

let write_json_to_file filename t database_name =
  let json = t_to_json t in
  let channel =
    open_out
      (Filename.concat (Filename.concat "data" database_name) filename
      ^ ".json")
  in
  output_string channel (make_pretty json);
  close_out channel

let record_of_json json =
  match json with
  | `Assoc fields ->
      List.map (fun (key, value) -> (key, data_of_json value)) fields
  | _ -> failwith "Invalid record value"

let t_of_json json =
  let attributes_json = member "attributes" json in
  let records_json = member "records" json in
  let attributes =
    List.map
      (fun (key, value) -> (key, data_of_json value))
      (to_assoc attributes_json)
  in
  let records = List.map record_of_json (to_list records_json) in
  { attributes; records }

let read_json_file filename =
  let json = Yojson.Basic.from_file filename in
  t_of_json json

let delete_from_table f table =
  {
    attributes = table.attributes;
    records = List.filter f table.records;
  }

let delete_all_from_table table =
  { attributes = table.attributes; records = [] }

let select_table table cols =
  let attributes =
    List.filter (fun (s, d) -> List.mem s cols) table.attributes
  in
  let records =
    List.map
      (fun r -> List.filter (fun (s, d) -> List.mem s cols) r)
      table.records
  in
  { attributes; records }
