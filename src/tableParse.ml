type t = Yojson.Basic.t

exception FileException

let from_file str = Yojson.Basic.from_file str

exception ParseException

let conv (yj : Yojson.Basic.t) : Table.data =
  match yj with
  | `String x -> Table.String x
  | `Int x -> Int x
  | `Float x -> Float x
  | `Bool x -> Bool x
  | `Null -> Null
  | _ -> raise ParseException

let parse j : Table.t =
  let open Yojson.Basic.Util in
  (* ['] indicates var contains unconverted Yojson types *)
  let table' = j |> to_list in
  (* elements of JSON table *)
  let records' = table' |> List.map to_assoc in
  let records =
    List.map (List.map (fun (a, d) -> (a, conv d))) records'
  in
  let tbl =
    List.fold_right
      (fun (a, d) (t : Table.t) -> Table.insert_attr a t d)
      (List.hd records) Table.empty
  in
  List.fold_right (fun r t -> Table.insert_full_rec r t) records tbl
