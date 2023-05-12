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
  let elements' = j |> to_assoc in (* elements of JSON database *)
  let records' = List.map (fun (_, x) -> to_assoc x) elements' in 
  let records = List.map (List.map (fun (a, d) -> (a, conv d))) records' in
  let tbl =
    List.fold_right
      (fun (a, _) (t : Table.t) -> Table.insert_attr a t)
      (List.hd records) Table.empty
  in
  List.fold_right (fun r t -> Table.insert_full_rec r t) records tbl
