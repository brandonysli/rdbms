type object_phrase = string list

exception Empty
exception Malformed

type condition =
  | Greater of (string * int)
  | Less of (string * int)
  | Equal of (string * int)

type table = From of string
type column = Distinct of string list | Nondistinct of string list
type selection = Count of column | Column of column

type query = {
  columns : selection;
  table : table;
  condition : condition option;
}

let rec get_input list_head list_tail =
  match list_tail with
  | [] -> (list_head, [])
  | head :: tail ->
      let s = String.lowercase_ascii head in
      if s = "select" || s = "count" || s = "from" || s = "where" then
        (list_head, list_tail)
      else get_input (list_head @ [ head ]) tail

let get_columns lst =
  match lst with
  | [] -> raise Malformed
  | head :: tail ->
      if String.lowercase_ascii head = "distinct" then
        let h, t = get_input [] tail in
        (Distinct h, t)
      else
        let h, t = get_input [] lst in
        (Nondistinct h, t)

let parse_selection lst =
  match lst with
  | [] -> raise Empty
  | head :: tail ->
      if String.lowercase_ascii head = "select" then
        match tail with
        | [] -> raise Malformed
        | head2 :: tail2 ->
            if String.lowercase_ascii head2 = "count" then
              let h, t = get_columns tail2 in
              (Count h, t)
            else
              let h, t = get_columns tail2 in
              (Column h, t)
      else raise Malformed

let parse_table lst =
  match lst with
  | [] -> raise Malformed
  | head :: tail ->
      if String.lowercase_ascii head = "from" then
        let h, t = get_input [] tail in
        (From (String.concat " " h), t)
      else raise Malformed

let get_int lst =
  match lst with
  | [] -> raise Malformed
  | [ x ] -> int_of_string x
  | _ :: _ -> raise Malformed

let parse_condition lst =
  match lst with
  | [] -> None
  | head :: tail ->
      if String.lowercase_ascii head = "where" then
        let data, t = get_input [] tail in
        match t with
        | [] -> raise Malformed
        | head :: tail -> (
            match head with
            | ">" ->
                let i = get_int tail in
                Some (Greater (String.concat " " data, i))
            | "<" ->
                let i = get_int tail in
                Some (Less (String.concat " " data, i))
            | "=" ->
                let i = get_int tail in
                Some (Equal (String.concat " " data, i))
            | _ -> raise Malformed)
      else raise Malformed

let parse str =
  let lst = List.filter (fun l -> l <> "") (String.split_on_char ' ' str) in
  let selection, next = parse_selection lst in
  let table, next2 = parse_table next in
  let cond = parse_condition next2 in
  { columns = selection; table; condition = cond }
