type object_phrase = string list

exception Empty
exception Malformed

type condition =
  | Greater of (string * int)
  | Less of (string * int)
  | Equal of (string * int)

type column = Distinct of string list | Nondistinct of string list
type selection = Count of column | Column of column

type query = {
  columns : selection;
  table : string;
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

let parse str =
  let lst = List.filter (fun l -> l <> "") (String.split_on_char ' ' str) in
  let selection, _ = parse_selection lst in
  { columns = selection; table = ""; condition = None }
