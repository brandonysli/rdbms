open Str

type object_phrase = string list

exception Empty
exception Malformed

(*|---------------------------------------------------------------------------|
  |-------------------------- Query Parsing Starts ---------------------------|
  |---------------------------------------------------------------------------|
  |---------------------------------------------------------------------------| *)

type condition =
  | Greater of (string * int)
  | Less of (string * int)
  | Equal of (string * int)

type table = From of string

type column =
  | Distinct of string list
  | Nondistinct of string list

type selection =
  | Count of column
  | Column of column

type query = {
  selection : selection;
  table : table;
  condition : condition option;
}

let rec get_input list_head list_tail =
  match list_tail with
  | [] -> (list_head, [])
  | head :: tail ->
      let s = String.lowercase_ascii head in
      if
        s = "select" || s = "count" || s = "from" || s = "where" || s = ">"
        || s = "<" || s = "="
      then (list_head, list_tail)
      else get_input (list_head @ [ head ]) tail

let get_columns lst =
  match lst with
  | [] -> raise Malformed
  | head :: tail ->
      if String.lowercase_ascii head = "distinct" then
        let h, t = get_input [] tail in
        let lst = Str.split (Str.regexp ", ") (String.concat " " h) in
        (Distinct lst, t)
      else
        let h, t = get_input [] lst in
        let lst = Str.split (Str.regexp ", ") (String.concat " " h) in
        (Nondistinct lst, t)

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
              let h, t = get_columns tail in
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
        | head2 :: tail2 -> (
            match head2 with
            | ">" ->
                let i = get_int tail2 in
                Some (Greater (String.concat " " data, i))
            | "<" ->
                let i = get_int tail2 in
                Some (Less (String.concat " " data, i))
            | "=" ->
                let i = get_int tail2 in
                Some (Equal (String.concat " " data, i))
            | _ -> raise Malformed)
      else raise Malformed

let parse_query str =
  let lst = List.filter (fun l -> l <> "") (String.split_on_char ' ' str) in
  let selection, next = parse_selection lst in
  let table, next2 = parse_table next in
  let cond = parse_condition next2 in
  { selection; table; condition = cond }

(*|---------------------------------------------------------------------------|
  |-------------------------Definition Parsing Starts-------------------------|
  |---------------------------------------------------------------------------|
  |---------------------------------------------------------------------------| *)

type data =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string

type column = {
  name : string;
  data : data;
}

type creation =
  | Table of {
      table_name : string;
      columns : column list;
    }
  | Database of string

let parse_data str =
  if Str.string_match (Str.regexp "int\((?:\d{1,3})?\)") str 0 then Int 255
  else if Str.string_match (Str.regexp "float\((?:\d{1,3})?\)") str 0 then
    Float 255.
  else if Str.string_match (Str.regexp "bool\((?:\d{1,3})?\)") str 0 then
    Bool true
  else if Str.string_match (Str.regexp "string\((?:\d{1,3})?\)") str 0 then
    String "hi"
  else raise Malformed

let parse_tables lst =
  let csv = lst |> String.concat " " |> Str.split (Str.regexp ", ") in
  match csv with
  | [] -> raise Malformed
  | [ name; data ] -> { name; data = parse_data data }
  | head :: tail -> raise Malformed

let parse_table_creation lst =
  let table_name, next = get_input [] lst in
  let tables = parse_tables next in
  Table { table_name = "table"; columns = [ tables ] }

let parse_def str =
  let lst = List.filter (fun l -> l <> "") (String.split_on_char ' ' str) in
  match lst with
  | [] -> raise Empty
  | head :: tail -> (
      match String.lowercase_ascii head with
      | "table" -> parse_table_creation tail
      | "database" -> Database "Unimplemented"
      | _ -> raise Malformed)
