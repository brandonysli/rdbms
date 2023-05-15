(* repl.ml *)
open Rdatabase
open Parse
open Ast
open Database
open Mem
open Interpreter
open State

let rec repl st () =
  if get_database st = "" then (
    print_endline
      "No Database has been selected! Please select a database or \
       create a new one!";
    Mem.list_databases ();
    print_string "> ";
    let line = read_line () in
    if line = "quit" then ()
    else if line = "clear" then clear st
    else
      match parse line with
      | exception Failure _ -> not_database_handler line st
      | DCREATE name ->
          let new_st = interpret (parse line) st in
          repl new_st ()
      | _ -> not_database_handler line st)
  else if
    st |> get_database |> database_from_file |> get_database_owner
    <> get_owner st
  then (
    print_endline
      "You are not the owner of this database! Try another database!";
    not_database_handler (read_line ()) st)
  else (
    print_string ("Database " ^ State.get_database st ^ "> ");
    let line = read_line () in
    if line = "quit" then ()
    else if line = "clear" then clear st
    else
      match parse line with
      | exception Failure s ->
          print_endline "Invalid SQL input!";
          repl st ()
      | _ ->
          let ast = parse line in
          let new_st = interpret ast st in
          print_endline "Success!";
          repl new_st ())

and clear st =
  print_endline "Are you sure you want to clear all databases? (Y/N)";
  match read_line () with
  | "Y" ->
      print_endline "clearing databases";
      Mem.clear_databases ()
  | _ ->
      print_endline "failed to clear databases";
      repl st ()

and not_database_handler line st =
  let s = line in
  if s = "quit" then ()
  else if Mem.is_database s then
    let new_st = update_state s (get_owner st) in
    let _ = Mem.print_tables (State.get_database new_st) () in
    repl new_st ()
  else if line = "quit" then ()
  else (
    print_endline "That is not a valid database!";
    print_endline "";
    repl st ())

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to the least scuffed Relational Database Management System!\n";
  print_endline "What is your name?";
  print_string "> ";
  let owner = read_line () in
  let st = make_owner owner in
  print_endline ("Welcome " ^ owner ^ "!");
  repl st ()

let () = main ()
