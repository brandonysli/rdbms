(* repl.ml *)
open Rdatabase
open Parse
open Ast
open Database
open Mem
open Interpreter

let rec repl () =
  print_string "> ";
  let line = read_line () in
  match line with
  | "quit" -> ()
  | _ ->
      let ast = parse line in
      ignore (interpret ast);
      (* Pass the memory state to `pp_mem` *)
      (* print_endline ("The current state of\n the memory: " ^
         pp_mem); *)
      repl ()

let () = repl ()
