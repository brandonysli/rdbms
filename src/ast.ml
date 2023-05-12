type expr =
  | STR of string
  | INT of int
  | FLOAT of float
  | FUN of string * expr list

type cond =
  | EQ of expr * expr
  | NEQ of expr * expr
  | LT of expr * expr
  | GT of expr * expr
  | LE of expr * expr
  | GE of expr * expr
  | AND of cond * cond
  | OR of cond * cond

type stmt =
  | SELECT of string list * string * cond option * string option * cond option
  | INSERT of string * string list * expr list
  | DELETE of string * cond option
  | UPDATE of string * (string * expr) list * cond option
  | TCREATE of string * (string * expr) list
  | TDROP of string
  | DCREATE of string
  | DDROP of string

type prog = stmt
