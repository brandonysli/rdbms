type expr =
  | STR of string
  | INT of int
  | FLOAT of float
  | PAIR of string * string
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

type join_type =
  | INNER
  | LEFT
  | RIGHT
  | FULL

type table = TBL of string * string option
type join = JOIN of join_type * table * cond

type stmt =
  | SELECT of string list * table list * join list option * cond option
  | INSERT of string * string list * expr list
  | DELETE of string * cond option
  | UPDATE of string * (string * expr) list * cond option
  | TCREATE of string * (string * expr) list
  | TDROP of string
  | DCREATE of string
  | DDROP of string

type prog = stmt
