%{
  open Ast
  open List
%}

%token <int> INT
%token <float> FLOAT
%token <string> STR
%token <string> ID 
%token <string * string> PAIR 
%token LPAREN RPAREN COMMA STAR EQUALS LT GT LE GE NEQ AND OR SC AS 
%token SELECT FROM INSERT INTO VALUES DELETE UPDATE CREATE TABLE DROP DATABASE SET WHERE
%token INNER LEFT RIGHT FULL JOIN ON


%left AND
%left OR

%start <Ast.prog> prog

%%

prog:
  | stmt { $1 }

stmt:
  | SELECT column_list FROM table_list SC { SELECT(rev $2, rev $4, None, None) }
  | SELECT column_list FROM table_list WHERE cond SC { SELECT(rev $2, rev $4, None, Some $6) } 
  | SELECT column_list FROM table_list join_list SC { SELECT(rev $2, rev $4, Some (rev $5), None) }
  | SELECT column_list FROM table_list join_list WHERE cond SC { SELECT(rev $2, rev $4, Some (rev $5), Some $7) }
  | INSERT INTO ID LPAREN id_list RPAREN VALUES LPAREN expr_list RPAREN SC { INSERT($3, $5, $9) }
  | DELETE FROM ID SC { DELETE($3, None) }
  | UPDATE ID SET update_list SC { UPDATE($2, $4, None) }
  | CREATE TABLE ID LPAREN id_type_list RPAREN SC { TCREATE($3, $5) }
  | DROP TABLE ID SC { TDROP($3) }
  | CREATE DATABASE ID SC { DCREATE($3) }
  | DROP DATABASE ID SC { DDROP($3) }

column_list:
  | STAR { ["*"] }
  | id_list { $1 }

table:
  | ID { TBL($1, None) }
  | ID AS ID { TBL($1, Some $3) }

table_list:
  | table { [$1] }
  | table_list COMMA table { $3 :: $1 }

join_type:
  | INNER { INNER }
  | LEFT { LEFT }
  | RIGHT { RIGHT }
  | FULL { FULL }

join_list:
  | join { [$1] }
  | join_list join { $2 :: $1 }

join:
  | join_type JOIN table ON cond { JOIN($1, $3, $5) }

id_type_list:
  | ID expr { [($1, $2)] }
  | id_type_list COMMA ID expr { ($3, $4) :: $1 }

id_list:
  | ID { [$1] }
  | id_list COMMA ID { $3 :: $1 }

expr_list:
  | expr { [$1] }
  | expr_list COMMA expr { $3 :: $1 }

expr:
  | INT { INT $1 }
  | FLOAT { FLOAT $1 }
  | STR { STR $1 }
  | PAIR { PAIR (fst $1, snd $1) }
  | ID { STR $1 }
  | ID LPAREN expr_list RPAREN { FUN($1, $3) }

cond:
  | expr EQUALS expr { EQ($1, $3) }
  | expr NEQ expr { NEQ($1, $3) }
  | expr LT expr { LT($1, $3) }
  | expr GT expr { GT($1, $3) }
  | expr LE expr { LE($1, $3) }
  | expr GE expr { GE($1, $3) }
  | cond AND cond { AND($1, $3) }
  | cond OR cond { OR($1, $3) }

update_list:
  | ID EQUALS expr { [($1, $3)] }
  | update_list COMMA ID EQUALS expr { ($3, $5) :: $1 }
