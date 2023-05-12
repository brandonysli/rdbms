%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STR
%token <string> ID ID2
%token LPAREN RPAREN COMMA STAR EQUALS LT GT LE GE NEQ AND OR SC
%token SELECT FROM INSERT INTO VALUES DELETE UPDATE CREATE TABLE DROP DATABASE SET WHERE



%left AND
%left OR

%start <Ast.prog> prog

%%

prog:
  | stmt { $1 }

stmt:
  | SELECT column_list FROM ID SC { SELECT($2, $4, None, None, None) }
  | SELECT column_list FROM ID WHERE cond SC { SELECT($2, $4, Some $6, None, None) }
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
  | ID2 { }
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
