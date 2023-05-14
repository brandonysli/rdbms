%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STR
%token <string> ID 
%token <string * string> PAIR 
%token LPAREN RPAREN COMMA STAR EQUALS LT GT LE GE NEQ AND OR SC AS
%token SELECT FROM INSERT INTO VALUES DELETE UPDATE CREATE TABLE DROP DATABASE SET WHERE



%left AND
%left OR

%start <Ast.prog> prog

%%

prog:
  | stmt { $1 }

stmt:
  | SELECT column_list FROM STR SC { SELECT($2, $4, None, None, None, None) }
  | SELECT column_list FROM STR AS ID SC { SELECT($2, $4, Some $6, None, None, None) }  
  | SELECT column_list FROM STR WHERE cond SC { SELECT($2, $4, None, Some $6, None, None) }
  | SELECT column_list FROM STR AS ID WHERE cond SC { SELECT($2, $4, Some $6, Some $8, None, None) } 
  | INSERT INTO STR LPAREN id_list RPAREN VALUES LPAREN expr_list RPAREN SC { INSERT($3, $5, $9) }
  | DELETE FROM STR SC { DELETE($3, None) }
  | UPDATE STR SET update_list SC { UPDATE($2, $4, None) }
  | CREATE TABLE STR LPAREN id_type_list RPAREN SC { TCREATE($3, $5) }
  | DROP TABLE STR SC { TDROP($3) }
  | CREATE DATABASE STR SC { DCREATE($3) }
  | DROP DATABASE STR SC { DDROP($3) }

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
