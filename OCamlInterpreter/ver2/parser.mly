%{
	open Syntax
	(* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>	INT
%token <bool>	BOOL 
%token <string> ID
%token LET IN					
%token PLUS TIMES MINUS DIV
%token AND OR
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR
%token FUN ARROW
%token REC
%token SEMISEMI
%token LETAND
%token UnknownToken
%token EXIT

%start toplevel 
%type <Syntax.command> toplevel
%% 

toplevel:
	| expr SEMISEMI { CExp $1 }
    | level SEMISEMI { CDeclare $1 }
    | EXIT SEMISEMI { CExit }
;

level:
    | LET var EQ expr          { CDecl ($2, $4) }
    | LET var EQ expr level    { CDeclLet ($2, $4, $5) }
    | LET var EQ expr LETAND and_expr { CDeclAnd ($2, $4, $6) }
    | LET REC var var EQ expr         { CRecDecl ($3, $4, $6) }
    | LET REC var var EQ expr IN expr { CLetRec ($3, $4, $6, $8) }
    | LET REC var var EQ expr LETAND recand_expr { CRecAnd ($3, $4, $6, $8) }
;

recand_expr:
    | var var EQ expr LETAND recand_expr   { CRecAnd ($1,$2,$4,$6) }
    | var var EQ expr                   { CRecDecl ($1,$2,$4) }
;

and_expr:
    | var EQ expr                 { CDecl ($1, $3) }
    | var EQ expr level           { CDeclAL ($1, $3, $4) }
    | var EQ expr LETAND and_expr { CDeclAnd ($1, $3, $5) }
;

expr:
	| LET var EQ expr IN expr	  { ELet($2,$4,$6) }
	| IF expr THEN expr ELSE expr { EIf($2,$4,$6) }
    | FUN var ARROW expr          { EFun($2,$4) }
	| arith_expr EQ arith_expr	  { EEq($1,$3) }
	| arith_expr LT arith_expr 	  { ELt($1,$3) }
	| arith_expr				  { $1 }
    | UnknownToken                { EUnknownToken }
;

arith_expr:
	| arith_expr PLUS factor_expr  { EAdd($1,$3) }
	| arith_expr MINUS factor_expr { ESub($1,$3) }
	| factor_expr	  			   { $1 }
;

factor_expr:
    | factor_expr TIMES boolean_expr { EMul($1,$3) }
    | factor_expr DIV boolean_expr   { EDiv($1,$3) }
    | boolean_expr               { $1 }
;

boolean_expr:
    | boolean_expr AND app_expr { EAnd($1,$3) }
    | boolean_expr OR app_expr  { EOr($1,$3) }
    | app_expr                  { $1 }
;

app_expr:
    | app_expr atomic_expr { EApp($1, $2) }
    | atomic_expr          { $1 }
;

atomic_expr:
	| INT 			 { EConstInt($1) }
	| BOOL			 { EConstBool($1) }
	| ID			 { EVar($1) }
	| LPAR expr RPAR { $2 }
;
 
var:
	| ID { $1 }
;
