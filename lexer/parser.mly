%{ open Ast %}

%token NEWL RINDENT LINDENT LPAREN RPAREN LBRACKET RBRACKET COMMA LBRACE RBRACE
%token PLUS MINUS TIMES DIVIDE MOD POWER ASSIGN
%token EQ NEQ GT LT GEQ LEQ AND OR NAND NOR XOR NOT TRUE FALSE
%token IF ELSE WHILE
%token ASSIGN RETURN CLASS EXTEND SUPER INIT FUNC PRIVATE PROTECTED PUBLIC
%token NULL VOID
%token NEW MAIN 
%token REFINABLE REFINE REFINES TO 
%token SEMI COMMA DOT EOF

%token <string> VAR
%token <string> TYPE
%token <int> ILIT
%token <float> FLIT
%token <bool> BOOLLIT
%token <string> SLIT
%token <string> ID

/* Want to work on associtivity when I'm a bit fresher */
%left EQ NEQ /* Equality operators: ==, != */
%left LT GT LEQ GEQ /* Comparison operators: <, >, <=, => */
%left PLUS MINUS /* Additive operators: +, - */
%left TIMES DIVIDE /* Multiplicative operators: *, / */

%start stmt /* The start symbol */
%type <Ast.stmt> stmt 


%%


lit:
	  ILIT { Int($1) }
	| FLIT { Float($1) }
expr:
	  lit { Literal($1) }
	| expr PLUS expr { Binop($1, Arithmatic(Add), $3) }
	| expr MINUS expr { Binop($1, Arithmatic(Sub), $3) }
	| expr TIMES expr { Binop($1, Arithmatic(Prod) , $3) }
	| expr DIVIDE expr { Binop($1, Arithmatic(Div) , $3) }
	| expr EQ expr { Binop($1, NumTest(Eq), $3) }
	| expr NEQ expr { Binop($1, NumTest(Neq), $3) }
	| expr LT expr { Binop($1, NumTest(Less), $3) }
	| expr LEQ expr { Binop($1, NumTest(Leq), $3) }
	| expr GT expr { Binop($1, NumTest(Grtr), $3) }
	| expr GEQ expr { Binop($1, NumTest(Geq), $3) }
	| LPAREN expr RPAREN { $2 }

stmt_list:
   /* nada */      { [] }
   | stmt_list stmt { $2 :: $1 }

stmt:
       expr SEMI 		{Expr($1)}
/*   | RETURN expr              {Expr($2)}*/
     | LBRACE stmt_list RBRACE  {Block(List.rev $2)}
/*   | IF LPAREN expr RPAREN stmt ELSE stmt {If($3, $5, $7)}*/
     | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE {While($3, $6)}
