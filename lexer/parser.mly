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
	| expr PLUS expr { Binop($1, Arithmetic(Add), $3) }
	| expr MINUS expr { Binop($1, Arithmetic(Sub), $3) }
	| expr TIMES expr { Binop($1, Arithmetic(Prod) , $3) }
	| expr DIVIDE expr { Binop($1, Arithmetic(Div) , $3) }
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
       expr	{Expr($1)}
    | RETURN expr          {Expr($2)}
    | LBRACE stmt_list RBRACE  {Block(List.rev $2)}
    | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE  {If($3, List.rev $6, List.rev $10)}
    | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE {While($3, List.rev $6)}

cdecl:
	CLASS TYPE extend_opt LBRACE  private_list public_list protected_list refinement_list main_opt RBRACE 
	{ { cname = $2; supername=$3; cprivate =$5; cpublic=$6; crefine=$7; cmain=$9  }}

main_opt:
		{ }
	| MAIN LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE  
		{ { fname = "main"; fstatic = 1; formals = $3; locals=List.rev $6; body=List.rev $7}   }

extend_opt:
 		{ }
	| EXTEND TYPE  { $2 }

private_list:
		{ [] }
	| PRIVATE LBRACE member_list RBRACE { List.rev $3 }
public_list:
		{  [] }
	| PUBLIC LBRACE member_list RBRACE  { List.rev $3 }
protected_list:
		{ [] }
	| PROTECTED LBRACE member_list RBRACE  { List.rev $3 }
refinement_list:
		{ [] }
	| REFINES LBRACE refine_list RBRACE { List.rev $3 }
refine_list:
	  refmem { [$1] }
	| refine_list refmem { $2 :: $1 }
formals_opt:
		{ [] }
	| formal_list {List.rev $1}
formals_list:
	  VAR { [$1] }
	| formal_list  COMMA VAR {$3 :: $1}

refmem:
	TYPE ID DOT ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
member_list:
		{ [] }
	| member_list member 	{ $2 :: $1 }
member:
	  vdecl_list 	{List.rev $1}
/*	| fdecl_list
	| INIT 
*/
vdecl_list:
		{ [] }
	| vdecl_list vdecl	{ $2 :: $1 }

vdecl:
	TYPE VAR 		{ $2 }

/*

cdecl:
	   CLASS ID LBRACE section_list RBRACE 			{cname = $2;}
	| CLASS ID EXTEND ID LBRACE section_list RBRACE 	{cname = $2; supername = $4;}
section_list:
							{ [] }
	| section_list section { $2 :: $1 }

section:
	  rfmt
	| agroup
	| MAIN

agroup:
	  PRIVATE LBRACE member_list RBRACE
	| PUBLIC LBRACE member_list RBRACE
	| PROTECTED LBRACE member_list RBRACE
*/
