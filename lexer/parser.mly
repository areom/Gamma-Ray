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


%start stmt /* The start symbol */
/*%type <Ast.expr> expr */
%type <Ast.stmt> stmt 
%%

/* Want to work on associtivity when I'm a bit fresher */
stmt_list:
   /* nada */      { [] }
   | stmt_list stmt { $2 :: $1 }

stmt:
/*   expr NEWL                    {Expr($1)}
   | RETURN expr NEWL           {Expr($2)}*/
   | LPAREN stmt_list RPAREN  {Block(List.rev $2)}
/*   | IF LPAREN expr RPAREN stmt ELSE stmt {If($3, $5, $7)}
   | WHILE LPAREN expr RPAREN stmt_list {While($3, $5)}*/
