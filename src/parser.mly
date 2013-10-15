%{ open Ast %}

%token NEWL RINDENT LINDENT LPAREN RPAREN LBRACK RBRACK COMMA
%token PLUS MINUS PRODUCT QUOTIENT DIVIDES MODULO POWER
%token EQ NEQ GT LT GEQ LEQ AND OR NAND NOR XOR NOT
%token IF WHILE
%token ASSIGN REFINE RETURN CLASS FUNC PRIVATE PROTECTED PUBLIC
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT
%token <string> STRINGLIT
%token <string> ID

/* Want to work on associtivity when I'm a bit fresher */
stmt_list:
   /* nada */       { [] }
   | stmt_list stmt { $2 :: $1 }

stmt:
   expr NEWL                    {Expr($1)}
   | RETURN expr NEWL           {Expr($1)}
   | RINDENT stmt_list LINDENT  {Block(List.rev $2)}
   | IF LPAREN expr RPAREN stmt ELSE stmt {If($3, $5, $7)}
   | WHILE LPAREN expr RPAREN stmt_list {While($3, $5)}
