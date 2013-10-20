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
%token <bool> BLIT
%token <string> SLIT
%token <string> ID

/* Want to work on associtivity when I'm a bit fresher */
%left EQ NEQ /* Equality operators: ==, != */
%left LT GT LEQ GEQ /* Comparison operators: <, >, <=, => */
%left PLUS MINUS /* Additive operators: +, - */
%left TIMES DIVIDE /* Multiplicative operators: *, / */

%start cdecl /* The start symbol */
%type <Ast.class_def> cdecl


%%


lit:
  | SLIT { String($1) }
  | ILIT { Int($1) }
  | FLIT { Float($1) }
  | BLIT { Boolean($1) }

expr:
  /* Literals are expressions */
  | lit { Literal($1) }

  /* Arithmetic operations are expressions */
  | expr PLUS expr    { Binop($1, Arithmetic(Add), $3) }
  | expr MINUS expr   { Binop($1, Arithmetic(Sub), $3) }
  | expr TIMES expr   { Binop($1, Arithmetic(Prod) , $3) }
  | expr DIVIDE expr  { Binop($1, Arithmetic(Div) , $3) }

  /* Boolean operations are expressions */
  | expr EQ expr   { Binop($1, NumTest(Eq), $3) }
  | expr NEQ expr  { Binop($1, NumTest(Neq), $3) }
  | expr LT expr   { Binop($1, NumTest(Less), $3) }
  | expr LEQ expr  { Binop($1, NumTest(Leq), $3) }
  | expr GT expr   { Binop($1, NumTest(Grtr), $3) }
  | expr GEQ expr  { Binop($1, NumTest(Geq), $3) }

  /* Parentheses for grouping; saving the
   * day in languages since the beginning
   */
  | LPAREN expr RPAREN { $2 }

stmt_block:
  | LBRACE stmt_list RBRACE  { List.rev $2 }
stmt_list:
  | /* nada */     { [] }
  | stmt_list stmt { $2 :: $1 }
stmt:
  | expr                           { Expr($1) }
  | RETURN expr                    { Expr($2) }
  | stmt_block                     { Block($1) }
  | IF pred stmt_block stmt_block  { If($2, $3, $4) }
  | WHILE pred stmt_block          { While($2, $3) }
pred:
  | LPAREN expr RPAREN       { $2 }

/* Class and subclassing -- cdecl / extend_opt */
cdecl:
  | CLASS TYPE extend_opt LBRACE private_list public_list protected_list refinement_list main_opt RBRACE 
    { { class     = $2;
        parent    = $3;
        privates  = $5;
        protects  = $6;
        publics   = $7;
        refines   = $8;
        main      = $9 } }
extend_opt:
  | { None }
  | EXTEND TYPE { Some($2) }

/* Private, protected, public members */
private_list:
  | PRIVATE members { $2 }
protected_list:
  | PROTECTED members { $2 }
public_list:
  | PUBLIC members { $2 }

/* Members of such access groups */
members:
  | LBRACE member_list RBRACE { List.rev $2 }
member_list:
  | { [] }
  | member_list member { $2 :: $1 }
member:
  | vdecl_list { $1 }
  | mdecl_list { $1 }
  | init_list { $1 }

/* Refinements */
refinement_list:
  | { [] }
  | REFINES refinements { $2 }
refinements:
  | LBRACE refine_list RBRACE { $2 }
refine_list:
  | refmem { [$1] }
  | refine_list refmem { $2 :: $1 }
refmem:
  | TYPE ID DOT ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    { { returns = $1;
        host    = Some($2);
        name    = $4;
        static  = false;
        formals = $6;
        body    = List.rev $9 } }

/* Each class has an optional main */
main_opt:
  | { None }
  | MAIN LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    { Some({
        returns = None;
        host    = None;
        name    = "main";
        static  = true;
        formals = $3;
        body    = List.rev $6 }) }

/* Variable declaration */
vdecl_list:
  | { [] }
  | vdecl_list vdecl { $2 :: $1 }
vdecl:
  TYPE VAR { VarDef($2) }

/* Method declaration */
mdecl_list:
  | { [] }
  | mdecl_list mdecl { $2 :: $1 }
mdecl:
  TYPE ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
  { FuncMem({
      returns = Some($1);
      host    = None;
      name    = $2;
      static  = false;
      formals = $4;
      body    = List.rev $7; }) }

/* Constructors */
init_list:
  | { [] }
  | init_list init { $2 :: $1 }
init:
  | INIT LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    { FuncMem({
        returns = None;
        host    = None;
        name    = "init";
        static  = false;
        formals = $3;
        body    = $6; }) }

/* Parameter lists */
formals_opt:
  | { [] }
  | formals_list { List.rev $1 }

formals_list:
  | vdecl { [$1] }
  | formals_list COMMA vdecl { $3 :: $1 }
