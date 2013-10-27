%{ open Ast %}

%token LPAREN RPAREN LBRACKET RBRACKET COMMA LBRACE RBRACE
%token PLUS MINUS TIMES DIVIDE MOD POWER
%token EQ NEQ GT LT GEQ LEQ AND OR NAND NOR XOR NOT TRUE FALSE
%token IF ELSE ELSIF WHILE
%token ASSIGN RETURN CLASS EXTEND SUPER INIT PRIVATE PROTECTED PUBLIC
%token NULL VOID THIS
%token NEW MAIN
%token REFINABLE REFINE REFINES TO
%token SEMI COMMA DOT EOF

%token <string> TYPE
%token <int> ILIT
%token <float> FLIT
%token <bool> BLIT
%token <string> SLIT
%token <string> ID

/* Want to work on associtivity when I'm a bit fresher */
%right ASSIGN
%left OR NOR XOR
%left AND NAND
%left EQ NEQ /* Equality operators: ==, != */
%left LT GT LEQ GEQ /* Comparison operators: <, >, <=, => */
%left PLUS MINUS /* Additive operators: +, - */
%left TIMES DIVIDE MOD /* Multiplicative operators: *, / */
%left NOT
%left POWER
%right LPAREN LBRACKET
%left RPAREN RBRACKET
%left DOT

%start cdecl /* The start symbol */
%type <Ast.class_def> cdecl


%%

/* Class and subclassing -- cdecl / extend_opt */
cdecl:
  | CLASS TYPE extend_opt class_section_list
    { { klass     = $2;
        parent    = $3;
        sections  = $4  } }
extend_opt:
  | /* default */  { None }
  | EXTEND TYPE    { Some($2) }

/* Class sections */
class_section_list:
  | LBRACE class_sections RBRACE  { $2 }
class_sections:
  | /* Base Case */
    { { privates = [];
        protects = [];
        publics  = [];
        refines  = [];
        mains    = [] } }
  | class_sections private_list  { { $1 with privates = $2 @  $1.privates } }
  | class_sections protect_list  { { $1 with protects = $2 @  $1.protects } }
  | class_sections public_list   { { $1 with publics  = $2 @  $1.publics  } }
  | class_sections refine_list   { { $1 with refines  = $2 @  $1.refines  } }
  | class_sections main_method   { { $1 with mains    = $2 :: $1.mains    } }

/* Refinements */
refine_list:
  | REFINES LBRACE refinements RBRACE { $3 }
refinements:
  | /* Can be empty */      { [] }
  | refinements refinement  { $2 :: $1 }
refinement:
  | TYPE ID DOT ID formals stmt_block
    { { returns = Some($1);
        host    = Some($2);
        name    = $4;
        static  = false;
        formals = $5;
        body    = $6 } }

/* Private, protected, public members */
private_list:
  | PRIVATE member_list    { $2 }
protect_list:
  | PROTECTED member_list  { $2 }
public_list:
  | PUBLIC member_list     { $2 }

/* Members of such access groups */
member_list:
  | LBRACE members RBRACE  { $2 }
members:
  | { [] }
  | members member  { $2 :: $1 }
member:
  | vdecl semi  { VarMem($1)    }
  | mdecl       { MethodMem($1) }
  | init        { InitMem($1)   }

/* Methods */
mdecl:
  | TYPE ID formals stmt_block
    { { returns = Some($1);
        host    = None;
        name    = $2;
        static  = false;
        formals = $3;
        body    = $4 } }
  | VOID ID formals stmt_block
    { { returns = None;
        host    = None;
        name    = $2;
        static  = false;
        formals = $3;
        body    = $4 } }

/* Constructors */
init:
  | INIT formals stmt_block
    { { returns = None;
        host    = None;
        name    = "init";
        static  = false;
        formals = $2;
        body    = $3 } }

/* Each class has an optional main */
main_method:
  | MAIN formals stmt_block
    { { returns = None;
        host    = None;
        name    = "main";
        static  = true;
        formals = $2;
        body    = $3 } }

/* Statements */
stmt_block:
  | LBRACE stmt_list RBRACE  { List.rev $2 }
stmt_list:
  | /* nada */      { [] }
  | stmt_list stmt  { $2 :: $1 }
stmt:
  | vdecl semi              { Decl($1) }
  | SUPER actuals semi      { Super($2) }
  | RETURN expr semi        { Expr($2) }
  | conditional             { $1 }
  | loop                    { $1 }
  | expr semi               { Expr($1) }

/* Control Flow */
conditional:
  | IF pred stmt_block else_list  { If((Some($2), $3) :: $4 }
else_list:
  | /* nada */                         { [] }
  | ELSE stmt_block                    { [(None, $2)] }
  | ELSIF pred stmt_block else_list    { (Some($2), $3) :: $4 }
loop:
  | WHILE pred stmt_block  { While($2, $3) }
pred:
  | LPAREN expr RPAREN  { $2 }


/* Expressions */
expr:
  | assignment          { $1 }
  | invocation          { $1 }
  | field               { $1 }
  | value               { $1 }
  | arithmetic          { $1 }
  | test                { $1 }
  | instantiate         { $1 }
  | refineexpr          { $1 }
  | literal             { $1 }
  | LPAREN expr RPAREN  { $2 }
  | THIS                { This }
  | NULL                { Null }

assignment:
  | expr ASSIGN expr  { Assign($1, $3) }

invocation:
  | expr DOT ID actuals { Invoc($1, $3, $4) }
  | ID actuals { Invoc(This, $1, $2) }

field:
  | expr DOT ID  { Field($1, $3) }

value:
  | ID    { Id($1) }
  | expr LBRACKET expr RBRACKET  { Deref($1, $3) }

arithmetic:
  | expr PLUS expr    { Binop($1, Arithmetic(Add), $3) }
  | expr MINUS expr   { Binop($1, Arithmetic(Sub), $3) }
  | expr TIMES expr   { Binop($1, Arithmetic(Prod), $3) }
  | expr DIVIDE expr  { Binop($1, Arithmetic(Div), $3) }
  | expr MOD expr     { Binop($1, Arithmetic(Mod), $3) }
  | expr POWER expr   { Binop($1, Arithmetic(Pow), $3) }

test:
  | expr EQ expr   { Binop($1, NumTest(Eq), $3) }
  | expr NEQ expr  { Binop($1, NumTest(Neq), $3) }
  | expr LT expr   { Binop($1, NumTest(Less), $3) }
  | expr LEQ expr  { Binop($1, NumTest(Leq), $3) }
  | expr GT expr   { Binop($1, NumTest(Grtr), $3) }
  | expr GEQ expr  { Binop($1, NumTest(Geq), $3) }
  | NOT expr       { Unop(CombTest(Not), $2) }
  | REFINABLE LPAREN ID RPAREN { Refinable($3) }

instantiate:
  | NEW TYPE                         { NewObj($2, []) }
  | NEW TYPE actuals                 { NewObj($2, $3) }
  | NEW TYPE LBRACKET expr RBRACKET  { NewArr($2, $4) }

refineexpr:
  | REFINE ID actuals TO TYPE  { Refine($2, $3, $5) }

literal:
  | lit  { Literal($1) }

/* Literally necessary */
lit:
  | SLIT { String($1) }
  | ILIT { Int($1) }
  | FLIT { Float($1) }
  | BLIT { Bool($1) }

/* Parameter lists */
formals:
  | LPAREN formals_opt RPAREN  { $2 }
formals_opt:
  | { [] }
  | formals_list  { List.rev $1 }
formals_list:
  | vdecl  { [$1] }
  | formals_list COMMA vdecl  { $3 :: $1 }

/* Arguments */
actuals:
  | LPAREN actuals_opt RPAREN { $2 }
actuals_opt:
  | { [] }
  | actuals_list { List.rev $1 }
actuals_list:
  | expr { [$1] }
  | actuals_list COMMA expr { $3 :: $1}

/* Variable declaration */
vdecl:
  | TYPE ID { ($1, $2) }

/* Eat multiple semis */
semi:
  | SEMI  {}
  | semi SEMI  {}
