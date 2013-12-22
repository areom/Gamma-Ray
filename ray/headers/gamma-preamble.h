#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BYTE unsigned char

#define PROMOTE_INTEGER(ival)   integer_value((ival))
#define PROMOTE_FLOAT(fval)     float_value((fval))
#define PROMOTE_STRING(sval)    string_value((sval))
#define PROMOTE_BOOL(bval)      bool_value((bval))

#define LIT_INT(lit_int)        PROMOTE_INTEGER(lit_int)
#define LIT_FLOAT(lit_flt)      PROMOTE_FLOAT(lit_flt)
#define LIT_STRING(lit_str)     PROMOTE_STRING(lit_str)
#define LIT_BOOL(lit_bool)      PROMOTE_BOOL(lit_bool)

#define ADD_INT_INT(l, r)       PROMOTE_INTEGER(INTEGER_OF(l) + INTEGER_OF(r))
#define ADD_FLOAT_FLOAT(l, r)   PROMOTE_FLOAT(FLOAT_OF(l) + FLOAT_OF(r))
#define SUB_INT_INT(l, r)       PROMOTE_INTEGER(INTEGER_OF(l) - INTEGER_OF(r))
#define SUB_FLOAT_FLOAT(l, r)   PROMOTE_FLOAT(FLOAT_OF(l) - FLOAT_OF(r))
#define PROD_INT_INT(l, r)      PROMOTE_INTEGER(INTEGER_OF(l) * INTEGER_OF(r))
#define PROD_FLOAT_FLOAT(l, r)  PROMOTE_FLOAT(FLOAT_OF(l) * FLOAT_OF(r))
#define DIV_INT_INT(l, r)       PROMOTE_INTEGER(INTEGER_OF(l) / INTEGER_OF(r))
#define DIV_FLOAT_FLOAT(l, r)   PROMOTE_FLOAT(FLOAT_OF(l) / FLOAT_OF(r))
#define MOD_INT_INT(l, r)       PROMOTE_INTEGER(INTEGER_OF(l) % INTEGER_OF(r))
#define POW_INT_INT(l, r)       PROMOTE_INTEGER(( (int)pow(INTEGER_OF(l), INTEGER_OF(r)) ))
#define POW_FLOAT_FLOAT(l, r)   PROMOTE_FLOAT( pow(FLOAT_OF(l), FLOAT_OF(r)) )

#define MAKE_NEW2(type, meta) ((struct type *)(allocate_for(sizeof(struct type), &meta)))
#define MAKE_NEW(t_name) MAKE_NEW2(t_##t_name, M_##t_name)

#define CAST(type, v) ( (struct t_##type *)(v) )
#define VAL_OF(type, v) ( CAST(type, v)->type.value )
#define BOOL_OF(b)    VAL_OF(Boolean, b)
#define FLOAT_OF(f)   VAL_OF(Float, f)
#define INTEGER_OF(i) VAL_OF(Integer, i)
#define STRING_OF(s)  VAL_OF(String, s)

#define NEG_INTEGER(i)            PROMOTE_INTEGER(-INTEGER_OF(i))
#define NEG_FLOAT(f)              PROMOTE_FLOAT(-FLOAT_OF(f))
#define NOT_BOOLEAN(b)            PROMOTE_BOOL(!BOOL_OF(b))

#define BINOP(type, op, l, r)     PROMOTE_BOOL( VAL_OF(type, l) op VAL_OF(type, r) )
#define IBINOP(op, l, r)          BINOP(Integer, op, l, r)
#define FBINOP(op, l, r)          BINOP(Float, op, l, r)
#define BBINOP(op, l, r)          BINOP(Boolean, op, l, r)

#define NTEST_EQ_INT_INT(l, r)    IBINOP(==, l, r)
#define NTEST_NEQ_INT_INT(l, r)   IBINOP(!=, l, r)
#define NTEST_LESS_INT_INT(l, r)  IBINOP(<, l, r)
#define NTEST_GRTR_INT_INT(l, r)  IBINOP(>, l, r)
#define NTEST_LEQ_INT_INT(l, r)   IBINOP(<=, l, r)
#define NTEST_GEQ_INT_INT(l, r)   IBINOP(>=, l, r)

#define NTEST_EQ_FLOAT_FLOAT(l, r)    FBINOP(==, l, r)
#define NTEST_NEQ_FLOAT_FLOAT(l, r)   FBINOP(!=, l, r)
#define NTEST_LESS_FLOAT_FLOAT(l, r)  FBINOP(<, l, r)
#define NTEST_GRTR_FLOAT_FLOAT(l, r)  FBINOP(>, l, r)
#define NTEST_LEQ_FLOAT_FLOAT(l, r)   FBINOP(<=, l, r)
#define NTEST_GEQ_FLOAT_FLOAT(l, r)   FBINOP(>=, l, r)

#define IS_CLASS(obj, kname) ( strcmp((obj)->meta->ancestors[obj->meta->generation], (kname)) == 0 )

#define INIT_MAIN(options) \
struct t_String **str_args = NULL; \
char *gmain = NULL; \
--argc; ++argv; \
if (!argc) { \
    fprintf(stderr, "Please select a main to use.  Available options: " options "\n"); \
    exit(1); \
}\
gmain = *argv;\
system_init(&global_system);

#define FAIL_MAIN(options) \
fprintf(stderr, "None of the available options were selected. Options were: " options "\n"); \
exit(1);
