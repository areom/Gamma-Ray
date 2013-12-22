#define PROMOTE_INTEGER(ival)   integer_value((ival))
#define PROMOTE_FLOAT(fval)     float_value((fval))
#define PROMOTE_STRING(sval)    string_value((sval))
#define PROMOTE_BOOL(bval)      bool_value((bval))

#define LIT_INT(lit_int)        PROMOTE_INTEGER(lit_int)
#define LIT_FLOAT(lit_flt)      PROMOTE_FLOAT(lit_flt)
#define LIT_STRING(lit_str)     PROMOTE_STRING(lit_str)
#define LIT_BOOL(lit_bool)      PROMOTE_BOOL(list_bool)

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


#define MAKE_NEW2(type, meta) ((type *)(allocate_for(sizeof(type), &meta)))
#define MAKE_NEW(t_name) MAKE_NEW2(T_#t_name, M_#t_name)

#define VAL_OF(type, v) ( ((t_#type *)(v))->type.value )
#define BOOL_OF(b)    VAL_OF(Boolean, b)
#define FLOAT_OF(f)   VAL_OF(Float, f)
#define INTEGER_OF(i) VAL_OF(Integer, i)
#define STRING_OF(s)  VAL_OF(String, s)

#define IS_CLASS(obj, kname) ( strcmp((obj)->meta->ancestors[obj->meta->generation], (kname)) == 0 )
