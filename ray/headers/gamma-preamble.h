#define LIT_INT(lit_int)
#define LIT_FLOAT(lit_flt)
#define LIT_STRING(lit_str)
#define LIT_BOOL(lit_bool)

#define ADD_INT_INT(l, r)
#define ADD_FLOAT_FLOAT(l, r)
#define SUB_INT_INT(l, r)
#define SUB_FLOAT_FLOAT(l, r)
#define PROD_INT_INT(l, r)
#define PROD_FLOAT_FLOAT(l, r)
#define DIV_INT_INT(l, r)
#define DIV_FLOAT_FLOAT(l, r)
#define MOD_INT_INT(l, r)
#define POW_INT_INT(l, r)
#define POW_FLOAT_FLOAT(l, r)


#define MAKE_NEW2(type, meta) ((type *)(allocate_for(sizeof(type), &meta)))
#define MAKE_NEW(t_name) MAKE_NEW2(T_#t_name, M_#t_name)

#define VAL_OF(type, v) ( ((t_#type *)(v))->type.value )
#define BOOL_OF(b)    VAL_OF(Boolean, b)
#define FLOAT_OF(f)   VAL_OF(Float, f)
#define INTEGER_OF(i) VAL_OF(Integer, i)
#define STRING_OF(s)  VAL_OF(String, s)

#define IS_CLASS(obj, kname) ( strcmp((obj)->meta->ancestors[obj->meta->generation], (kname)) == 0 )
