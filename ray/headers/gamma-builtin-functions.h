
/* GLOBAL DATA */
struct t_System global_system;
int object_counter;
int global_argc;

/* Prototypes */
struct t_Object *allocate_for(size_t, ClassInfo *);
void *array_allocator(size_t, int);
struct t_Integer *integer_value(int);
struct t_Float *float_value(double);
struct t_Boolean *bool_value(unsigned char);
struct t_String *string_value(char *);
struct t_Boolean *boolean_init(struct t_Boolean *);
struct t_Integer *integer_init(struct t_Integer *);
struct t_Float *float_init(struct t_Float *);
struct t_Object *object_init(struct t_Object *);
struct t_String *string_init(struct t_String *);
struct t_Printer *printer_init(struct t_Printer *, struct t_Boolean *);
struct t_Scanner *scanner_init(struct t_Scanner *);
struct t_Integer *float_to_i(struct t_Float *);
struct t_Float *integer_to_f(struct t_Integer *);
struct t_Float *scanner_scan_float(struct t_Scanner *);
struct t_Integer *scanner_scan_integer(struct t_Scanner *);
struct t_String *scanner_scan_string(struct t_Scanner *);
void printer_print_float(struct t_Printer *, struct t_Float *);
void printer_print_integer(struct t_Printer *, struct t_Integer *);
void printer_print_string(struct t_Printer *, struct t_String *);
struct t_String **get_gamma_args(char **argv, int argc);

/* Functions! */

/* Magic allocator. DO NOT INVOKE THIS, USE MAKE_NEW(TYPE)
 * where type is not prefixed (i.e. MAKE_NEW(Integer) not
 * MAKE_NEW(t_Integer))
 */
struct t_Object *allocate_for(size_t s, ClassInfo *meta) {
    struct t_Object *this = (struct t_Object *)(malloc(s));
    if (!this) {
        fprintf(stderr, "Could not even allocate memory. Exiting.\n");
        exit(1);
    }
    this->meta = meta;
    return this;
}

void *array_allocator(size_t size, int n) {
    void *mem = malloc(size * n);
    if (!mem) {
        fprintf(stderr, "Failure allocating for array.  Exiting.\n");
        exit(1);
    }
    memset(mem, 0, size * n);
    return mem;
}

/* Make basic objects with the given values. */
struct t_Integer *integer_value(int in_i) {
    struct t_Integer *i = MAKE_NEW(Integer);
    i = integer_init(i);
    i->Integer.value = in_i;
    return i;
}

struct t_Float *float_value(double in_f) {
    struct t_Float *f = MAKE_NEW(Float);
    f = float_init(f);
    f->Float.value = in_f;
    return f;
}

struct t_Boolean *bool_value(unsigned char in_b) {
    struct t_Boolean *b = MAKE_NEW(Boolean);
    b = boolean_init(b);
    b->Boolean.value = in_b;
    return b;
}

struct t_String *string_value(char *s_in) {
    size_t length = 0;
    char *dup = NULL;
    length = strlen(s_in) + 1;

    struct t_String *s = MAKE_NEW(String);
    s = string_init(s);
    dup = malloc(sizeof(char) * length);
    if (!dup) {
        fprintf(stderr, "Out of memory in string_value.\n");
        exit(1);
    }
    s->String.value = strcpy(dup, s_in);
    return s;
}

struct t_Boolean *boolean_init(struct t_Boolean *this){
    object_init((struct t_Object *)(this));
    this->Boolean.value = 0;
    return this;
}

struct t_Integer *integer_init(struct t_Integer *this){
    object_init((struct t_Object *)(this));
    this->Integer.value = 0;
    return this;
}

struct t_Float *float_init(struct t_Float *this){
    object_init((struct t_Object *)(this));
    this->Float.value = 0.0;
    return this;
}

struct t_Object *object_init(struct t_Object *this){
    this->Object.v_system = &global_system;
    return this;
}

struct t_String *string_init(struct t_String *this)
{
    object_init((struct t_Object *)(this));
    this->String.value = NULL;
    return this;
}

struct t_System *system_init(struct t_System *this)
{
    this->System.v_err = MAKE_NEW(Printer);
    this->System.v_in = MAKE_NEW(Scanner);
    this->System.v_out = MAKE_NEW(Printer);
    this->System.v_argc = MAKE_NEW(Integer);

    this->System.v_err->Printer.target = stderr;
    this->System.v_in->Scanner.source = stdin;
    this->System.v_out->Printer.target = stdout;
    this->System.v_argc->Integer.value = global_argc;
    this->Object.v_system =
        this->System.v_err->Object.v_system =
        this->System.v_in->Object.v_system =
        this->System.v_out->Object.v_system =
        this->System.v_argc->Object.v_system = this;
    return this;
};

struct t_Printer *printer_init(struct t_Printer *this, struct t_Boolean *v_stdout)
{
    object_init((struct t_Object *)(this));
    this->Printer.target = v_stdout->Boolean.value ? stdout : stderr;
    return this;
}

struct t_Scanner *scanner_init(struct t_Scanner *this)
{
    object_init((struct t_Object *)(this));
    this->Scanner.source = stdin;
}

struct t_Integer *float_to_i(struct t_Float *this){
    return integer_value((int)(this->Float.value));
}

struct t_Float *integer_to_f(struct t_Integer *this){
    return float_value((double)(this->Integer.value));
}

struct t_Float *scanner_scan_float(struct t_Scanner *this)
{
    double dval;
    fscanf(this->Scanner.source, "%lf", &dval);
    return float_value(dval);
}

struct t_Integer *scanner_scan_integer(struct t_Scanner *this)
{
    int ival;
    fscanf(this->Scanner.source, "%d", &ival);
    return integer_value(ival);
}

struct t_String *scanner_scan_string(struct t_Scanner *this)
{
    int ret = -1;
    char *inpstr = NULL;
    struct t_String *astring = NULL;
    ret = getline(&inpstr, 0, this->Scanner.source);
    if(ret == -1) {
        fprintf(stderr, "Error in string input\n");
        exit(0);
    }
    astring = string_value(inpstr);
    free(astring);
    return astring;
}

void printer_print_float(struct t_Printer *this, struct t_Float *v_arg)
{
    fprintf(this->Printer.target, "%lf", v_arg->Float.value);
}

void printer_print_integer(struct t_Printer *this, struct t_Integer *v_arg)
{
    fprintf(this->Printer.target, "%d", v_arg->Integer.value);
}

void printer_print_string(struct t_Printer *this, struct t_String *v_arg)
{
    fprintf(this->Printer.target, "%s", v_arg->String.value);
}

void system_exit(struct t_System *this, struct t_Integer *v_code) {
    exit(INTEGER_OF(v_code));
}


struct t_String **get_gamma_args(char **argv, int argc) {
    struct t_String **args = NULL;
    int i = 0;

    if (!argc) return NULL;
    args = ONE_DIM_ALLOC(struct t_String *, argc);
    for (i = 0; i < argc; ++i)
        args[i] = string_value(argv[i]);
    args[i] = NULL;

    return args;
}
