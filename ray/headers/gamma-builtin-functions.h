
/* Magic allocator. DO NOT INVOKE THIS, USE MAKE_NEW(TYPE)
 * where type is not prefixed (i.e. MAKE_NEW(Integer) not
 * MAKE_NEW(t_Integer))
 */
t_Object *allocate_for(size_t s, ClassInfo *meta) {
    t_Object *this = (t_Object *)(malloc(s));
    if (!this) {
        fprintf(stderr, "Could not even allocate memory. Exiting.\n");
        exit(1);
    }
    this->meta = meta;
    return this;
}

/* Make basic objects with the given values. */
t_Integer *integer_value(int in_i) {
    t_Integer *i = MAKE_NEW(Integer);
    i->Integer.value = in_i;
    return i;
}

t_Float *float_value(double in_f) {
    t_Float *f = MAKE_NEW(Float);
    f->Float.value = in_f;
    return f;
}

t_Boolean *bool_value(unsigned char in_b) {
    t_Boolean *b = MAKE_NEW(Boolean);
    b->Boolean.value = in_b;
    return b;
}

t_String *string_value(char *s_in) {
    size_t length = 0;
    char *dup = NULL;
    length = strlen(s) + 1;

    t_String *s = MAKE_NEW(String);
    dup = malloc(sizeof(char) * length);
    if (!dup) {
        fprintf(stderr, "Out of memory in string_value.\n");
        exit(1);
    }
    s->String.value = strcpy(dup, s);
    return s;
}

/* t_Boolean *boolean_init(t_Boolean *this) */
/* t_Float *float_init(t_Float *this) */
/* t_Integer *float_to_i(t_Float *this) */
/* t_Integer *integer_init(t_Integer *this) */
/* t_Float *integer_to_f(t_Integer *this) */
/* t_String *object_get_id(t_Object *this) */
/* t_Object *object_init(t_Object *this) */
t_Boolean *boolean_init(t_Boolean *this){
    this->Object = *object_init(&this->Object);
    this->Boolen.value = false;
    return this;
}

t_Float *float_init(t_Float *this){
    this->Object = *object_init(&this->Object);
    this->Float.value = 0.0;
    return this;
}

t_Integer *float_to_i(t_Float *this){
    t_Integer *new_int = MAKE_NEW(Integer);
    new_int = integer_init(new_int);
    new_int->Integer.value = (int)(this->Float.value);
    return new_int;
}

t_Float *integer_to_f(t_Integer *this){
    t_Float *new_float = MAKE_NEW(Float);
    new_float = float_init(new_float);
    new_float->Float.value = (float)(this->Integer.value);
    return new_float
}

t_Integer *integer_init(t_Integer *this){
    this->Object = *object_init(&this->Object);
    this->Intger.value = 0;
    return this;
}

t_String *object_get_id(t_Object *this){
Add a comment to this line
    return :wthis->Object.v_obj_id;
}

t_Object *object_init(t_Object *this){
    this->Object = *object_init(&this->Object);
    this->Object.v_system = system_init(this->Object.v_system);
    return this;
}
t_Printer *printer_init(t_Printer *this, t_Boolean *v_stdout)
{
    this->Object = *object_init(&this->Object);
    if(*v_stdout)
        this->Printer.target = stdout;
    else
        this->Printer.target = stderr;
    return this;
}
/* t_Scanner *scanner_init(t_Scanner *this) */
/* t_String *string_init(t_String *this) */
/* void system_exit(t_System *this, t_Integer *v_code) */
/* t_System *system_init(t_System *this) */


t_Float *scanner_scan_float(t_Scanner *this)
{
    double dval;
    fscanf(this->Scanner.source, "%ld", &dval);	

    t_Float *new_float = MAKE_NEW(Float);
    new_float = float_init(new_float);
    new_float->Float.value = dval;
}

t_Integer *scanner_scan_integer(t_Scanner *this)
{
    int ival;
    fscanf(this->Scanner.source, "%d", &val);

    t_Integer *new_int = MAKE_NEW(Integer);
    new_int = integer_init(new_int);
    new_int->Integer.value = ival;
}

t_String *scanner_scan_string(t_Scanner *this)
{
    int ret;
    char *inpstr;
    ret = getline(&inpstr, 0, this->Scanner.source);
    if(ret == -1) {
        fprintf(stderr, "Error in string input\n");
        exit(0);
    }
    t_String *new_str = MAKE_NEW(String);
    new_str = string_init(new_str);
    new_str->String.value = inpstr;
}

void printer_print_float(t_Printer *this, t_Float *v_arg)
{
    fprintf(this->Printer.target, "%ld\n", v_arg->Float.value);
}
void printer_print_integer(t_Printer *this, t_Integer *v_arg)
{
    fprintf(this->Printer.target, "%d\n", v_arg->Integer.value);
}
void printer_print_string(t_Printer *this, t_String *v_arg)
{
    fprintf(this->Printer.target, "%s\n", varg->String.value);
}
t_System global_system;
int obj_counter;
