/* Starting Build Process...
 * Reading Tokens...
 * Parsing Tokens...
 * Generating Global Data...
 * Using Normal KlassData Builder
 * Building Semantic AST...
 * Deanonymizing Anonymous Classes.
 * Rebinding refinements.
 * Generating C AST...
 * Generating C...
 */


/*
 * Passing over code to find dispatch data.
 */


/*
 * Gamma preamble -- macros and such needed by various things
 */
#include "gamma-preamble.h"



/*
 * Ancestry meta-info to link to later.
 */
char *m_classes[] = {
	"t_Boolean", "t_Float", "t_Integer", "t_Object", "t_Printer", "t_Scanner",
	"t_String", "t_System", "t_Test"
};


/*
 * Enums used to reference into ancestry meta-info strings.
 */
enum m_class_idx {
	T_BOOLEAN = 0, T_FLOAT, T_INTEGER, T_OBJECT, T_PRINTER, T_SCANNER, T_STRING,
	T_SYSTEM, T_TEST
};


/*
 * Header file containing meta information for built in classes.
 */
#include "gamma-builtin-meta.h"



/*
 * Meta structures for each class.
 */
ClassInfo M_Test;

void init_class_infos() {
	init_built_in_infos();
	class_info_init(&M_Test, 2, m_classes[T_OBJECT], m_classes[T_TEST]);
}



/*
 * Header file containing structure information for built in classes.
 */
#include "gamma-builtin-struct.h"



/*
 * Structures for each of the objects.
 */
struct t_Test {
	ClassInfo *meta;

	struct {
		struct t_System *v_system;
	} Object;


	struct { BYTE empty_vars; } Test;
};




/*
 * Header file containing information regarding built in functions.
 */
#include "gamma-builtin-functions.h"



/*
 * All of the function prototypes we need to do magic.
 */
struct t_Test *f_00000001_init(struct t_Test *);
void f_00000002_main(struct t_System *, struct t_String **);


/*
 * All the dispatching functions we need to continue the magic.
 */


/*
 * Array allocators also do magic.
 */


/*
 * All of the functions we need to run the program.
 */
/* Place-holder for struct t_Boolean *boolean_init(struct t_Boolean *this) */
/* Place-holder for struct t_Float *float_init(struct t_Float *this) */
/* Place-holder for struct t_Integer *float_to_i(struct t_Float *this) */
/* Place-holder for struct t_Integer *integer_init(struct t_Integer *this) */
/* Place-holder for struct t_Float *integer_to_f(struct t_Integer *this) */
/* Place-holder for struct t_Object *object_init(struct t_Object *this) */
/* Place-holder for struct t_Printer *printer_init(struct t_Printer *this, struct t_Boolean *v_stdout) */
/* Place-holder for void printer_print_float(struct t_Printer *this, struct t_Float *v_arg) */
/* Place-holder for void printer_print_integer(struct t_Printer *this, struct t_Integer *v_arg) */
/* Place-holder for void printer_print_string(struct t_Printer *this, struct t_String *v_arg) */
/* Place-holder for struct t_Scanner *scanner_init(struct t_Scanner *this) */
/* Place-holder for struct t_Float *scanner_scan_float(struct t_Scanner *this) */
/* Place-holder for struct t_Integer *scanner_scan_integer(struct t_Scanner *this) */
/* Place-holder for struct t_String *scanner_scan_string(struct t_Scanner *this) */
/* Place-holder for struct t_String *string_init(struct t_String *this) */
/* Place-holder for void system_exit(struct t_System *this, struct t_Integer *v_code) */
/* Place-holder for struct t_System *system_init(struct t_System *this) */

struct t_Test *f_00000001_init(struct t_Test *this)
{
	object_init((struct t_Object *)(this));
	return ( this );
}


void f_00000002_main(struct t_System *v_sys, struct t_String **v_args)
{
	struct t_Integer *v_i = ((struct t_Integer *)(LIT_INT(0)));
	struct t_Printer *v_p = ((struct t_Printer *)((v_sys)->System.v_out));
	while ( BOOL_OF( NTEST_LESS_INT_INT( v_i , (v_sys)->System.v_argc ) ) ) {
		( printer_print_string(((struct t_Printer *)(v_p)), LIT_STRING("arg[")) );
		( printer_print_integer(((struct t_Printer *)(v_p)), v_i) );
		( printer_print_string(((struct t_Printer *)(v_p)), LIT_STRING("] = ")) );
		( printer_print_string(((struct t_Printer *)(v_p)), ((struct t_String **)(v_args))[INTEGER_OF((v_i))]) );
		( printer_print_string(((struct t_Printer *)(v_p)), LIT_STRING("\n")) );
		( v_i = ((struct t_Integer *)(ADD_INT_INT( v_i , LIT_INT(1) ))) );
	}
}



/*
 * Dispatch looks like this.
 */


/*
 * Array allocators.
 */


/*
 * The main.
 */
#define CASES "Test"

int main(int argc, char **argv) {
	INIT_MAIN(CASES)
	if (!strncmp(gmain, "Test", 5)) { f_00000002_main(&global_system, str_args); return 0; }
	FAIL_MAIN(CASES)
	return 1;
}

