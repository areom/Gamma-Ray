/* Starting Build Process...
 * Reading Tokens...
 * Parsing Tokens...
 * Generating Global Data...
 * Using Normal KlassData Builder
 * Building Semantic AST...
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
	"t_Account", "t_Boolean", "t_Float", "t_Integer", "t_Main", "t_Object", "t_Printer",
	"t_Scanner", "t_String", "t_System"
};


/*
 * Enums used to reference into ancestry meta-info strings.
 */
enum m_class_idx {
	T_ACCOUNT = 0, T_BOOLEAN, T_FLOAT, T_INTEGER, T_MAIN, T_OBJECT, T_PRINTER,
	T_SCANNER, T_STRING, T_SYSTEM
};


/*
 * Header file containing meta information for built in classes.
 */
#include "gamma-builtin-meta.h"



/*
 * Meta structures for each class.
 */
ClassInfo M_Account;
ClassInfo M_Main;

void init_class_infos() {
	init_built_in_infos();
	class_info_init(&M_Account, 2, m_classes[T_OBJECT], m_classes[T_ACCOUNT]);
	class_info_init(&M_Main, 2, m_classes[T_OBJECT], m_classes[T_MAIN]);
}



/*
 * Header file containing structure information for built in classes.
 */
#include "gamma-builtin-struct.h"



/*
 * Structures for each of the objects.
 */
struct t_Account {
	ClassInfo *meta;

	struct {
		struct t_System *v_system;
	} Object;


	struct {
		struct t_Integer *v_balance;
		struct t_String *v_customer;
		struct t_Integer *v_id;
		struct t_Integer *v_trans_len;
	} Account;

};


struct t_Main {
	ClassInfo *meta;

	struct {
		struct t_System *v_system;
	} Object;


	struct { BYTE empty_vars; } Main;
};




/*
 * Header file containing information regarding built in functions.
 */
#include "gamma-builtin-functions.h"



/*
 * All of the function prototypes we need to do magic.
 */
struct t_Account *f_00000001_init(struct t_Account *, struct t_Integer *, struct t_String *);
struct t_Integer *f_00000002_get_id(struct t_Account *);
struct t_String *f_00000003_get_customer_name(struct t_Account *);
struct t_Integer *f_00000004_get_balance(struct t_Account *);
struct t_Integer *f_00000005_deposit(struct t_Account *, struct t_Integer *);
struct t_Integer *f_00000006_withdraw(struct t_Account *, struct t_Integer *);
struct t_Main *f_00000007_init(struct t_Main *);
void f_00000008_main(struct t_System *, struct t_String **);


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

struct t_Account *f_00000001_init(struct t_Account *this, struct t_Integer *v_new_id, struct t_String *v_name)
{
	object_init((struct t_Object *)(this));
	( (this->Account).v_id = ((struct t_Integer *)(v_new_id)) );
	( (this->Account).v_customer = ((struct t_String *)(v_name)) );
	( (this->Account).v_balance = ((struct t_Integer *)(LIT_INT(0))) );
	( (this->Account).v_trans_len = ((struct t_Integer *)(LIT_INT(0))) );
	return ( this );
}


struct t_Integer *f_00000002_get_id(struct t_Account *this)
{
	return ( (this->Account).v_id );
}


struct t_String *f_00000003_get_customer_name(struct t_Account *this)
{
	return ( (this->Account).v_customer );
}


struct t_Integer *f_00000004_get_balance(struct t_Account *this)
{
	return ( (this->Account).v_balance );
}


struct t_Integer *f_00000005_deposit(struct t_Account *this, struct t_Integer *v_amount)
{
	if ( BOOL_OF( NTEST_LESS_INT_INT( v_amount , LIT_INT(0) ) ) ) {
		return ( LIT_INT(1) );
	}
	( (this->Account).v_balance = ((struct t_Integer *)(ADD_INT_INT( (this->Account).v_balance , v_amount ))) );
	( (this->Account).v_trans_len = ((struct t_Integer *)(ADD_INT_INT( (this->Account).v_trans_len , LIT_INT(1) ))) );
	return ( LIT_INT(0) );
}


struct t_Integer *f_00000006_withdraw(struct t_Account *this, struct t_Integer *v_amount)
{
	if ( BOOL_OF( NTEST_LESS_INT_INT( v_amount , LIT_INT(0) ) ) ) {
		return ( LIT_INT(1) );
	}
	if ( BOOL_OF( NTEST_LESS_INT_INT( (this->Account).v_balance , v_amount ) ) ) {
		return ( LIT_INT(1) );
	}
	( (this->Account).v_balance = ((struct t_Integer *)(SUB_INT_INT( (this->Account).v_balance , v_amount ))) );
	return ( LIT_INT(0) );
}


struct t_Main *f_00000007_init(struct t_Main *this)
{
	object_init((struct t_Object *)(this));
	return ( this );
}


void f_00000008_main(struct t_System *v_system, struct t_String **v_args)
{
	struct t_Account *v_test_account = ((struct t_Account *)(f_00000001_init(MAKE_NEW(Account), LIT_INT(0), LIT_STRING("Bender"))));
	( printer_print_string((v_system)->System.v_out, LIT_STRING("Current Balance:")) );
	( printer_print_integer((v_system)->System.v_out, f_00000004_get_balance(v_test_account)) );
	( printer_print_string((v_system)->System.v_out, LIT_STRING("\n")) );
	( f_00000005_deposit(v_test_account, NEG_INTEGER( LIT_INT(100) )) );
	( printer_print_string((v_system)->System.v_out, LIT_STRING("Current Balance:")) );
	( printer_print_integer((v_system)->System.v_out, f_00000004_get_balance(v_test_account)) );
	( printer_print_string((v_system)->System.v_out, LIT_STRING("\n")) );
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
#define CASES "Main"

int main(int argc, char **argv) {
	INIT_MAIN(CASES)
	if (!strncmp(gmain, "Main", 5)) { f_00000008_main(&global_system, str_args); return 0; }
	FAIL_MAIN(CASES)
	return 1;
}

