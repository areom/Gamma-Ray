

/*
 * Structures for each of the objects.
 */
struct t_Boolean;
struct t_Float;
struct t_Integer;
struct t_Object;
struct t_Printer;
struct t_Scanner;
struct t_String;
struct t_System;


struct t_Boolean {
	ClassInfo *meta;

	struct {
		struct t_Integer *v_obj_id;
		struct t_System *v_system;
	} Object;


	struct { unsigned char value; } Boolean;
};


struct t_Float {
	ClassInfo *meta;

	struct {
		struct t_Integer *v_obj_id;
		struct t_System *v_system;
	} Object;


	struct { double value; } Float;
};


struct t_Integer {
	ClassInfo *meta;

	struct {
		struct t_Integer *v_obj_id;
		struct t_System *v_system;
	} Object;


	struct { int value; } Integer;
};


struct t_Object {
	ClassInfo *meta;

	struct {
		struct t_Integer *v_obj_id;
		struct t_System *v_system;
	} Object;
};


struct t_Printer {
	ClassInfo *meta;

	struct {
		struct t_Integer *v_obj_id;
		struct t_System *v_system;
	} Object;


	struct { FILE *target; } Printer;
};


struct t_Scanner {
	ClassInfo *meta;

	struct {
		struct t_Integer *v_obj_id;
		struct t_System *v_system;
	} Object;


	struct { FILE *source; } Scanner;
};


struct t_String {
	ClassInfo *meta;

	struct {
		struct t_Integer *v_obj_id;
		struct t_System *v_system;
	} Object;


	struct { char *value; } String;
};


struct t_System {
	ClassInfo *meta;

	struct {
		struct t_Integer *v_obj_id;
		struct t_System *v_system;
	} Object;


	struct {
		struct t_Printer *v_err;
		struct t_Scanner *v_in;
		struct t_Printer *v_out;
	} System;
};
