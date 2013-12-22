

/*
 * Structures for each of the objects.
 */
typedef struct {
	ClassInfo *meta;

	struct {
		t_Integer *v_obj_id;
		t_System *v_system;
	} Object;


	struct { unsigned char value; } Boolean;
} t_Boolean ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_Integer *v_obj_id;
		t_System *v_system;
	} Object;


	struct { double value; } Float;
} t_Float ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_Integer *v_obj_id;
		t_System *v_system;
	} Object;


	struct { int value; } Integer;
} t_Integer ;



typedef struct {
	ClassInfo *meta;

	struct {
		t_Integer *v_obj_id;
		t_System *v_system;
	} Object;

} t_Object ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_Integer *v_obj_id;
		t_System *v_system;
	} Object;


	struct { FILE *target; } Printer;
} t_Printer ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_Integer *v_obj_id;
		t_System *v_system;
	} Object;


	struct { FILE *source; } Scanner;
} t_Scanner ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_Integer *v_obj_id;
		t_System *v_system;
	} Object;


	struct { char *value; } String;
} t_String ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_Integer *v_obj_id;
		t_System *v_system;
	} Object;


	struct {
		t_Printer *v_err;
		t_Scanner *v_in;
		t_Printer *v_out;
	} System;

} t_System ;
