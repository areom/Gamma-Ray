


typedef struct {
	ClassInfo *meta;

	struct {
		t_System *v_system;
	} Object;


	struct { BYTE empty_vars; } Boolean;
} t_Boolean ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_System *v_system;
	} Object;


	struct { BYTE empty_vars; } Float;
} t_Float ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_System *v_system;
	} Object;


	struct { BYTE empty_vars; } Integer;
} t_Integer ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_System *v_system;
	} Object;

} t_Object ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_System *v_system;
	} Object;


	struct { BYTE empty_vars; } Printer;
} t_Printer ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_System *v_system;
	} Object;


	struct { BYTE empty_vars; } Scanner;
} t_Scanner ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_System *v_system;
	} Object;


	struct { BYTE empty_vars; } String;
} t_String ;


typedef struct {
	ClassInfo *meta;

	struct {
		t_System *v_system;
	} Object;


	struct {
		t_Printer *v_err;
		t_Scanner *v_in;
		t_Printer *v_out;
	} System;

} t_System ;
