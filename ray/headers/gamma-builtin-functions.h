/* 
	Initializes the given ClassInfo
*/
int class_info_init(ClassInfo* meta, int num_args, char* objtypes[]) {

	int i;
	meta->ancestors = malloc(sizeof(char *) * num_args);

	if (meta->ancestors == NULL) {
		return -1;
	}
	
	for(i = 0; i < num_args; i++) {
		meta->ancestors[i] = objtypes[i];
	}

	meta->generation = num_args - 1;	
	meta->class = meta->ancestors[meta->generation];
	return 0;
}
/* t_Boolean *boolean_init(t_Boolean *this) */
/* t_Float *float_init(t_Float *this) */
/* t_Integer *float_to_i(t_Float *this) */
/* t_Integer *integer_init(t_Integer *this) */
/* t_Float *integer_to_f(t_Integer *this) */
/* t_String *object_get_id(t_Object *this) */
/* t_Object *object_init(t_Object *this) */
/* t_Printer *printer_init(t_Printer *this, t_Boolean *v_stdout) */
/* void printer_print_float(t_Printer *this, t_Float *v_arg) */
/* void printer_print_integer(t_Printer *this, t_Integer *v_arg) */
/* void printer_print_string(t_Printer *this, t_String *v_arg) */
/* t_Scanner *scanner_init(t_Scanner *this) */
/* t_Float *scanner_scan_float(t_Scanner *this) */
/* t_Integer *scanner_scan_integer(t_Scanner *this) */
/* t_String *scanner_scan_string(t_Scanner *this) */
/* t_String *string_init(t_String *this) */
/* void system_exit(t_System *this, t_Integer *v_code) */
/* t_System *system_init(t_System *this) */
