#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct {
        int generation;
        char* class;
        char** ancestors;
} ClassInfo;


ClassInfo M_Boolean;
ClassInfo M_Float;
ClassInfo M_Integer;
ClassInfo M_Object;
ClassInfo M_Printer;
ClassInfo M_Scanner;
ClassInfo M_String;
ClassInfo M_System;


/*
        Initializes the given ClassInfo
*/
void class_info_init(ClassInfo* meta, int num_args, ...) {

        int i;
        va_list objtypes;
        va_start(objtypes, num_args);

        meta->ancestors = malloc(sizeof(char *) * num_args);

        if (meta->ancestors == NULL) {
                printf("\nMemory error - class_info_init failed\n");
                exit(0);
        }
        for(i = 0; i < num_args; i++) {
                meta->ancestors[i] = va_arg(objtypes, char * );
        }
        meta->generation = num_args - 1;
        meta->class = meta->ancestors[meta->generation];
        va_end(objtypes);
}


void init_built_in_infos() {
	class_info_init(&M_Boolean, 2, m_classes[T_OBJECT], m_classes[T_BOOLEAN]);
	class_info_init(&M_Float, 2, m_classes[T_OBJECT], m_classes[T_FLOAT]);
	class_info_init(&M_Integer, 2, m_classes[T_OBJECT],m_classes[T_INTEGER]);
	class_info_init(&M_Object, 1, m_classes[T_OBJECT]);
	class_info_init(&M_Printer, 2, m_classes[T_OBJECT],m_classes[T_PRINTER]);
	class_info_init(&M_Scanner, 2, m_classes[T_OBJECT],m_classes[T_SCANNER]);
	class_info_init(&M_String, 2, m_classes[T_OBJECT],m_classes[T_STRING]);
	class_info_init(&M_System, 2, m_classes[T_OBJECT],m_classes[T_SYSTEM]);
}

