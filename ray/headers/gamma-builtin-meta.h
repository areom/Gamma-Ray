#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct {
        int generation;
        char* class;
        char** ancestors;
} ClassInfo;


ClassInfo M_BOOLEAN;
ClassInfo M_FLOAT;
ClassInfo M_INTEGER;
ClassInfo M_OBJECT;
ClassInfo M_PRINTER;
ClassInfo M_SCANNER;
ClassInfo M_STRING;
ClassInfo M_SYSTEM;


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
	class_info_init(&M_BOOLEAN, 2, m_classes[T_OBJECT], m_classes[T_BOOLEAN]);
	class_info_init(&M_FLOAT, 2, m_classes[T_OBJECT], m_classes[T_FLOAT]);
	class_info_init(&M_FLOAT, 2, m_classes[T_OBJECT],m_classes[T_FLOAT]);
	class_info_init(&M_INTEGER, 2, m_classes[T_OBJECT],m_classes[T_INTEGER]);
	class_info_init(&M_OBJECT, 1, m_classes[T_OBJECT]);
	class_info_init(&M_PRINTER, 2, m_classes[T_OBJECT],m_classes[T_PRINTER]);
	class_info_init(&M_SCANNER, 2, m_classes[T_OBJECT],m_classes[T_SCANNER]);
	class_info_init(&M_STRING, 2, m_classes[T_OBJECT],m_classes[T_STRING]);
	class_info_init(&M_SYSTEM, 2, m_classes[T_OBJECT],m_classes[T_SYSTEM]);
}

