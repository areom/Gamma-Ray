#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct {
        int generation;
        char* class;
        char** ancestors;
} ClassInfo;


ClassInfo M_BOOLEAN = {
	.ancestors = {
		m_classes[T_OBJECT ], m_classes[T_BOOLEAN ]
	},
	.generation = 1,
	.class = m_classes[T_BOOLEAN ]
};

ClassInfo M_FLOAT = {
	.ancestors = {
		m_classes[T_OBJECT ], m_classes[T_FLOAT ]
	},
	.generation = 1,
	.class = m_classes[T_FLOAT ]
};

ClassInfo M_INTEGER = {
	.ancestors = {
		m_classes[T_OBJECT ], m_classes[T_INTEGER ]
	},
	.generation = 1,
	.class = m_classes[T_INTEGER ]
};

ClassInfo M_OBJECT = {
	.ancestors = {
		m_classes[T_OBJECT ]
	},
	.generation = 0,
	.class = m_classes[T_OBJECT ]
};

ClassInfo M_PRINTER = {
	.ancestors = {
		m_classes[T_OBJECT ], m_classes[T_PRINTER ]
	},
	.generation = 1,
	.class = m_classes[T_PRINTER ]
};

ClassInfo M_SCANNER = {
	.ancestors = {
		m_classes[T_OBJECT ], m_classes[T_SCANNER ]
	},
	.generation = 1,
	.class = m_classes[T_SCANNER ]
};

ClassInfo M_STRING = {
	.ancestors = {
		m_classes[T_OBJECT ], m_classes[T_STRING ]
	},
	.generation = 1,
	.class = m_classes[T_STRING ]
};

ClassInfo M_SYSTEM = {
	.ancestors = {
		m_classes[T_OBJECT ], m_classes[T_SYSTEM ]
	},
	.generation = 1,
	.class = m_classes[T_SYSTEM ]
};

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
