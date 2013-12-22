typedef struct {
        int generation;
        char* class;
        char** ancestors;
} ClassInfo;

void class_info_init(do var arg magic here) {


}

ClassInfo M_BOOLEAN;
ClassInfo M_FLOAT;
ClassInfo M_INTEGER;
ClassInfo M_OBJECT;
ClassInfo M_PRINTER;
ClassInfo M_SCANNER;
ClassInfo M_STRING;
ClassInfo M_SYSTEM;

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

