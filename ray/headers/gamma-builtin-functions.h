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
