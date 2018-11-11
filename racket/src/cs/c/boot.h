BOOT_EXTERN void racket_boot(int argc, char **argv, char *exec_file, char *run_file,
			     char *boot_exe, long segment_offset,
			     char *coldir, char *configdir,
			     int pos1, int pos2, int pos3,
			     int cs_compiled_subdir, int is_gui,
			     int wm_is_gracket, char *gracket_guid);

typedef void (*racket_boot_t)(int argc, char **argv, char *exec_file, char *run_file,
			      char* boot_exe, long segment_offset,
			      char *coldir, char *configdir,
			      int pos1, int pos2, int pos3,
			      int cs_compiled_subdir, int is_gui,
			      int wm_is_gracket, char *gracket_guid);
