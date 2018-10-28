BOOT_EXTERN void racket_boot(int argc, char **argv, char *self,
			     char *boot_exe, long segment_offset,
			     char *coldir, char *configdir,
			     int pos1, int pos2, int pos3,
			     int cs_compiled_subdir, int is_gui);

typedef void (*racket_boot_t)(int argc, char **argv, char *self,
			      char* boot_exe, long segment_offset,
			      char *coldir, char *configdir,
			      int pos1, int pos2, int pos3,
			      int cs_compiled_subdir, int is_gui);
