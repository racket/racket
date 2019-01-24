BOOT_EXTERN void racket_boot(int argc, char **argv, char *exec_file, char *run_file,
			     char *boot_exe, long segment_offset,
			     char *coldir, char *configdir, /* wchar_t * */void *dlldir,
			     int pos1, int pos2, int pos3,
			     int cs_compiled_subdir, int is_gui,
			     int wm_is_gracket_or_x11_arg_count, char *gracket_guid_or_x11_args,
			     void *ddll_open, void *dll_find_object);

typedef void (*racket_boot_t)(int argc, char **argv, char *exec_file, char *run_file,
			      char* boot_exe, long segment_offset,
			      char *coldir, char *configdir, /* wchar_t * */void *dlldir,
			      int pos1, int pos2, int pos3,
			      int cs_compiled_subdir, int is_gui,
                              int wm_is_gracket_or_x11_arg_count, char *gracket_guid_or_x11_args,
			      void *ddll_open, void *dll_find_object);
