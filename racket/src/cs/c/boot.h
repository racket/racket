#ifndef RACKETCS_BOOT_H
#define RACKETCS_BOOT_H

/* This structure type can change, but NULL/0 will be supported as a
   default for any new field that is added. */
typedef struct racket_boot_arguments_t {
  /* Boot files --- potentially the same path with different offsets.
     If a boot image is embedded in a larger file, it must be
     terminated with "\177". */
  const char *boot1_path; /* REQUIRED; path to "petite.boot" */
  long boot1_offset;
  long boot1_len; /* 0 => unknown length */
  const char *boot2_path; /* REQUIRED; path to "scheme.boot" */
  long boot2_offset;
  long boot2_len; /* 0 => unknown length */
  const char *boot3_path; /* REQUIRED; path to "racket.boot" */
  long boot3_offset;
  long boot3_len; /* 0 => unknown length */

  /* Command-line arguments are handled in the same way as the
     `racket` exectuable. The `argv` array should *not* include the
     executable name like `argv` passed to `main`. */
  int argc;
  char **argv; /* NULL => "-n", which does nothing after booting */

  /* Racket path configuration, mostly setting the results of
     `(find-system-path ...)`: */
  const char *exec_file;  /* REQUIRED; usually the original argv[0] */
  const char *run_file;   /* can be NULL to mean the same as `exec_file` */
  const char *collects_dir;   /* can be NULL or "" to disable collection path */
  const char *config_dir; /* use NULL or "etc" if you don't care */
  /* wchar_t * */void *dll_dir; /* can be NULL for default */
  const char *k_file;     /* for -k; can be NULL for the same as `exec_file` */

  /* How to initialize `use-compiled-file-paths`: */
  int cs_compiled_subdir; /* true => subdirectory of "compiled" */
    
  /* Embedded-code offset, which is added to any `-k` argument. */
  long segment_offset; /* use 0 if no `-k` embedding */
  
  /* For embedded DLLs on Windows, if non-NULL: */
  void *dll_open;
  void *dll_find_object;
  void *dll_close;
  
  /* Whether to run as command-line Racket or in embedded mode: */
  int exit_after; /* 1 => exit after handling the command-line */

  /* For GUI applications; use 0 and "" as defaults: */
  int is_gui;
  int wm_is_gracket_or_x11_arg_count;
  char *gracket_guid_or_x11_args;
} racket_boot_arguments_t;

BOOT_EXTERN void racket_boot(racket_boot_arguments_t *boot_args);

/* Same as `racket_boot` prototype; but in type form: */
typedef void (*racket_boot_t)(racket_boot_arguments_t *boot_args);

#endif
