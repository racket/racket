/* include "chezscheme.h" before this file */

#ifndef RACKETCS_H
#define RACKETCS_H

#ifndef RACKET_API_EXTERN
# define RACKET_API_EXTERN EXPORT
#endif

#ifndef RACKETCS_BOOT_H
# define BOOT_EXTERN EXPORT
# include "racketcsboot.h"
#endif

RACKET_API_EXTERN ptr racket_apply(ptr proc, ptr arg_list);

RACKET_API_EXTERN ptr racket_primitive(const char *name);

RACKET_API_EXTERN ptr racket_eval(ptr s_expr);
RACKET_API_EXTERN ptr racket_dynamic_require(ptr module_path, ptr sym_or_false);
RACKET_API_EXTERN void racket_namespace_require(ptr module_path);

RACKET_API_EXTERN void racket_embedded_load_bytes(const char *code, uptr len, int as_predefined);
RACKET_API_EXTERN void racket_embedded_load_file(const char *path, int as_predefined);
RACKET_API_EXTERN void racket_embedded_load_file_region(const char *path, uptr start, uptr end, int as_predefined);

RACKET_API_EXTERN void *racket_cpointer_address(ptr cptr);
RACKET_API_EXTERN void *racket_cpointer_base_address(ptr cptr);
RACKET_API_EXTERN iptr racket_cpointer_offset(ptr cptr);

#endif
