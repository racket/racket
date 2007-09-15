/* 
   The cstartup.inc file is not, as distributed, compatible with
   omissions from or additions to the set of built-in identifiers.
   The reason is that the cstartup.inc file is a .zo version of the
   startup.inc files, and the .zo format changes when the set of
   built-in names changes (because indices assigned to the built-in
   names shift).

   If you make a version with omissions or additions and then run
   `make startup' to recreate the cstartup.inc file, set
   EXPECTED_PRIM_COUNT to the new value, and then USE_COMPILED_STARTUP
   can be set to 1 again. */

#define USE_COMPILED_STARTUP 1

#define EXPECTED_PRIM_COUNT 911

#ifdef MZSCHEME_SOMETHING_OMITTED
# undef USE_COMPILED_STARTUP
# define USE_COMPILED_STARTUP 0
#endif

#if defined(__MWERKS__) && !defined(powerc)
#define MZCOMPILED_STRING_FAR far
#else
#define MZCOMPILED_STRING_FAR /**/
#endif

#if USE_COMPILED_STARTUP
extern Scheme_Object *scheme_eval_compiled_sized_string(const char *str, int len, Scheme_Env *env);
#endif
