/* 
   The "cstartup.inc" file is not, as distributed, compatible with
   omissions from or additions to the set of built-in identifiers.
   The reason is that the "cstartup.inc" file is a ".zo" version of the
   "startup.inc" files, and the ".zo" format changes when the set of
   built-in names changes (because indices assigned to the built-in
   names shift).

   If you make a version with omissions or additions, set
   USE_COMPILED_STARTUP to 0 and change the version on "schvers.h";
   then, `make cgc' and then `make cstartup' to create "cstartup.inc";
   finally, set EXPECTED_PRIM_COUNT to the right value and
   USE_COMPILED_STARTUP to 1 and `make' again. */

#define USE_COMPILED_STARTUP 1

#define EXPECTED_PRIM_COUNT 1131
#define EXPECTED_UNSAFE_COUNT 106
#define EXPECTED_FLFXNUM_COUNT 69
#define EXPECTED_EXTFL_COUNT 45
#define EXPECTED_FUTURES_COUNT 15
#define EXPECTED_FOREIGN_COUNT 79

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
