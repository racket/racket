
/* The version string has one of the forms:
      X.Y
      X.Y.Z     Z != 0
      X.Y.Z.W   W != 0
   where each X, Y, Z, W is a non-negative exact integer, Y must not
   exceed 99, and Z or W must not exceed 999.  Y>=90 means that this is
   working towards {X+1}.0, and X.Y (Z=0, W=0) is an alpha version for
   {X+1}.0; Z>=900 means working towards X.{Y+1}, and X.Y.Z as an
   alpha release.

   Note that the version number in the "base" package's "info.rkt"
   needs to be updated separately.
*/

#define MZSCHEME_VERSION_X 8
#define MZSCHEME_VERSION_Y 11
#define MZSCHEME_VERSION_Z 1
#define MZSCHEME_VERSION_W 1

/* A level of indirection makes `#` work as needed: */
#define AS_a_STR_HELPER(x) #x
#define AS_a_STR(x) AS_a_STR_HELPER(x)

#if MZSCHEME_VERSION_W != 0
# define MZSCHEME_VERSION AS_a_STR(MZSCHEME_VERSION_X) "." AS_a_STR(MZSCHEME_VERSION_Y) "." AS_a_STR(MZSCHEME_VERSION_Z) "." AS_a_STR(MZSCHEME_VERSION_W)
#elif MZSCHEME_VERSION_Z != 0
# define MZSCHEME_VERSION AS_a_STR(MZSCHEME_VERSION_X) "." AS_a_STR(MZSCHEME_VERSION_Y) "." AS_a_STR(MZSCHEME_VERSION_Z)
#else
# define MZSCHEME_VERSION AS_a_STR(MZSCHEME_VERSION_X) "." AS_a_STR(MZSCHEME_VERSION_Y)
#endif

#define MZSCHEME_VERSION_MAJOR ((MZSCHEME_VERSION_X * 100) + MZSCHEME_VERSION_Y)
#define MZSCHEME_VERSION_MINOR ((MZSCHEME_VERSION_Z * 1000) + MZSCHEME_VERSION_W)

#define MZSCHEME_VM "racket"
