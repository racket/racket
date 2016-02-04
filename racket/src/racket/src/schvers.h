
/* The version string has one of the forms:
      X.Y
      X.Y.Z     Z != 0
      X.Y.Z.W   W != 0
   where each X, Y, Z, W is a non-negative exact integer, Y must not
   exceed 99, and Z or W must not exceed 999.  Y>=90 means that this is
   working towards {X+1}.0, and X.Y (Z=0, W=0) is an alpha version for
   {X+1}.0; Z>=900 means working towards X.{Y+1}, and X.Y.Z as an
   alpha release.

   (The string and the separate X/Y/Z/W numbers must be updated
   consistently.)
*/

#define MZSCHEME_VERSION "6.4"

#define MZSCHEME_VERSION_X 6
#define MZSCHEME_VERSION_Y 4
#define MZSCHEME_VERSION_Z 0
#define MZSCHEME_VERSION_W 0

#define MZSCHEME_VERSION_MAJOR ((MZSCHEME_VERSION_X * 100) + MZSCHEME_VERSION_Y)
#define MZSCHEME_VERSION_MINOR ((MZSCHEME_VERSION_Z * 1000) + MZSCHEME_VERSION_W)
