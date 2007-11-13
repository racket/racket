
/* The version string has one of the forms:
      X.Y
      X.Y.Z     Z != 0
      X.Y.Z.W   W != 0
   where each X, Y, Z, W is a non-negative exact
   integer, Y must not exceed 99, and Z must not
   exceed 999. 

   The string and the separate X/Y/Z/W numbers must
   be updated consistently. */

#define MZSCHEME_VERSION "3.99.0.2"

#define MZSCHEME_VERSION_X 3
#define MZSCHEME_VERSION_Y 99
#define MZSCHEME_VERSION_Z 0
#define MZSCHEME_VERSION_W 2

#define MZSCHEME_VERSION_MAJOR ((MZSCHEME_VERSION_X * 100) + MZSCHEME_VERSION_Y)
#define MZSCHEME_VERSION_MINOR ((MZSCHEME_VERSION_Z * 1000) + MZSCHEME_VERSION_W)
