/* The rktio library has no callbacks, so no GC. */
#define RKTIO_EXTERN XFORM_NONGCING extern

#include "rktio.h"
