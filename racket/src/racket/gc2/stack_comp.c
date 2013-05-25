
#include "mzconfig.h"
#if STACK_DIRECTION > 0
# define SHALLOWER_STACK_ADDRESS(a, b) ((uintptr_t)a > (uintptr_t)b)
#else
# define SHALLOWER_STACK_ADDRESS(a, b) ((uintptr_t)a < (uintptr_t)b)
#endif
