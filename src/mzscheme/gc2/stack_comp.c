
#include "mzconfig.h"
#if STACK_DIRECTION > 0
# define SHALLOWER_STACK_ADDRESS(a, b) ((unsigned long)a > (unsigned long)b)
#else
# define SHALLOWER_STACK_ADDRESS(a, b) ((unsigned long)a < (unsigned long)b)
#endif
