      SCHEME_USE_FUEL(1);
#ifdef DO_STACK_CHECK
      {
#include "mzstkchk.h"
        return is_equal_overflow(obj1, obj2, eql);
      }
#endif
