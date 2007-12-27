
    if (!--equal_counter) {
      equal_counter = EQUAL_COUNT_START;
      SCHEME_USE_FUEL(EQUAL_COUNT_START);
    
#ifdef DO_STACK_CHECK
      {
#include "mzstkchk.h"
        return is_equal_overflow(obj1, obj2, eql);
      }
#endif
    }
