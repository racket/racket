
    if (!--hash_counter) {
      hash_counter = HASH_COUNT_START;
      SCHEME_USE_FUEL(HASH_COUNT_START);
    
#ifdef DO_STACK_CHECK
      {
#include "mzstkchk.h"
        return OVERFLOW_HASH();
      }
#endif
    }
