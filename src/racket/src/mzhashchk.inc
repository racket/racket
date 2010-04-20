#ifdef MZ_USE_PLACES
     if (!do_atomic)
#endif
      SCHEME_USE_FUEL(1);
#ifdef DO_STACK_CHECK
      {
#include "mzstkchk.h"
        return OVERFLOW_HASH();
      }
#endif
