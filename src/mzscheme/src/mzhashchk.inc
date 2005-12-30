
    if (!--hash_counter) {
      hash_counter = HASH_COUNT_START;
      SCHEME_USE_FUEL(HASH_COUNT_START);
    
#ifdef DO_STACK_CHECK
      {
#include "mzstkchk.h"
	{
          Scheme_Object *nv;
	  long val;
#ifndef ERROR_ON_OVERFLOW
	  Scheme_Thread *p = scheme_current_thread;
	  p->ku.k.p1 = (void *)o;
          p->ku.k.i1 = MZ_HASH_I1;
#endif
	  nv = scheme_handle_stack_overflow(MZ_HASH_K);
	  scheme_get_int_val(nv, &val);
          return val;
	}
      }
#endif
    }
