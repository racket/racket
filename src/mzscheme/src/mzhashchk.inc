
    if (!--hash_counter) {
      hash_counter = HASH_COUNT_START;
      SCHEME_USE_FUEL(HASH_COUNT_START);
    
#ifdef DO_STACK_CHECK
      {
#include "mzstkchk.h"
	{
#ifndef ERROR_ON_OVERFLOW
	  Scheme_Thread *p = scheme_current_thread;
	  p->ku.k.p1 = (void *)o;
#endif
	  return (long)scheme_handle_stack_overflow(hash_k);
	}
      }
#endif
    }
