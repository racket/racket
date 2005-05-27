
    if (!--equal_counter) {
      equal_counter = EQUAL_COUNT_START;
      SCHEME_USE_FUEL(EQUAL_COUNT_START);
    
#ifdef DO_STACK_CHECK
      {
#include "mzstkchk.h"
	{
#ifndef ERROR_ON_OVERFLOW
	  Scheme_Thread *p = scheme_current_thread;
	  p->ku.k.p1 = (void *)obj1;
	  p->ku.k.p2 = (void *)obj2;
#endif
	  return SCHEME_TRUEP(scheme_handle_stack_overflow(equal_k));
	}
      }
#endif
    }
