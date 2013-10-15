#ifndef SCHEME_CURRENT_PROCESS
# define SCHEME_CURRENT_PROCESS scheme_current_thread
#endif
#ifndef SCHEME_STACK_BOUNDARY
# define SCHEME_STACK_BOUNDARY scheme_stack_boundary
#endif
#ifndef SCHEME_PLUS_STACK_DELTA
# define SCHEME_PLUS_STACK_DELTA(x) x
#endif

#ifdef SPAWN_NEW_STACK
  uintptr_t _stk_pos;

  _stk_pos = (uintptr_t)&_stk_pos;
  if (STK_COMP(SCHEME_PLUS_STACK_DELTA(_stk_pos), (uintptr_t)SCHEME_CURRENT_PROCESS->stack_end)
      && !scheme_no_stack_overflow)
#else
# ifdef USE_STACKAVAIL
  if ((stackavail() < STACK_SAFETY_MARGIN) && !scheme_no_stack_overflow)
# endif
# if defined(UNIX_FIND_STACK_BOUNDS) || defined(WINDOWS_FIND_STACK_BOUNDS) \
     || defined(MACOS_FIND_STACK_BOUNDS) || defined(ASSUME_FIXED_STACK_SIZE) \
     || defined(BEOS_FIND_STACK_BOUNDS) || defined(OSKIT_FIXED_STACK_BOUNDS) \
     || defined(PALM_FIND_STACK_BOUNDS) || defined(PTHREAD_STACKSEG_FIND_STACK_BOUNDS)
  uintptr_t _stk_pos;

  _stk_pos = (uintptr_t)&_stk_pos;

if (STK_COMP(SCHEME_PLUS_STACK_DELTA(_stk_pos), SCHEME_STACK_BOUNDARY)
      && !scheme_no_stack_overflow)
# endif
#endif

#undef SCHEME_CURRENT_PROCESS
#undef SCHEME_STACK_BOUNDARY
#undef SCHEME_PLUS_STACK_DELTA
