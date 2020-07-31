
#if 1
/* Normal version: */
# define PUSH_RUNSTACK(p, r, amt) (r -= amt)
# define CHECK_RUNSTACK(p, r) /**/
#else
/* Debugging version: */
# define PUSH_RUNSTACK(p, r, amt) (r -= amt, CHECK_RUNSTACK(p, r), r)
# define CHECK_RUNSTACK(p, r) \
  ((((uintptr_t)r) < ((uintptr_t)MZ_RUNSTACK_START)) \
   ? (scheme_signal_error("internal error: runstack overflow!"), 0) \
   : 0)
#endif
