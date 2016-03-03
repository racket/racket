
  MZ_MARK_STACK_TYPE old_cont_mark_stack;
  GC_CAN_IGNORE Scheme_Object *v;
  GC_CAN_IGNORE Scheme_Primitive_Closure *prim;
  GC_CAN_IGNORE Scheme_Thread *p = scheme_current_thread;
  GC_CAN_IGNORE Scheme_Primitive_Closure_Proc *f;

#if !PRIM_NO_STACK_CHECK
# ifdef DO_STACK_CHECK
#  define SCHEME_CURRENT_PROCESS p
#  ifdef MZ_REAL_THREADS
#   define SCHEME_STACK_BOUNDARY ((unsigned long)p->stack_end)
#  endif
#  include "mzstkchk.h"
  {
#  ifndef ERROR_ON_OVERFLOW
    GC_CAN_IGNORE void *ra;
    if (argc) {
      int i;
      ra = (void *)MALLOC_N(Scheme_Object*, argc);
      for (i = argc; i--; ) {
	((Scheme_Object **)ra)[i] = argv[i];
      }
    } else
      ra = NULL;
    p->ku.k.p1 = (void *)rator;
    p->ku.k.i1 = argc;
    p->ku.k.p2 = (void *)ra;
#  endif
    return scheme_handle_stack_overflow(do_apply_known_k);
  }
# endif

  DO_CHECK_FOR_BREAK(p, ;);
#endif

  prim = (Scheme_Primitive_Closure *)rator;

#if PRIM_CHECK_ARITY
  if (argc < prim->p.mina || (argc > prim->p.mu.maxa && prim->p.mina >= 0)) {
    scheme_wrong_count_m(prim->p.name, prim->p.mina, prim->p.mu.maxa, argc, argv, 0);
    return NULL; /* Shouldn't get here */
  }
#endif

  MZ_CONT_MARK_POS++;
  old_cont_mark_stack = MZ_CONT_MARK_STACK;

  f = prim->p.prim_val;
  v = f(argc, argv, (Scheme_Object *)prim);

#if !PRIM_NO_CHECK_VALUE
  v = _scheme_force_value(v);
#endif

#if PRIM_CHECK_MULTI
  if (v == SCHEME_MULTIPLE_VALUES) {
    scheme_wrong_return_arity(NULL, 1, scheme_multiple_count, scheme_multiple_array, NULL);
    return NULL; /* Shouldn't get here */
  }
#endif

  --MZ_CONT_MARK_POS;
  MZ_CONT_MARK_STACK = old_cont_mark_stack;

  DEBUG_CHECK_TYPE(v);

  return v;

#undef PRIM_NO_STACK_CHECK
#undef PRIM_NO_CHECK_VALUE
#undef PRIM_CHECK_ARITY
#undef PRIM_CHECK_MULTI
