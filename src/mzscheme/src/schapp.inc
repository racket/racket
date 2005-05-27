
  MZ_MARK_STACK_TYPE old_cont_mark_stack;
  Scheme_Object *v;
  Scheme_Closed_Primitive_Proc *prim;
  Scheme_Thread *p = scheme_current_thread;

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

  prim = (Scheme_Closed_Primitive_Proc *)rator;

#if PRIM_CHECK_ARITY
  if (argc < prim->mina || (argc > prim->maxa && prim->maxa >= 0)) {
    scheme_wrong_count(prim->name, prim->mina, prim->maxa, argc, argv);
    return NULL; /* Shouldn't get here */
  }
#endif

  MZ_CONT_MARK_POS++;
  old_cont_mark_stack = MZ_CONT_MARK_STACK;

  v = prim->prim_val(prim->data, argc, argv);
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
