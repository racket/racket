
/* For non-tail calls, the native context has already
   incremented MZ_CONT_MARK_POS. Counter
   scheme_do_eval()'s increment, because this
   might be the continuation of a tail call. */

/* The arguments in argv are in the runstack. If computation can go
   back into native code, those arguments should not live past the
   native-code call. The native code clears/reuses arguments itself if
   they are on the stack, but there's a problem if a tail buffer leads
   to new pushes onto the run stack. We handle this with code marked
   [TC-SFS]. */

/* This code is written in such a way that xform can
   see that no GC cooperation is needed. */

static MZ_INLINE Scheme_Object *PRIM_APPLY_NAME_FAST(Scheme_Object *rator,
                                                     int argc,
                                                     Scheme_Object **argv)
{
  GC_CAN_IGNORE Scheme_Object *v;
  GC_CAN_IGNORE Scheme_Primitive_Proc *prim;
  GC_CAN_IGNORE Scheme_Primitive_Closure_Proc *f;
  
  prim = (Scheme_Primitive_Proc *)rator;
  
  if (argc < prim->mina || (argc > prim->mu.maxa && prim->mina >= 0)) {
    scheme_wrong_count_m(prim->name, prim->mina, prim->mu.maxa, argc, argv,
                         prim->pp.flags & SCHEME_PRIM_IS_METHOD);
    return NULL; /* Shouldn't get here */
  }
  
  f = (Scheme_Primitive_Closure_Proc *)prim->prim_val;
  v = f(argc, argv, (Scheme_Object *)prim);

#if PRIM_CHECK_VALUE
  if (v == SCHEME_TAIL_CALL_WAITING) {
    int i;
    for (i = 0; i < argc; i++) { argv[i] = NULL; } /* [TC-SFS]; see above */
    v = scheme_force_value_same_mark(v);
  }
#endif
  
#if PRIM_CHECK_MULTI
  if (v == SCHEME_MULTIPLE_VALUES) {
    scheme_wrong_return_arity(NULL, 1, scheme_multiple_count, scheme_multiple_array, NULL);
    return NULL; /* Shouldn't get here */
  }
#endif
  
  return v;
}

Scheme_Object *PRIM_APPLY_NAME(Scheme_Object *rator,
                               int argc,
                               Scheme_Object **argv)
{
  if (!SCHEME_INTP(rator)) {
    Scheme_Type t;

    t = _SCHEME_TYPE(rator);

    if (t == scheme_prim_type) {
      return PRIM_APPLY_NAME_FAST(rator, argc, argv);
    } if ((t == scheme_proc_chaperone_type)
          && SCHEME_MPAIRP(((Scheme_Chaperone *)rator)->redirects)) {
      return scheme_apply_chaperone(rator, argc, argv, NULL, PRIM_CHECK_MULTI | (PRIM_CHECK_VALUE << 1));
    }
  }

#if PRIM_CHECK_MULTI
  {
    GC_CAN_IGNORE Scheme_Object *v;
    MZ_CONT_MARK_POS -= 2;


    v = _scheme_apply(rator, argc, argv);
    MZ_CONT_MARK_POS += 2;
    return v;
  }
#else
# if PRIM_CHECK_VALUE
  {

    GC_CAN_IGNORE Scheme_Object *v;
    MZ_CONT_MARK_POS -= 2;
    v = _scheme_apply_multi(rator, argc, argv);
    MZ_CONT_MARK_POS += 2;
    return v;
  }
# else 

  return _scheme_tail_apply(rator, argc, argv);
# endif
#endif
}

#undef PRIM_CHECK_VALUE
#undef PRIM_CHECK_MULTI
#undef PRIM_APPLY_NAME
#undef PRIM_APPLY_NAME_FAST
