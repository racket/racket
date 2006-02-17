  if (!SCHEME_INTP(rator)) {
    Scheme_Type t;

    t = _SCHEME_TYPE(rator);

    if (t == scheme_prim_type) {
      Scheme_Object *v;
      Scheme_Primitive_Proc *prim;

      prim = (Scheme_Primitive_Proc *)rator;

      if (argc < prim->mina || (argc > prim->maxa && prim->maxa >= 0)) {
	scheme_wrong_count(prim->name, prim->mina, prim->maxa, argc, argv);
	return NULL; /* Shouldn't get here */
      }

      v = prim->prim_val(argc, argv);

#if PRIM_CHECK_VALUE
      v = _scheme_force_value(v);
#endif

#if PRIM_CHECK_MULTI
      if (v == SCHEME_MULTIPLE_VALUES) {
	scheme_wrong_return_arity(NULL, 1, scheme_multiple_count, scheme_multiple_array, NULL);
	return NULL; /* Shouldn't get here */
      }
#endif

      return v;
    } else if (t == scheme_closed_prim_type) {
      Scheme_Object *v;
      Scheme_Closed_Primitive_Proc *prim;

      prim = (Scheme_Closed_Primitive_Proc *)rator;

      if (argc < prim->mina || (argc > prim->maxa && prim->maxa >= 0)) {
	scheme_wrong_count(prim->name, prim->mina, prim->maxa, argc, argv);
	return NULL; /* Shouldn't get here */
      }

      v = prim->prim_val(prim->data, argc, argv);

#if PRIM_CHECK_VALUE
      v = _scheme_force_value(v);
#endif

#if PRIM_CHECK_MULTI
      if (v == SCHEME_MULTIPLE_VALUES) {
	scheme_wrong_return_arity(NULL, 1, scheme_multiple_count, scheme_multiple_array, NULL);
	return NULL; /* Shouldn't get here */
      }
#endif

      return v;
    }
  }

#if PRIM_CHECK_MULTI
  return _scheme_apply(rator, argc, argv);
#else
# if PRIM_CHECK_VALUE
  return _scheme_apply_multi(rator, argc, argv);
# else
  return _scheme_tail_apply(rator, argc, argv);
# endif
#endif

#undef PRIM_CHECK_VALUE
#undef PRIM_CHECK_MULTI
