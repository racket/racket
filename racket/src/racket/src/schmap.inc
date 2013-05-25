
/* common code for `map', `for-each', `andmap' and `ormap' */

/*
 DO_MAP = C function name
 MAP_NAME = Racket function name as string
 MAP_MODE => map
 FOR_EACH_MODE => for-each
 AND_MODE => and mode
 OR_MODE => or mode
*/

static Scheme_Object *
DO_MAP(int argc, Scheme_Object *argv[])
{
# define NUM_QUICK_ARGS 3
# define NUM_QUICK_RES  5
  int i, size = 0, l, pos;
  Scheme_Object *quick1[NUM_QUICK_ARGS], *quick2[NUM_QUICK_ARGS];
  Scheme_Object **working, **args;
# ifdef MAP_MODE
  Scheme_Object *quick3[NUM_QUICK_RES], **resarray;
# endif
# ifndef FOR_EACH_MODE
  Scheme_Object *v;
# endif
  int cc;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_contract(MAP_NAME, "procedure?", 0, argc, argv);

  for (i = 1; i < argc; i++) {
    l = scheme_proper_list_length(argv[i]);
    
    if (l < 0)
      scheme_wrong_contract(MAP_NAME, "list?", i, argc, argv);
    
    if (i == 1)
      size = l;
    else if (size != l) {
      char *argstr;
      intptr_t alen;
      
      argstr = scheme_make_args_string("", -1, argc, argv, &alen);
      
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                       "%s: all lists must have same size%t",
                       MAP_NAME, argstr, alen);
      return NULL;
    }
  }

  if (SCHEME_FALSEP(scheme_get_or_check_arity(argv[0], argc - 1))) {
    char *s;
    intptr_t aelen;

    s = scheme_make_arity_expect_string(MAP_NAME, argv[0], argc - 1, NULL, &aelen);

    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%t",
		     s, aelen);
    return NULL;
  }

  if (argc <= (NUM_QUICK_ARGS + 1)) {
    args = quick1;
    working = quick2;
  } else {
    args = MALLOC_N(Scheme_Object *, argc - 1);
    working = MALLOC_N(Scheme_Object *, argc - 1);
  }

#ifdef MAP_MODE
  if (size <= NUM_QUICK_RES)
    resarray = quick3;
  else
    resarray = MALLOC_N(Scheme_Object *, size);
#endif

  /* Copy argc into working array */
  for (i = 1; i < argc; i++) {
    working[i-1] = argv[i];
  }

  --argc;

  pos = 0;
  while (pos < size) {
    /* collect args to apply */
    for (i = 0; i < argc ; i++) {
#if 0
      /* No longer needed: */
      if (!SCHEME_PAIRP(working[i])) {
	/* There was a mutation! */
	scheme_raise_exn(MZEXN_FAIL_CONTRACT,
			 "%s: argument list mutated",
			 MAP_NAME);
	return NULL;
      }
#endif
      args[i] = SCHEME_CAR(working[i]);
      working[i] = SCHEME_CDR(working[i]);
    }

    cc = scheme_cont_capture_count;

#ifdef MAP_MODE
    v = _scheme_apply(argv[0], argc, args);
#else
# ifdef FOR_EACH_MODE
    _scheme_apply_multi(argv[0], argc, args);
# else
    if (pos + 1 == size) {
      return _scheme_tail_apply(argv[0], argc, args);
    } else {
      v = _scheme_apply(argv[0], argc, args);
    }
# endif
#endif

    if (cc != scheme_cont_capture_count) {
      /* Copy arrays to avoid messing with other continuations */
#ifdef MAP_MODE
      if (size > NUM_QUICK_RES) {
	Scheme_Object **naya;
	naya = MALLOC_N(Scheme_Object *, size);
	memcpy(naya, resarray, pos * sizeof(Scheme_Object *));
	resarray = naya;
      }
#endif
      if ((argc > NUM_QUICK_ARGS) && (pos + 1 < size)) {
	Scheme_Object **naya;
	args = MALLOC_N(Scheme_Object *, argc);
	naya = MALLOC_N(Scheme_Object *, argc);
	memcpy(naya, working, argc * sizeof(Scheme_Object *));
	working = naya;
      }
    }

#ifdef MAP_MODE
    resarray[pos] = v;
#endif
#ifdef AND_MODE
    if (SCHEME_FALSEP(v))
      return scheme_false;
#endif
#ifdef OR_MODE
    if (SCHEME_TRUEP(v))
      return v;
#endif
    pos++;
  }

#ifdef MAP_MODE
  return scheme_build_list(size, resarray);
#endif
#ifdef FOR_EACH_MODE
  return scheme_void;
#endif
#ifdef AND_MODE
  return scheme_true;
#endif
#ifdef OR_MODE
  return scheme_false;
#endif
}
