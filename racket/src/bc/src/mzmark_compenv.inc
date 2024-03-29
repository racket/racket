/* >>>> Generated by mkmark.rkt from mzmarksrc.c <<<< */

static int mark_comp_env_SIZE(void *p, struct NewGC *gc) {
#ifndef GC_NO_SIZE_NEEDED_FROM_PROCS
  gcBYTES_TO_WORDS(sizeof(Scheme_Comp_Env));
#else
  return 0;
#endif
}

static int mark_comp_env_MARK(void *p, struct NewGC *gc) {
#ifndef GC_NO_MARK_PROCEDURE_NEEDED
  Scheme_Comp_Env *e = (Scheme_Comp_Env *)p;

  gcMARK2(e->vars, gc);
  gcMARK2(e->value_name, gc);
  gcMARK2(e->linklet, gc);
  gcMARK2(e->realm, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Scheme_Comp_Env));
# endif
#endif
}

static int mark_comp_env_FIXUP(void *p, struct NewGC *gc) {
#ifndef GC_NO_FIXUP_PROCEDURE_NEEDED
  Scheme_Comp_Env *e = (Scheme_Comp_Env *)p;

  gcFIXUP2(e->vars, gc);
  gcFIXUP2(e->value_name, gc);
  gcFIXUP2(e->linklet, gc);
  gcFIXUP2(e->realm, gc);

# ifdef GC_NO_SIZE_NEEDED_FROM_PROCS
  return 0;
# else
  return
  gcBYTES_TO_WORDS(sizeof(Scheme_Comp_Env));
# endif
#endif
}

#define mark_comp_env_IS_ATOMIC 0
#define mark_comp_env_IS_CONST_SIZE 1


