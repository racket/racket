/* 
   The same as makeaddr.c, but revised and annotated so that it works
   with 3m without using --xform. All non-3m comments have been
   deleted (to better highlight the 3m parts).
*/

#include "escheme.h"

static Scheme_Object *sch_inner(void *closure_data, int argc, Scheme_Object **argv)
{
  Scheme_Object *n = (Scheme_Object *)closure_data;
  Scheme_Object *plus, *plus_sym, *result;
  Scheme_Env *env;
  Scheme_Object *a[2];
  /* Declare registration space. The number 6 comes from the
     MZ_GC_VAR... declarations (i.e., if we add or remove
     some, the number changes */
  MZ_GC_DECL_REG(6);

  MZ_GC_ARRAY_VAR_IN_REG(0, a, 2); /* takes 3 slots */
  MZ_GC_VAR_IN_REG(3, argv);
  MZ_GC_VAR_IN_REG(4, n);
  MZ_GC_VAR_IN_REG(5, plus_sym);
  MZ_GC_REG();

  /* Note that we've pulled out nested calls and assigned
     the results to explicitly declared variables. Even though
     `env' is not help across an allocating function call,
     we need to lift out the call to scheme_get_env(), otherwise
     plus_sym's value might get pushed on the stack in anticipation
     of the function call, and the corresponding object might
     move. As written, plus_sym's value is not set up for the
     call until after scheme_get_env() returns. */
  plus_sym = scheme_intern_symbol("+");
  env = scheme_get_env(NULL);
  plus = scheme_lookup_global(plus_sym, env);

  a[0] = n;
  a[1] = argv[0]; /* m */
  result = _scheme_apply(plus, 2, a); 
  
  /* The following unregister can't go before _scheme_apply,
     because `a' is passed in as a stack-allocated array.
     If `a' were heap-allocated, instead, MZ_GC_UNREG()
     could go before the call to _scheme_apply. */
  MZ_GC_UNREG();

  return result;

}

static Scheme_Object *sch_make_adder(int argc, Scheme_Object **argv)
{
  return scheme_make_closed_prim_w_arity(sch_inner,
					 argv[0],
					 "adder",
					 1, 1);
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Object *p;
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, env);
  
  MZ_GC_REG();

  p = scheme_make_prim_w_arity(sch_make_adder,
			       "make-adder", 
			       1, 1);

  scheme_add_global("make-adder", p, env);

  MZ_GC_UNREG();

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  /* First load is same as every load: */
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  /* This extension doesn't define a module: */
  return scheme_false;
}
