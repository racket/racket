#include "escheme.h"

Scheme_Object *ex(int argc, Scheme_Object **argv)
{
  return scheme_make_utf8_string("Hello, world!");
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Env *menv;

  menv = scheme_primitive_module(scheme_intern_symbol("embed-me8"),
				 env);

  scheme_add_global("ex", scheme_make_prim_w_arity(ex, "ex", 0, 0), menv);

  scheme_finish_primitive_module(menv);

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  /* First load is same as every load: */
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  return scheme_intern_symbol("embed-me8");
}
