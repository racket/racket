/*
  Extension that declares a module.
  The extension is equivalent to Scheme source of them form:
    (module idmodule mzscheme
      (define (identity x) x)
      (provide identity))
*/

#include "escheme.h"

static Scheme_Object *id(int argc, Scheme_Object **argv)
{
  return argv[0];
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Env *menv;
  Scheme_Object *proc;

  menv = scheme_primitive_module(scheme_intern_symbol("idmodule"),
				 env);

  proc = scheme_make_prim_w_arity(id, "identity", 1, 1);

  /* All added names are automatically exported by the module: */
  scheme_add_global("identity", proc, menv);

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
  /* This extension defines a module named `idmodule': */
  return scheme_intern_symbol("idmodule");
}
