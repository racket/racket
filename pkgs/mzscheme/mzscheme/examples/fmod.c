/*
  Extension that defines fmod, modulo on floating-point numbers.
  The extension is equivalent to Scheme source of them form:
    (define (fmod a b) ...)
*/

#include "escheme.h"
#include <math.h>

/**************************************************/

/* Every C implementation of a Scheme function takes argc and an array
   of Scheme_Object* values for argv, and returns a Scheme_Object*: */
static Scheme_Object *sch_fmod(int argc, Scheme_Object **argv)
{
  /* Because we'll use scheme_make_prim_w_arity, Racket will
     have already checked that we're getting the right number of
     arguments. */
  Scheme_Object *a = argv[0], *b = argv[1];
  double v;

  /* Make sure we got real numbers, and complain if not: */
  if (!SCHEME_REALP(a))
    scheme_wrong_type("fmod", "real number", 0, argc, argv);
  /*                       1st arg wrong ----^ */
  if (!SCHEME_REALP(b))
    scheme_wrong_type("fmod", "real number", 1, argc, argv);
  /*                       2nd arg wrong ----^ */

  /* Convert the Scheme numbers to double-precision floating point
     numbers, and compute fmod: */
  v = fmod(scheme_real_to_double(a),
	   scheme_real_to_double(b));

  /* Return the result, packaging it as a Scheme value: */
  return scheme_make_double(v);
}

/**************************************************/

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Object *proc;

  /* The MZ_GC... lines are for for 3m, because env is live across an
     allocating call. They're not needed for plain old (conservatively
     collected) Mzscheme. See makeadder3m.c for more info. */
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, env);
  MZ_GC_REG();

  /* Package the C implementation of fmod into a Scheme procedure
     value: */
  proc = scheme_make_prim_w_arity(sch_fmod, "fmod", 2, 2);
  /*               Requires at least two args ------^  ^ */
  /*                  Accepts no more than two args ---| */

  /* Define `fmod' as a global :*/
  scheme_add_global("fmod", proc, env);

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
