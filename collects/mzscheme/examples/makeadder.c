/* 
   Defines make-adder:
     (define (make-adder n)
       (lambda (m) (+ m n)))
   which illustrates closure-creation, looking up Scheme
   definitions, and calling Scheme procedures from C.
*/

#include "escheme.h"

/* The inner lambda, which must close over `n'. A closure function is
   like a regular Scheme-procedure function, except that it takes an
   extra argument containing the closure data. The closre data can be
   any format that we want. */
static Scheme_Object *sch_inner(void *closure_data, int argc, Scheme_Object **argv)
{
  /* We only close over one value, so our closure data reprsentation
     is just thaht value: */
  Scheme_Object *n = (Scheme_Object *)closure_data;
  Scheme_Object *plus;
  Scheme_Object *a[2];

  plus = scheme_lookup_global(scheme_intern_symbol("+"), 
			      scheme_get_env(NULL));

  /* return the result of summing m and n: */
  a[0] = n;
  a[1] = argv[0]; /* m */
  return _scheme_apply(plus, 2, a); 

  /* Actually, that's not quite right. In the Scheme code, (+ m n) is
     a tail call. The following would be better:
     return _scheme_tail_apply(plus, 2, a); */
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
  scheme_add_global("make-adder",
		    scheme_make_prim_w_arity(sch_make_adder,
					     "make-adder", 
					     1, 1),
		    env);

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
