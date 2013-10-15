/* 
   Racket extension example that catches exceptions and extracts
   error messages.

   The defined function is `eval-string/catch-error', which takes a
   string and evaluates it, returning either the value, a string for
   the error message, and a non-exn value raised by the expression.

  > (eval-string/catch-error "10")
  10

  > (eval-string/catch-error "(+ 'a)")
  "+: expects argument of type <number>; given a"

  > (eval-string/catch-error "(raise 'ack)")
  ack

*/

#include "escheme.h"

/*********************************************************************/
/* Exception-catching code                                           */
/*********************************************************************/

/* These must be registered with the memory manager: */
static Scheme_Object *exn_catching_apply, *exn_p, *exn_message;

static void init_exn_catching_apply()
{
  if (!exn_catching_apply) {
    Scheme_Env *env;
    char *e = 
      "(lambda (thunk) "
	"(with-handlers ([void (lambda (exn) (cons #f exn))]) "
	  "(cons #t (thunk))))";

    /* make sure we have a namespace with the standard bindings: */
    env = (Scheme_Env *)scheme_make_namespace(0, NULL);

    scheme_register_extension_global(&exn_catching_apply, sizeof(Scheme_Object *));
    scheme_register_extension_global(&exn_p, sizeof(Scheme_Object *));
    scheme_register_extension_global(&exn_message, sizeof(Scheme_Object *));
    
    exn_catching_apply = scheme_eval_string(e, env);
    exn_p = scheme_lookup_global(scheme_intern_symbol("exn?"), env);
    exn_message = scheme_lookup_global(scheme_intern_symbol("exn-message"), env);
  }
}

/* This function applies a thunk, returning the Scheme value if there's no exception, 
   otherwise returning NULL and setting *exn to the raised value (usually an exn 
   structure). */
Scheme_Object *_apply_thunk_catch_exceptions(Scheme_Object *f, Scheme_Object **exn)
{
  Scheme_Object *v;

  init_exn_catching_apply();
  
  v = _scheme_apply(exn_catching_apply, 1, &f);
  /* v is a pair: (cons #t value) or (cons #f exn) */

  if (SCHEME_TRUEP(SCHEME_CAR(v)))
    return SCHEME_CDR(v);
  else {
    *exn = SCHEME_CDR(v);
    return NULL;
  }
}

Scheme_Object *extract_exn_message(Scheme_Object *v)
{
  init_exn_catching_apply();

  if (SCHEME_TRUEP(_scheme_apply(exn_p, 1, &v)))
    return _scheme_apply(exn_message, 1, &v);
  else
    return NULL; /* Not an exn structure */
}

/*********************************************************************/
/* Use of example exception-catching code                            */
/*********************************************************************/

static Scheme_Object *do_eval(void *s, int noargc, Scheme_Object **noargv)
{
  return scheme_eval_string((char *)s, scheme_get_env(NULL));
}

static Scheme_Object *eval_string_or_get_exn_message(char *s)
{
  Scheme_Object *v, *exn;

  v = _apply_thunk_catch_exceptions(scheme_make_closed_prim(do_eval, s), &exn);
  /* Got a value? */
  if (v)
    return v;

  v = extract_exn_message(exn);
  /* Got an exn? */
  if (v)
    return v;

  /* `raise' was called on some arbitrary value */
  return exn;
}

static Scheme_Object *catch_eval_error(int argc, Scheme_Object **argv)
{
  Scheme_Object *bs;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("eval-string/catch-error", "string", 0, argc, argv);

  bs = scheme_char_string_to_byte_string(argv[0]);
  
  return eval_string_or_get_exn_message(SCHEME_BYTE_STR_VAL(bs));
}

/*********************************************************************/
/* Initialization                                                    */
/*********************************************************************/

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  scheme_add_global("eval-string/catch-error",
		    scheme_make_prim_w_arity(catch_eval_error,
					     "eval-string/catch-error", 
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
