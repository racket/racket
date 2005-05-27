
#include "escheme.h"

static Scheme_Object *llsize(int argc, Scheme_Object **argv)
{
  return scheme_make_integer(sizeof(mzlonglong));
}

static Scheme_Object *toll(int argc, Scheme_Object **argv)
{
  mzlonglong l;

  if (scheme_get_long_long_val(argv[0], &l))
    return scheme_make_sized_byte_string((char *)&l, sizeof(mzlonglong), 1);
  else
    return scheme_false;
}

static Scheme_Object *toull(int argc, Scheme_Object **argv)
{
  umzlonglong l;

  if (scheme_get_unsigned_long_long_val(argv[0], &l))
    return scheme_make_sized_byte_string((char *)&l, sizeof(umzlonglong), 1);
  else
    return scheme_false;
}

static Scheme_Object *fromll(int argc, Scheme_Object **argv)
{
  mzlonglong l;

  if (!SCHEME_BYTE_STRINGP(argv[0])
      || (SCHEME_BYTE_STRTAG_VAL(argv[0]) != sizeof(mzlonglong)))
    scheme_wrong_type("long-long-bytes->integer", 
		      "byte string of mzlonglong size",
		      0, argc, argv);


  l = *(mzlonglong *)SCHEME_BYTE_STR_VAL(argv[0]);

  return scheme_make_integer_value_from_long_long(l);
}

static Scheme_Object *fromull(int argc, Scheme_Object **argv)
{
  umzlonglong l;

  if (!SCHEME_BYTE_STRINGP(argv[0])
      || (SCHEME_BYTE_STRTAG_VAL(argv[0]) != sizeof(umzlonglong)))
    scheme_wrong_type("unsigned-long-long-bytes->integer", 
		      "byte string of mzlonglong size",
		      0, argc, argv);


  l = *(umzlonglong *)SCHEME_BYTE_STR_VAL(argv[0]);

  return scheme_make_integer_value_from_unsigned_long_long(l);
}



Scheme_Object *scheme_reload(Scheme_Env *env)
{
  scheme_add_global("long-long-size", 
		    scheme_make_prim_w_arity(llsize, "long-long-size", 0, 0),
		    env);

  scheme_add_global("integer->long-long-bytes", 
		    scheme_make_prim_w_arity(toll, "integer->long-long-bytes", 1, 1),
		    env);
  scheme_add_global("integer->unsigned-long-long-bytes", 
		    scheme_make_prim_w_arity(toull, "integer->unsigned-long-long-bytes", 1, 1),
		    env);

  scheme_add_global("long-long-bytes->integer", 
		    scheme_make_prim_w_arity(fromll, "long-long-bytes->integer", 1, 1),
		    env);
  scheme_add_global("unsigned-long-long-bytes->integer", 
		    scheme_make_prim_w_arity(fromull, "unsigned-long-long-bytes->integer", 1, 1),
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
