/*
Matthew writes:

This file, when loaded, defines:

  object->address : value -> exact integer in [0,2^32-1]
  address->object : exact integer in [0,2^32-1] -> value

Obviously, address->object is not safe.

To Compile:

  mzc --cc addrhack.c
  mzc --ld addrhack.so addrhack.o

*/
#include "escheme.h"

Scheme_Object *object_to_address(int c, Scheme_Object **a)
{
  return scheme_make_integer_value_from_unsigned((unsigned long)a[0]);
}

Scheme_Object *address_to_object(int c, Scheme_Object **a)
{
  unsigned long v;

  if (!scheme_get_unsigned_int_val(a[0], &v))
    scheme_signal_error("bad address");

  return (Scheme_Object *)v;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  scheme_add_global("object->address", 
                    scheme_make_prim_w_arity(object_to_address,
                                             "object->address",
                                             1, 1),
                    env);
  scheme_add_global("address->object",
                    scheme_make_prim_w_arity(address_to_object, 
                                             "address->object",
                                             1, 1),
                    env);

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name() {
  return scheme_make_string("addrhack");
}
