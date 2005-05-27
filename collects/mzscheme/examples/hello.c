/* 
   MzScheme extension example that returns the string "Hello, world!"
   when loaded.

   Compile with:
     mzc --cc hello.c
     mzc --ld hello.so hello.o
   And load with
     (load-extension "hello.so")
*/

#include "escheme.h"

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  /* When the extension is loaded, return a Scheme string: */
  return scheme_make_utf8_string("Hello, world!");
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
