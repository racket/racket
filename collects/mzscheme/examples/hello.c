/* 
   Racket extension example that returns the string "Hello, world!"
   when loaded.

   For the 3m GC (the default), compile with:
     mzc --xform hello.c
     mzc --3m --cc hello.3m.c
     mzc --3m --ld hello.so hello_3m.o
   And load with
     (load-extension "hello.so") ; or "hello.dylib" for Mac OS X
                                 ; or "hello.dll" for Windows

   For CGC, compile with:
     mzc --cgc --cc hello.c
     mzc --cgc --ld hello.so hello.o
   And load with
     (load-extension "hello.so") ; or "hello.dylib" for Mac OS X
                                 ; or "hello.dll" for Windows

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
