/* Like hello.c, but prints to the current output port and returns
   (void). */

#include "escheme.h"

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  /* Make the string: */
  Scheme_Object *hw;
  hw = scheme_make_utf8_string("Hello, World!\n");

  /* Display it: */
  scheme_display(hw, scheme_get_param(scheme_current_config(), 
				      MZCONFIG_OUTPUT_PORT));

  /* Why not just
        printf("Hello, World!\n");
     ? That would write to stdout, which may or may not be the same as
     the current output port. But sometimes printf() is what you
     want. */

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
