
/* Example dynamically-loaded Racket extension, but not a good one.

   For example extensions, see plt/pkgs/mzscheme/mzscheme/examples. */

#include "escheme.h"
#include <time.h>

static Scheme_Object *sch_date(int argc, Scheme_Object **argv)
{
  time_t clock;
  struct tm *now;
  char *str;

  if (argc)
    scheme_wrong_count("date", 0, 0, argc, argv);

  time(&clock);
  now = localtime(&clock);
  str = asctime(now);

  /* Get rid of newline */
  str[24] = 0;

  return scheme_make_string(str);
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  scheme_add_global("date", scheme_make_prim(sch_date), env);

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_reload(env);
}
