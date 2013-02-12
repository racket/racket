/*
  Racket
  Copyright (c) 2004-2013 PLT Design Inc.
  Copyright (c) 2000-2001 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"
#include "schminc.h"

/* On the Mac, 68K, store the built-in Racket code as pc-relative */
#if defined(__MWERKS__)
#if !defined(__POWERPC__)
#pragma pcrelstrings on
#endif
#endif

Scheme_Object *scheme_eval_compiled_sized_string_with_magic(const char *str, int len, Scheme_Env *env,
							    Scheme_Object *magic_sym, Scheme_Object *magic_val,
							    int multi_ok)
{
  Scheme_Object *port, *expr;

  port = scheme_make_sized_byte_string_input_port(str, -len); /* negative means it's constant */

  if (!env)
    env = scheme_get_env(NULL);
    
  expr = scheme_internal_read(port, NULL, 1, 1, 0, 0, -1, NULL, 
                              magic_sym, magic_val,
                              NULL);

  if (multi_ok)
    return _scheme_eval_compiled_multi(expr, env);
  else
    return _scheme_eval_compiled(expr, env);
}

Scheme_Object *scheme_eval_compiled_sized_string(const char *str, int len, Scheme_Env *env)
{
  return scheme_eval_compiled_sized_string_with_magic(str, len, env, NULL, NULL, 0);
}

void scheme_add_embedded_builtins(Scheme_Env *env)
{
#define EVAL_ONE_STR(str) scheme_eval_module_string(str, env)
#define EVAL_ONE_SIZED_STR(str, len) scheme_eval_compiled_sized_string(str, len, env)

#if USE_COMPILED_STARTUP
# include "cstartup.inc"
#else
# include "startup.inc"
#endif
}

#if defined(__MWERKS__)
#if !defined(__POWERPC__)
#pragma pcrelstrings reset
#endif
#endif
