/*
  Extension that uses the curses library.

  Link the extension to the curses library like this:
     mzc --xform curses.c
     mzc --3m --cc curses.3m.c
     mzc --3m --ld curses.so curses_3m.o -lcurses

  For obvious reasons, this library doesn't interact well
  with Racket's read-eval-print loop. The example file
  curses-demo.ss demos this extension.
*/

#include "escheme.h"
#include <curses.h>

/**************************************************/

static Scheme_Object *sch_clear(int argc, Scheme_Object **argv)
{
  clear();
}

static Scheme_Object *sch_put(int argc, Scheme_Object **argv)
{
  /* Puts a char or string on the screen */
  if (SCHEME_CHARP(argv[0]))
    addch(SCHEME_CHAR_VAL(argv[0]));
  else if (SCHEME_BYTE_STRINGP(argv[0]))
    addstr(SCHEME_BYTE_STR_VAL(argv[0]));
  else if (SCHEME_CHAR_STRINGP(argv[0])) {
    Scheme_Object *bs;
    bs = scheme_char_string_to_byte_string(argv[0]);
    addstr(SCHEME_BYTE_STR_VAL(bs));
  } else
    scheme_wrong_type("put", "character, string, or byte string", 0, argc, argv);

  return scheme_void;
}

static Scheme_Object *sch_get(int argc, Scheme_Object **argv)
{
  /* Gets keyboard input */
  int c;
  c = getch();
  return scheme_make_character(c);
}

static Scheme_Object *sch_move(int argc, Scheme_Object **argv)
{
  /* Move the output cursor */
  if (!SCHEME_INTP(argv[0]))
    scheme_wrong_type("move", "exact integer", 0, argc, argv);
  if (!SCHEME_INTP(argv[1]))
    scheme_wrong_type("move", "exact integer", 1, argc, argv);

  move(SCHEME_INT_VAL(argv[0]), SCHEME_INT_VAL(argv[1]));

  return scheme_void;
}

static Scheme_Object *sch_get_size(int argc, Scheme_Object **argv)
{
  /* Returns two values */
  int w, h;
  Scheme_Object *a[2];

  w = getmaxx(stdscr);
  h = getmaxy(stdscr);

  a[0] = scheme_make_integer(w);
  a[1] = scheme_make_integer(h);
  return scheme_values(1, a);
}

static Scheme_Object *sch_refresh(int argc, Scheme_Object **argv)
{
  refresh();
  return scheme_void;
}

/**************************************************/

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  /* The MZ_GC... lines are for for 3m, because env is live across an
     allocating call. They're not needed for plain old (conservatively
     collected) Mzscheme. See makeadder3m.c for more info. */
  Scheme_Object *v;
  /* Old annotations, are they needed?
  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, env);
  MZ_GC_REG();
  */

  v = scheme_make_prim_w_arity(sch_clear, "clear", 0, 0),
  scheme_add_global("clear", v, env);

  v = scheme_make_prim_w_arity(sch_put, "put", 1, 1);
  scheme_add_global("put", v, env);

  v = scheme_make_prim_w_arity(sch_get, "get", 0, 0);
  scheme_add_global("get", v, env);

  v = scheme_make_prim_w_arity(sch_move, "move", 2, 2);
  scheme_add_global("move", v, env);

  v = scheme_make_prim_w_arity(sch_get_size, "get-size", 0, 0);
  scheme_add_global("get-size", v, env);

  v = scheme_make_prim_w_arity(sch_refresh, "refresh", 0, 0);
  scheme_add_global("refresh", v, env);

  MZ_GC_UNREG();

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  /* The first time we're loaded, initialize the screen: */
  initscr();
  cbreak();
  noecho();
  atexit(endwin);

  /* Then do the usual stuff: */
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  /* This extension doesn't define a module: */
  return scheme_false;
}
