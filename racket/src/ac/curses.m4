if test "${enable_curses}" != "no" ; then
  if test "${enable_curseslib}" != "" ; then
    LIBS="${LIBS} ${enable_curseslib}"
  fi
fi

curses_lib_flag=""
if test "${skip_curses_check}" = "no" ; then
 if test "${enable_curses}" = "yes" ; then
  AC_CHECK_HEADER(curses.h, enable_curses=yes, enable_curses=no)
  if test "${enable_curses}" = "yes" ; then
    AC_CHECK_HEADER(term.h, enable_curses=yes, enable_curses=no)
    if test "${enable_curses}" = "yes" ; then
      # Does it all work, now?
      AC_TRY_LINK(
      [#include <curses.h>]
      [#include <term.h>],
[      int errret; ]
[      setupterm("", 0, &errret);]
       return 0;
       , enable_curses=yes, enable_curses=no)
      if test "${enable_curses}" = "no" ; then
        # Try adding -lncurses
        ORIG_LIBS="$LIBS"
        LIBS="$LIBS -lncurses"
        AC_TRY_LINK(
        [#include <curses.h>]
        [#include <term.h>],
[        int errret; ]
[        setupterm("", 0, &errret);]
         return 0;
         , enable_curses=yes, enable_curses=no)
        if test "${enable_curses}" = "no" ; then
          LIBS="$ORIG_LIBS"
        else
          curses_lib_flag=" -lncurses"
        fi
      fi
    fi
  fi
 fi
 [ msg="curses is usable" ]
 AC_MSG_CHECKING($msg)
 curses_usage_result="$enable_curses$curses_lib_flag"
 AC_MSG_RESULT($curses_usage_result)
fi

if test "${enable_curses}" = "no" ; then
  add_curses_lib=
  disable_curses_arg=--disable-curses
else
  disable_curses_arg=
fi

if test "${add_curses_lib}" != "" ; then
    LIBS="${LIBS} ${add_curses_lib}"
fi
