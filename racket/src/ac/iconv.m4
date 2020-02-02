iconv_lib_flag=""
if test "${skip_iconv_check}" = "no" ; then
 if test "${enable_iconv}" = "yes" ; then
  AC_CHECK_HEADER(iconv.h, enable_iconv=yes, enable_iconv=no)
  if test "${enable_iconv}" = "yes" ; then
    # Does it all work, now?
    AC_TRY_LINK(
    [#include <iconv.h>]
    [#include <langinfo.h>],
[     iconv_open("UTF-8", "UTF-8");]
      return 0;
    , enable_iconv=yes, enable_iconv=no)
    if test "${enable_iconv}" = "no" ; then
      # Try adding -liconv ?
      #  We did not use AC_CHECK_LIB because iconv is sometimes macro-renamed
      ORIG_LIBS="$LIBS"
      LIBS="$LIBS -liconv"
      AC_TRY_LINK(
      [#include <iconv.h>]
      [#include <langinfo.h>],
[     iconv_open("UTF-8", "UTF-8");]
      return 0;
      , enable_iconv=yes, enable_iconv=no)
      if test "${enable_iconv}" = "no" ; then
        LIBS="$ORIG_LIBS"
      else
        iconv_lib_flag=" -liconv"
      fi
    fi
  fi
 fi
 [ msg="iconv is usable" ]
 AC_MSG_CHECKING($msg)
 iconv_usage_result="$enable_iconv$iconv_lib_flag"
 AC_MSG_RESULT($iconv_usage_result)
fi
