# If using gcc, we'll want all warnings

changequote(<<, >>)dnl
is_gcc=`$CC -v 2>&1 | tail -n 1 | grep -E '^gcc version [[:digit:]]+\.[[:digit:]]+\.[[:digit:]]+ \([^\)]+\)[[:space:]]*$'`
changequote([, ])dnl

if test "$is_gcc" = "" ; then 
  if test "`basename $CC`" = "gcc" ; then
    is_gcc=yes
  fi
fi
