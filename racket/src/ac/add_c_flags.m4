
# The "filter_add.sh" script is inserted into autoconf output just
# before the generated configure script handles arguments. The
# configure script later handles the variables potentially built up
# here, adding contents to the default or initially configured flag
# sets.

if test "$CPPFLAGS_ADD" != "" ; then
   CPPFLAGS="$CPPFLAGS $CPPFLAGS_ADD"
fi

if test "$CFLAGS_ADD" != "" ; then
   CFLAGS="$CFLAGS $CFLAGS_ADD"
fi

if test "$LDFLAGS_ADD" != "" ; then
   LDFLAGS="$LDFLAGS $LDFLAGS_ADD"
fi

if test "$LIBS_ADD" != "" ; then
   LIBS="$LIBS $LIBS_ADD"
fi
