
# The "filter_add.sh" script is prefiexed onto
# autoconf output, and then we handle the variables
# potentially built up, adding contents to the
# default flag sets

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
