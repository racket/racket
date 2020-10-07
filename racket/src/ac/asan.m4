if test "${enable_asan}" = "yes" ; then
   ASAN="-fsanitize=address -fsanitize-recover=address -fno-omit-frame-pointer -fno-common"
   CFLAGS="$CFLAGS $ASAN"
   CPPFLAGS="$CPPFLAGS $ASAN"
   PREFLAGS="$PREFLAGS $ASAN"
   LDFLAGS="$LDFLAGS $ASAN"
fi

