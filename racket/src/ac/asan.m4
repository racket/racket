if test "${enable_asan}" = "yes" ; then
   ASAN="-fsanitize=address -fsanitize-recover=address -fno-omit-frame-pointer -fno-common"
   CFLAGS="$CFLAGS $ASAN"
   if test "${COPY_NEW_CFLAGS_TO_CPPFLAGS}" = "yes" ; then
     CPPFLAGS="$CPPFLAGS $ASAN"
     PREFLAGS="$PREFLAGS $ASAN"
   fi
   LDFLAGS="$LDFLAGS $ASAN"
fi
