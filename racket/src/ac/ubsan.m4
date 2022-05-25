if test "${enable_ubsan}" = "yes" ; then
   UBSAN="-fsanitize=undefined -fno-sanitize=alignment -fno-sanitize=float-divide-by-zero"
   CFLAGS="$CFLAGS $UBSAN"
   if test "${COPY_NEW_CFLAGS_TO_CPPFLAGS}" = "yes" ; then
     CPPFLAGS="$CPPFLAGS $UBSAN"
     PREFLAGS="$PREFLAGS $UBSAN"
   fi
   LDFLAGS="$LDFLAGS $UBSAN"
fi
