if test "${enable_ubsan}" = "yes" ; then
   UBSAN="-fsanitize=undefined -fno-sanitize=alignment -fno-sanitize=float-divide-by-zero"
   CFLAGS="$CFLAGS $UBSAN"
   CPPFLAGS="$CPPFLAGS $UBSAN"
   PREFLAGS="$PREFLAGS $UBSAN"
   LDFLAGS="$LDFLAGS $UBSAN"
fi
