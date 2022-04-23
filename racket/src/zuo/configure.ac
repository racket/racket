AC_INIT([Zuo],
        [1],
        [https://github.com/racket/racket/issues],
        [],
        [https://github.com/racket/zuo])
AC_CONFIG_SRCDIR(zuo.c)

AC_ARG_ENABLE(embed,   [  --enable-embed=<libs>   embed <libs>; defaults to "zuo"])
AC_ARG_ENABLE(big,     [  --enable-big            embed "zuo/hygienic" as well as "zuo"])

if test "${enable_embed}" != "" ; then
  EMBED_LIBS="${enable_embed}"
elif test "${enable_big}" != "" ; then
  EMBED_LIBS="zuo zuo/hygienic"
else
  EMBED_LIBS="zuo"
fi

AC_PROG_MAKE_SET()
AC_PROG_CC

AC_SUBST(EMBED_LIBS)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT()

echo "#lang zuo" > main.zuo
echo "(require "'"'"${srcdir}/build.zuo"'"'")" >> main.zuo
echo "(build/command-line* targets-at at-source)" >> main.zuo
