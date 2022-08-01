# Process this file with autoconf to produce a configure script.
AC_INIT([Zuo],
        [1.0],
        [https://github.com/racket/racket/issues],
        [],
        [https://github.com/racket/zuo])
AC_CONFIG_SRCDIR(zuo.c)

AC_ARG_ENABLE(embed,
              [AS_HELP_STRING([--enable-embed=<libs>],
                              [embed <libs>; defaults to "zuo"])],
              []
              [: m4_divert_text([DEFAULTS], [enable_embed="zuo"])])
AC_ARG_ENABLE(big,[AS_HELP_STRING([--enable-big],
                                  [embed "zuo/hygienic" as well as "zuo"])])

EMBED_LIBS="zuo"
AS_IF([test "x$enable_embed" = xno],
      [EMBED_LIBS=""],
      [test "x$enable_big" = xyes],
      [EMBED_LIBS="zuo zuo/hygienic"],
      [test "x$enable_embed" != xyes],
      [EMBED_LIBS="${enable_embed}"])

AC_PROG_MAKE_SET()
AC_PROG_CC

if test "${CC_FOR_BUILD}" = ""; then
  CC_FOR_BUILD='$(CC) -O2'
  CPPFLAGS_FOR_BUILD='$(CPPFLAGS)'
  CFLAGS_FOR_BUILD='$(CFLAGS)'
  LDFLAGS_FOR_BUILD='$(LDFLAGS)'
  LIBS_FOR_BUILD='$(LIBS)'
fi
AC_ARG_VAR(CC_FOR_BUILD,[build system C compiler command])
AC_ARG_VAR(CFLAGS_FOR_BUILD,[build system C compiler flags])
AC_ARG_VAR(LDFLAGS_FOR_BUILD,[build system linker flags])
AC_ARG_VAR(LIBS_FOR_BUILD,[build system libraries for linker])
AC_ARG_VAR(CPPFLAGS_FOR_BUILD,[build system preprocessor flags])

AC_SUBST(EMBED_LIBS)
AC_MSG_NOTICE([zuo libraries to embed: "${EMBED_LIBS}"])

AC_CONFIG_FILES([Makefile main.zuo])
AC_OUTPUT()
