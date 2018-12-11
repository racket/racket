
AC_ARG_ENABLE(origtree,  [  --enable-origtree       install with original directory structure])
AC_ARG_ENABLE(pkgscope,  [  --enable-pkgscope=<s>   set `raco pkg' default: installation, user, or shared])

AC_ARG_ENABLE(docs,      [  --enable-docs           build docs on install (enabled by default)], , enable_docs=yes)
AC_ARG_ENABLE(usersetup, [  --enable-usersetup      setup user-specific files on install])
