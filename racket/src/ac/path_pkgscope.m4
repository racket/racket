INSTALL_PKGSCOPE=user
MAKE_INSTALL_PKGSCOPE=preserve
if test "${enable_pkgscope}" != "" ; then
  case "${enable_pkgscope}" in
   installation)
     INSTALL_PKGSCOPE=installation
     ;;
   user)
     INSTALL_PKGSCOPE=user
     ;;
   shared)
     INSTALL_PKGSCOPE=shared
     ;;
   *)
     echo "Unrecognized package scope: ${enable_pkgscope}"
     exit 1
     ;;
  esac
  echo "=== Package scope: " $INSTALL_PKGSCOPE
  MAKE_INSTALL_PKGSCOPE=adjust
fi

AC_SUBST(INSTALL_PKGSCOPE)
AC_SUBST(MAKE_INSTALL_PKGSCOPE)
