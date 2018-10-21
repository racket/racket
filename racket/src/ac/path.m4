
unixstyle=no
if test "${prefix}" != "NONE" ; then
  if test "${enable_origtree}" != "yes" ; then
    unixstyle=yes
  fi
fi
if test "${exec_prefix}" != "NONE" ; then
 unixstyle=yes
fi
if test "${bindir}" != '${exec_prefix}/bin' ; then
 unixstyle=yes
fi
if test "${datadir}" != '${prefix}/share' ; then
 # Newer autoconf uses datarootdir:
 if test "${datadir}" = '${datarootdir}' ; then
   if test "${datarootdir}" != '${prefix}/share' ; then
    unixstyle=yes
   fi
 else
   unixstyle=yes
 fi
fi
if test "${libdir}" != '${exec_prefix}/lib' ; then
 unixstyle=yes
fi
if test "${includedir}" != '${prefix}/include' ; then
 unixstyle=yes
fi
if test "${mandir}" != '${prefix}/man' ; then
 if test "${mandir}" = '${datarootdir}/man' ; then
   if test "${datarootdir}" != '${prefix}/share' ; then
    unixstyle=yes
   fi
 else
   unixstyle=yes
 fi
fi
if test "${docdir}" != '${datarootdir}/doc/${PACKAGE}' ; then
  unixstyle=yes
fi
if test "${collectsdir}" != '${exec_prefix}/share/${PACKAGE}/collects' ; then
  unixstyle=yes
fi
if test "${appsdir}" != '${exec_prefix}/share/applications' ; then
  unixstyle=yes
fi

MAKE_COPYTREE=no
if test "${unixstyle}" = "no" ; then
  if test "${prefix}" = "NONE" ; then
    inplacebuild=yes
    prefix=`cd "${srcdir}/..${PREFIX_PATH_RELATIVE}" && pwd`
  else
    MAKE_COPYTREE=copytree
  fi
  bindir='${prefix}/bin'
  libpltdir='${prefix}/lib'
  libpltdir_rel='lib'
  sharepltdir='${prefix}/share'
  etcpltdir='${prefix}/etc'
  includepltdir='${prefix}/include'
  docdir='${prefix}/doc'
  mandir='${prefix}/man'
  collectsdir='${prefix}/collects'
  appsdir='${prefix}/share/applications'
  COLLECTS_PATH="../collects"
  CONFIG_PATH="../etc"
  GR_APP_COLLECTS_PATH="../../../../collects"
  GR_APP_CONFIG_PATH="../../../../etc"
  INSTALL_ORIG_TREE=yes
else
  if test "${prefix}" = "NONE" ; then
    # Set prefix explicitly so we can use it during configure
    prefix="${ac_default_prefix}"
  fi
  libpltdir="${libdir}/"'${PACKAGE}'
  libpltdir_rel=""
  if test "${libpltdir}" = '${exec_prefix}/lib/${PACKAGE}' ; then
    if test "${bindir}" = '${exec_prefix}/bin' ; then
      libpltdir_rel="lib/"'${PACKAGE}'
    fi
  fi
  sharepltdir="${datadir}/"'${PACKAGE}'
  etcpltdir="${sysconfdir}/"'${PACKAGE}'
  includepltdir="${includedir}/"'${PACKAGE}'
  MAKE_COPYTREE=copytree
  COLLECTS_PATH='${collectsdir}'
  CONFIG_PATH='${etcpltdir}'
  GR_APP_COLLECTS_PATH="${COLLECTS_PATH}"
  GR_APP_CONFIG_PATH="${CONFIG_PATH}"
  INSTALL_ORIG_TREE=no
fi

GUI_COLLECTS_PATH="${COLLECTS_PATH}"
GUI_CONFIG_PATH="${CONFIG_PATH}"

CS_BOOTSTRAP_HELP="no-3m"

########################################

AC_SUBST(collectsdir)
AC_SUBST(appsdir)
AC_SUBST(libpltdir)
AC_SUBST(libpltdir_rel)
AC_SUBST(sharepltdir)
AC_SUBST(etcpltdir)
AC_SUBST(includepltdir)
AC_SUBST(docdir)

AC_SUBST(COLLECTS_PATH)
AC_SUBST(GUI_COLLECTS_PATH)
AC_SUBST(GR_APP_COLLECTS_PATH)
AC_SUBST(CONFIG_PATH)
AC_SUBST(GUI_CONFIG_PATH)
AC_SUBST(GR_APP_CONFIG_PATH)

AC_SUBST(MAKE_COPYTREE)
AC_SUBST(MAKE_GRACKET)
AC_SUBST(LIBFINISH)
AC_SUBST(INSTALL_ORIG_TREE)

AC_SUBST(MMM)
AC_SUBST(MMM_INSTALLED)
AC_SUBST(MMM_CAP_INSTALLED)
AC_SUBST(CGC)
AC_SUBST(CGC_INSTALLED)
AC_SUBST(CGC_CAP_INSTALLED)
AC_SUBST(MAIN_VARIANT)
AC_SUBST(MAIN_MAKE_TARGET)
AC_SUBST(CS_BOOTSTRAP_HELP)

AC_SUBST(MAKE_LOCAL_RACKET)

########################################

show_path_results()
{
  if test "${inplacebuild}" = "yes" ; then
    echo ">>> Installation is in-place:"
    echo "      ${srcdir}/.."
    echo "    Configure with --prefix if you wanted to install somewhere else."
    if test "${enable_quartz}" != "yes" ; then
      echo "    The --prefix option also makes the installed files better conform"
      echo "    to Unix installation conventions. (The configure script will show"
      echo "    you specific installation paths when --prefix is used.)"
    fi
    if test "${enable_shared}" != "yes" ; then
      echo "    Alternately, you can simply "'`'"mv' the in-place installation after"
      echo "    running "'`'"make install'."
    fi
  else
    echo ">>> Installation targets:"
    echo " executables        : ${bindir}/..."
    echo " core docs          : ${docdir}/..."
    echo " C libraries        : ${libdir}/..."
    echo " C headers          : ${includepltdir}/..."
    echo " platform libraries : ${libpltdir}/..."
    echo " common libraries   : ${sharepltdir}/..."
    echo " base collections   : ${collectsdir}/..."
    echo " configuration      : ${etcpltdir}/..."
    echo " .desktop files     : ${appsdir}/..."
    echo " man pages          : ${mandir}/..."
    echo "     where prefix = ${prefix}"
    echo "  and datarootdir = ${datarootdir}"
    if test "${unixstyle}" = "yes" ; then
      echo "  and exec_prefix = ${exec_prefix}"
      echo "  and PACKAGE = racket"
    fi
  fi
}
  
