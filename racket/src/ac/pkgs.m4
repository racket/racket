INSTALL_PKGS="$enable_pkgs"
INSTALL_PKG_CATALOG="$enable_catalog"

if test "$enable_missingpkgs" = "" ; then
  if test "$dist_install_missing_pkgs" = "yes" ; then
    INSTALL_MISSING_PKGS="dist"
  fi
else
  INSTALL_MISSING_PKGS="$enable_missingpkgs"
fi
