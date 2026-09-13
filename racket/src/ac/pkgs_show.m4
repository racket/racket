show_pkg_catalog="no"

if test "$enable_pkgs" != ""  ; then
  echo "=== Adding extra packages on install: $enable_pkgs"
  show_pkg_catalog="yes"
fi

if test "$enable_missingpkgs" == "yes"  ; then
  echo "=== Adding any missing package dependencies on install"
  show_pkg_catalog="yes"
fi

if test "$show_pkg_catalog" != ""  ; then
  if test "$enable_catalog" != "" ; then
    echo "=== Installing packages from: $enable_catalog"
  fi
fi
