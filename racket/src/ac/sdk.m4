
enable_quartz=no
if test "${enable_xonx}" = "yes" ; then
  enable_quartz=no
else
  case "$host_os" in
  darwin*)
    enable_quartz=yes
    if test "${enable_origtree}" != "no" ; then
      enable_origtree=yes
    fi
    if test "${prefix}" != "NONE" ; then
      if test "${enable_macprefix}" != "yes" ; then
        echo "ERROR: --prefix not allowed for a Mac OS build, unless either"
        echo "         --enable-xonx is supplied (to create a Unix-style"
        echo "           build), or "
        echo "         --enable-macprefix is supplied (to allow a Mac-style"
        echo "           installation, even though --prefix is normally used"
        echo "           for Unix-style installations)"
        exit 1
      fi
    fi
  ;;
  esac
fi
