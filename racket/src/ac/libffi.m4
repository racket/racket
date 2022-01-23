if test "${enable_libffi}" = "yes" ; then
 if test "${enable_foreign}" = "yes" ; then
  AC_MSG_CHECKING([for libffi])

  # Try to get flags form pkg-config:
  libffi_config_prog="pkg-config libffi"
  libffi_config_preflags=`$libffi_config_prog --cflags-only-I  2> /dev/null`
  if test "$?" = 0 ; then
    libffi_config_cflags=`$libffi_config_prog --cflags-only-other  2> /dev/null`
    if test "$?" = 0 ; then
      libffi_config_libs=`$libffi_config_prog --libs  2> /dev/null`
      if test "$?" != 0 ; then
        libffi_config_preflags=""
        libffi_config_cflags=""
        libffi_config_libs="-lffi"
      fi
    else
      libffi_config_preflags=""
      libffi_config_cflags=""
      libffi_config_libs="-lffi"
    fi
  else
    libffi_config_preflags=""
    libffi_config_cflags=""
    libffi_config_libs="-lffi"
  fi

  OLD_CFLAGS="${CFLAGS}"
  OLD_LIBS="${LIBS}"
  CFLAGS="${CFLAGS} ${libffi_config_preflags} ${libffi_config_cflags}"
  LIBS="${LIBS} ${libffi_config_libs}"
  AC_TRY_LINK([#include <ffi.h>],
              [ffi_cif cif; ]
              [ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 0, &ffi_type_void, NULL);],
             have_libffi=yes,
             have_libffi=no)
  AC_MSG_RESULT($have_libffi)
  if test "${have_libffi}" = "no" ; then
    CFLAGS="${OLD_CFLAGS}"
    LIBS="${OLD_LIBS}"
    echo "${libffi_unavailable_message}"
    if test "${complain_if_libffi_fails}" = "yes" ; then
       echo configure: unable to link to libffi
       exit 1
    fi
  else
    CFLAGS="${OLD_CFLAGS}"
    PREFLAGS="${PREFLAGS} ${libffi_config_preflags}"
    COMPFLAGS="${COMPFLAGS} ${libffi_config_cflags}"
    echo "Using installed libffi"
    OWN_LIBFFI="OFF"
  fi
 fi
fi
