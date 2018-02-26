
#################################################################
# This is the source for the `configure` script, to be compiled #
# by autoconf (use `make-configure` in "../../racket").         #
#################################################################

AC_INIT([embed-boot.rkt])
AC_CONFIG_HEADERS([cs_config.h])

AC_CONFIG_AUX_DIR(../../lt)
AC_CANONICAL_SYSTEM

AC_ARG_ENABLE(shared,     [  --enable-shared         create shared libraries (ok, but not recommended)])
AC_ARG_ENABLE(standalone, [  --enable-standalone     create a standalone shared library])
AC_ARG_ENABLE(pthread,    [  --enable-pthread        link with pthreads (usually auto-enabled if needed)])
AC_ARG_ENABLE(iconv,      [  --enable-iconv          use iconv (usually auto-enabled)])
AC_ARG_ENABLE(xonx,       [  --enable-xonx           use Unix style (e.g., use Gtk) for Mac OS])
AC_ARG_ENABLE(racket,     [  --enable-racket=<path>  use <path> as Racket to build; or "auto" to create])
AC_ARG_ENABLE(scheme,     [  --enable-scheme=<path>  Chez Scheme build directory at <path>])
AC_ARG_ENABLE(mach,       [  --enable-mach=<mac>     Use Chez Scheme machine type <mach>])

show_explicitly_disabled()
{
  if test "$1" = "no" ; then
    echo "=== $2 disabled"
  fi
}

show_explicitly_enabled()
{
  if test "$1" = "yes" ; then
    echo "=== $2 enabled"
    if test "$3" != "" ; then
      echo "  $3"
    fi
  fi
}

show_explicitly_set()
{
  if test "$1" != "" ; then
    echo "=== $2 set to $1"
  fi
}

show_explicitly_enabled "${enable_pthread}" "pthreads"
show_explicitly_disabled "${enable_pthread}" "pthreads"
show_explicitly_enabled "${enable_xonx}" "Unix style"
show_explicitly_set "${enable_racket}" "Racket"
show_explicitly_set "${enable_scheme}" "Chez Scheme build directory"
show_explicitly_set "${enable_mach}" "machine type"

if test "${enable_iconv}" = "" ; then
  enable_iconv=yes
fi

if test "${enable_xonx}" == "" ; then
  enable_xonx=no
fi

skip_iconv_check=no
use_flag_pthread=yes
use_flag_posix_pthread=no

INCLUDEDEP="#"
OSX="not_osx"
NOT_OSX=""
CONFIGURE_RACKET_SO_COMPILE=""

FRAMEWORK_INSTALL_DIR='$(srcdir)/../../../lib/'
FRAMEWORK_PREFIX='@executable_path/../lib/'

RACKET='$(DEFAULT_RACKET)'

enable_pthread_by_default=yes

###### Autoconfigure #######

AC_PROG_CC

# If using gcc, we want all warnings:
if test "$CC" = "gcc" ; then 
  CFLAGS="$CFLAGS -Wall"
fi

AC_CHECK_LIB(m, fmod)
AC_CHECK_LIB(dl, dlopen)

############## platform tests ################

case "$host_os" in
  solaris2*)
    MACH_OS=s2
    ;;
  aix*)
    ;;
  *freebsd*)
    MACH_OS=fb
    ;;
  openbsd*)
    MACH_OS=ob
    ;;
  bitrig*)
    ;;
  dragonfly*)
    ;;
  netbsd*)
    MACH_OS=nb
    ;;
  irix*)
    ;;
  linux*)
    MACH_OS=le
    LIBS="${LIBS} -lncurses"
    ;;
  osf1*)
    ;;
  hpux*)
    ;;
  *mingw*)
    skip_iconv_check=yes
    ;;    
  cygwin*)
    ;;
  darwin*)
    PREFLAGS="$PREFLAGS -DOS_X"
    MACH_OS=osx
    LIBS="${LIBS} -lncurses -framework CoreFoundation"
    if test "${enable_xonx}" == "no" ; then
      INCLUDEDEP="-include"
      OSX=""
      NOT_OSX="osx"
    else
      CONFIGURE_RACKET_SO_COMPILE="env PLT_CS_MAKE_UNIX_STYLE_MACOS=y"
      CPPFLAGS="${CPPFLAGS} -DRACKET_XONX"
    fi

    # -pthread is not needed and triggers a warning
    use_flag_pthread=no
    ;;
  nto-qnx*)
    MACH_OS=qnx
    use_flag_pthread=no
    ;;
  *)
    ;;
esac

case "$host_cpu" in
  arm*)
    enable_pthread_by_default=no
    ;;
esac

if test "${enable_pthread}" = "" ; then
  if test "${enable_pthread_by_default}" = "yes" ; then
    enable_pthread=yes
  fi
fi

thread_prefix=""
thread_config_arg=""
if test "${enable_pthread}" = "yes" ; then
  thread_prefix="t"
  thread_config_arg="--threads"
fi

case "$host_cpu" in
  x86_64)
    MACH="${thread_prefix}a6${MACH_OS}"
    ;;
  x86|i*86)
    MACH="${thread_prefix}i3${MACH_OS}"
    ;;
  arm*)
    MACH="${thread_prefix}arm32${MACH_OS}"
    ;;
  power*)
    MACH="${thread_prefix}ppc32${MACH_OS}"
    ;;
esac

SCHEME_SRC=../ChezScheme

if test "${enable_scheme}" != "" ; then
  SCHEME_SRC="${enable_scheme}"
fi

if test "${enable_racket}" != "" ; then
  RACKET="${enable_racket}"
fi

if test "${enable_mach}" != "" ; then
  MACH="${enable_mach}"
fi

SCHEME_CONFIG_ARGS="--machine=${MACH} ${thread_config_arg}"

############## C flags ################

AC_LANG_C

iconv_lib_flag=""
if test "${skip_iconv_check}" = "no" ; then
 if test "${enable_iconv}" = "yes" ; then
  AC_CHECK_HEADER(iconv.h, enable_iconv=yes, enable_iconv=no)
  if test "${enable_iconv}" = "yes" ; then  
    # Does it all work, now?
    AC_TRY_RUN(
[   #include <iconv.h>]
[   #include <langinfo.h>]
    int main() {
[     iconv_open("UTF-8", "UTF-8");]
      return 0;
    }, enable_iconv=yes, enable_iconv=no, enable_iconv=yes)
    if test "${enable_iconv}" = "no" ; then  
      # Try adding -liconv ?
      #  We did not use AC_CHECK_LIB because iconv is sometimes macro-renamed
      ORIG_LIBS="$LIBS"
      LIBS="$LIBS -liconv"
      AC_TRY_RUN(
[     #include <iconv.h>]
[     #include <langinfo.h>]
      int main() {
[     iconv_open("UTF-8", "UTF-8");]
      return 0;
      }, enable_iconv=yes, enable_iconv=no, enable_iconv=yes)
      if test "${enable_iconv}" = "no" ; then
        LIBS="$ORIG_LIBS"
      else
        iconv_lib_flag=" -liconv"      
      fi
    fi
  fi
 fi
 [ msg="iconv is usable" ]
 AC_MSG_CHECKING($msg)
 iconv_usage_result="$enable_iconv$iconv_lib_flag"
 AC_MSG_RESULT($iconv_usage_result)
fi
if test "${enable_iconv}" = "no" ; then
  AC_DEFINE(RKTIO_NO_ICONV,1,[Do not use iconv])
fi

if test "${enable_iconv}" = "yes" ; then
  AC_MSG_CHECKING([for nl_langinfo (CODESET)])
  AC_TRY_LINK([#include <langinfo.h>],
              [char *codeset = nl_langinfo (CODESET);],
              AC_DEFINE(RKTIO_HAVE_CODESET,1,[Have nl_langinfo (CODESET)])
               have_codeset=yes,
              have_codeset=no)
  AC_MSG_RESULT($have_codeset)
fi

############### pthread ###################

if test "${enable_pthread}" = "yes" ; then
  if test "${use_flag_pthread}" = "yes" ; then
    PREFLAGS="$PREFLAGS -pthread"
    LDFLAGS="$LDFLAGS -pthread"
  fi
  if test "${use_flag_posix_pthread}" = "yes" ; then
    PREFLAGS="$PREFLAGS -D_POSIX_PTHREAD_SEMANTICS -D_REENTRANT"
  fi
  AC_DEFINE(RKTIO_USE_PTHREADS, 1, [Pthread enabled])
fi

############## libtool ################

if test "${enable_shared}" = "yes" ; then
  echo "Configuring libtool"

  # Assuming an absolute "${libdir}":
  abslibdir="${libdir}"

  if test ! -d "../../lt" ; then
    mkdir "../../lt"
  fi
  abssrcdir=`(cd ${srcdir}; pwd)`

  if test "${LIBTOOLPROG}" = "" ; then  
    (cd ../lt; sh ${abssrcdir}/../lt/configure --enable-shared --disable-static)
    LIBTOOLPROG=`pwd`/../lt/libtool
  fi

  if test "${need_gcc_static_libgcc}" = "yes" ; then
    need_gcc_static_libgcc=""
    if test "$CC" = "gcc" ; then 
      gcc_vers_three=`${CC} -v 2>&1 | grep "version 3[.]"`
      if test "$gcc_vers_three" = "" ; then
        need_gcc_static_libgcc=""
      else
        need_gcc_static_libgcc=" -XCClinker -static-libgcc"
      fi
    fi
  fi

  # Use only for standalone builds:
  AR="${LIBTOOLPROG} --mode=link --tag=CC $CC${need_gcc_static_libgcc}${ar_libtool_no_undefined} -release ${rktio_version} -rpath ${abslibdir} \$(ARLIBFLAGS) -o"
  # Used for a librktio convenience library:
  STATIC_AR="${LIBTOOLPROG} --mode=link --tag=CC $CC -o"
  ARFLAGS=""
  RANLIB=":"
  
  RKTLINKER="${LIBTOOLPROG} --mode=link --tag=CC $CC${need_gcc_static_libgcc}"
  CC="${LIBTOOLPROG} --mode=compile --tag=CC $CC"
  LTO="lo"
  LTA="la"
  STRIP_LIB_DEBUG=":"
  LIBSFX=la
  ICP_LIB="${LIBTOOLPROG} --mode=install install -s"
else
  ICP=cp
  LTO="o"
  LTA="a"
  RKTLINKER='$(CC)'
  STATIC_AR="$AR"
  LIBSFX=so
  ICP_LIB=cp
  if test "${make_darwin_dylib}" = "yes" ; then
     LIBSFX="dylib"
     AR='$(RKTLINKER) --shared -o'
     ARFLAGS=""
     LIBS="${LIBS} -framework CoreFoundation"
     ICP_LIB=cp
  fi
fi

############## final output ################

CPPFLAGS="$CPPFLAGS $PREFLAGS"

AC_SUBST(CC)
AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(LIBS)
AC_SUBST(AR)
AC_SUBST(ARFLAGS)
AC_SUBST(RANLIB)
AC_SUBST(STATIC_AR)
AC_SUBST(ICP)
AC_SUBST(RKTLINKER)
AC_SUBST(INCLUDEDEP)
AC_SUBST(RACKET)
AC_SUBST(SCHEME_DIR)
AC_SUBST(MACH)
AC_SUBST(OSX)
AC_SUBST(NOT_OSX)
AC_SUBST(CONFIGURE_RACKET_SO_COMPILE)
AC_SUBST(SCHEME_SRC)
AC_SUBST(SCHEME_CONFIG_ARGS)
AC_SUBST(FRAMEWORK_INSTALL_DIR)
AC_SUBST(FRAMEWORK_PREFIX)

makefiles="Makefile"

AC_OUTPUT($makefiles)

mkdir -p rktio
abssrcdir=`(cd ${srcdir}; pwd)`
echo "=== configuring in rktio (${abssrcdir}/../../rktio)"
cd rktio; ${abssrcdir}/../../rktio/configure
