
#################################################################
# This is the source for the `configure` script, to be compiled #
# by autoconf (use `make-configure` in "../../ac").             #
#################################################################

AC_INIT([embed-boot.rkt])
AC_CONFIG_HEADERS([cs_config.h])

AC_CONFIG_AUX_DIR(../../lt)
AC_CANONICAL_SYSTEM

AC_DISABLE_OPTION_CHECKING

AC_ARG_ENABLE(shared,     [  --enable-shared         create shared libraries (ok, but not recommended)])
AC_ARG_ENABLE(standalone, [  --enable-standalone     create a standalone shared library])
AC_ARG_ENABLE(pthread,    [  --enable-pthread        link with pthreads (usually auto-enabled if needed)])
AC_ARG_ENABLE(iconv,      [  --enable-iconv          use iconv (usually auto-enabled)])
AC_ARG_ENABLE(compress,   [  --enable-compress       compress compiled code (enabled by default)])
m4_include(../ac/path_arg.m4)
AC_ARG_ENABLE(racket,     [  --enable-racket=<path>  use <path> as Racket to build; or "auto" to create])
AC_ARG_ENABLE(scheme,     [  --enable-scheme=<path>  Chez Scheme build directory at <path>])
AC_ARG_ENABLE(mach,       [  --enable-mach=<mach>    Use Chez Scheme machine type <mach>])
AC_ARG_ENABLE(target,     [  --enable-target=<mach>  Cross-build for Chez Scheme machine type <mach>])
m4_include(../ac/sdk_arg.m4)
m4_include(../ac/strip_arg.m4)
AC_ARG_ENABLE(csdefault,  [  --enable-csdefault      use CS as default build])
AC_ARG_ENABLE(csonly,     [  --enable-csonly         build CS only])
AC_ARG_ENABLE(parent,     [  --enable-parent         Create "../Makefile" (internal use)])
m4_include(../ac/crossany_arg.m4)

m4_include(../ac/sdk.m4)

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
show_explicitly_disabled "${enable_compress}" "Compressed code"
show_explicitly_enabled "${enable_xonx}" "Unix style"
show_explicitly_set "${enable_racket}" "Racket"
show_explicitly_set "${enable_scheme}" "Chez Scheme build directory"
show_explicitly_set "${enable_mach}" "machine type"
show_explicitly_set "${enable_target}" "cross-build machine type"

if test "${enable_csonly}" = "yes" ; then
  enable_csdefault=yes
fi
show_explicitly_enabled "${enable_csdefault}" "executables without suffix"

m4_include(../ac/sdk_show.m4)
m4_include(../ac/strip_show.m4)

if test "${enable_iconv}" = "" ; then
  enable_iconv=yes
fi

if test "${enable_xonx}" == "" ; then
  enable_xonx=no
fi

m4_include(../ac/path_pkgscope.m4)

m4_include(../ac/crossany.m4)

SUB_CONFIGURE_EXTRAS=


CS_INSTALLED=cs
if test "${enable_csdefault}" = "yes" ; then
  CS_INSTALLED=""
fi

CS_COMPILED_SUBDIR=1
COMP_SUBDIR_CONFIGURE_ARGS=
if test "${enable_csdefault}" = "yes" ; then
  CS_COMPILED_SUBDIR=0
  COMP_SUBDIR_CONFIGURE_ARGS=--enable-csdefault
fi

############## Install targets ################

PREFIX_PATH_RELATIVE=/../..

m4_include(../ac/path.m4)

############## init variables ################

skip_iconv_check=no
use_flag_pthread=yes
use_flag_posix_pthread=no

INCLUDEDEP="#"
OSX="not_osx"
NOT_OSX=""
MINGW="not_mingw"
NOT_MINGW=""
CONFIGURE_RACKET_SO_COMPILE=""
COMPRESS_COMP=""
ELF_COMP=""

FRAMEWORK_INSTALL_DIR='$(libpltdir)'
FRAMEWORK_PREFIX='@executable_path/../lib/'

RACKET='$(DEFAULT_RACKET)'
RUN_RACKET='$(RUN_THIS_RACKET)'

INSTALL_SETUP_FLAGS=
INSTALL_SETUP_RACKET_FLAGS=

enable_pthread_by_default=yes

###### Autoconfigure #######

AC_PROG_CC

# If using gcc, we want all warnings:
if test "$CC" = "gcc" ; then 
  CFLAGS="$CFLAGS -Wall"
fi

AC_CHECK_LIB(m, fmod)
AC_CHECK_LIB(dl, dlopen)

AC_PROG_RANLIB
if test "$AR" = '' ; then 
  AR="${ac_tool_prefix}ar"
  AC_CHECK_PROG(platform_ar_found, $AR, yes, no)
  if test "$platform_ar_found" = 'no' ; then
    AR="ar"
  fi
fi
if test "$ARFLAGS" = '' ; then 
  ARFLAGS=ruv
fi

LD="${ac_tool_prefix}ld"

WINDRES=windres

############## platform tests ################

# At first, `MACH` means the target that we're building for. In
# cross-build mode, `MACH` will be moved to `TARGET_MACH`.

MACH_HOST_CPU="${host_cpu}"
extra_scheme_config_args=
disable_curses_arg=--disable-curses

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
    LIBS="${LIBS} -lrt -luuid"
    CPPFLAGS="${CPPFLAGS} -DELF_FIND_BOOT_SECTION"
    ELF_COMP="--expect-elf"
    ;;
  osf1*)
    ;;
  hpux*)
    ;;
  *mingw*)
    skip_iconv_check=yes
    use_flag_pthread=no
    MACH_OS=nt
    MINGW=""
    NOT_MINGW="mingw"
    if `which ${host}-windres > /dev/null` ; then
      WINDRES="${host}-windres"
    fi
    ;;
  cygwin*)
    ;;
  darwin*)
    PREFLAGS="$PREFLAGS -DOS_X"
    MACH_OS=osx
    LIBS="${LIBS} -lncurses -framework CoreFoundation"
    LDFLAGS="${LDFLAGS} -Wl,-headerpad_max_install_names"
    if test "${enable_xonx}" == "no" ; then
      INCLUDEDEP="-include"
      OSX=""
      NOT_OSX="osx"
    else
      CONFIGURE_RACKET_SO_COMPILE="env PLT_CS_MAKE_UNIX_STYLE_MACOS=y"
      CPPFLAGS="${CPPFLAGS} -DRACKET_XONX"
      strip_needs_dash_s=yes
    fi

    m4_include(../ac/sdk_mac.m4)

    if test "${enable_libfw}" = "yes" ; then
      FRAMEWORK_INSTALL_DIR=/Library/Frameworks
      FRAMEWORK_REL_INSTALL=no
      FRAMEWORK_PREFIX=''
    elif test "${enable_userfw}" = "yes" ; then
      FRAMEWORK_INSTALL_DIR=~/Library/Frameworks
      FRAMEWORK_REL_INSTALL=no
      FRAMEWORK_PREFIX=''
    else
      FRAMEWORK_INSTALL_DIR='${libpltdir}'
      FRAMEWORK_REL_INSTALL=yes
      if test "${libpltdir_rel}" = "" ; then
        FRAMEWORK_PREFIX='$(FRAMEWORK_ABS_PREFIX)'
      else
        FRAMEWORK_PREFIX='$(FRAMEWORK_REL_PREFIX)'
      fi
    fi

    if test "${enable_mac64}" == "no"; then
      MACH_HOST_CPU="x86"
      extra_scheme_config_args="--32"
    fi

    # -pthread is not needed and triggers a warning
    use_flag_pthread=no

    # ncurses.h is always available
    disable_curses_arg=""
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
if test "${enable_pthread}" = "yes" ; then
  thread_prefix="t"
fi

case "$MACH_HOST_CPU" in
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

if test "${enable_mach}" != "" ; then
  MACH="${enable_mach}"
fi

if test "${enable_target}" != "" ; then
  TARGET_MACH=${enable_target}
  CROSS_MODE="cross"
elif test "${build_os}_${build_cpu}" != "${host_os}_${host_cpu}" ; then
  BUILD_THREAD_PREFIX="${thread_prefix}"
  case "$build_os" in
    solaris2*)
      BUILD_OS=s2
      ;;
    *freebsd*)
      BUILD_OS=fb
      ;;
    openbsd*)
      BUILD_OS=ob
      ;;
    netbsd*)
      BUILD_OS=nb
      ;;
    linux*)
      BUILD_OS=le
      ;;
    *mingw*)
      BUILD_OS=nt
      ;;
    darwin*)
      BUILD_OS=osx
      ;;
    nto-qnx*)
      BUILD_OS=qnx
      ;;
    *)
      echo "unknown build OS"
      exit 1
      ;;
  esac
  case "$build_cpu" in
    x86_64)
      BUILD_MACH="${BUILD_THREAD_PREFIX}a6${BUILD_OS}"
      ;;
    x86|i*86)
      BUILD_MACH="${BUILD_THREAD_PREFIX}i3${BUILD_OS}"
      ;;
    arm*)
      BUILD_MACH="${BUILD_THREAD_PREFIX}arm32${BUILD_OS}"
      ;;
    power*)
      BUILD_MACH="${BUILD_THREAD_PREFIX}ppc32${BUILD_OS}"
      ;;
  esac
  TARGET_MACH=${MACH}
  MACH=${BUILD_MACH}
  CROSS_MODE="cross"
else
  BUILD_MACH=${MACH}
  TARGET_MACH=${MACH}
  CROSS_MODE=""
fi

if test "${CROSS_MODE}" = "cross" ; then
  T_CROSS_MODE="-cross"
  TT_CROSS_MODE="--cross"
  RUN_RACKET='$(RACKET)'
else
  T_CROSS_MODE=""
  TT_CROSS_MODE=""
fi

# From this point on, `MACH` means the build machine, and
# `TARGET_MACH` is the target that we're building for (which
# is different in cross-build mode).

SCHEME_SRC=../../ChezScheme
MAKE_BUILD_SCHEME=y

if test "${enable_scheme}" != "" ; then
  SCHEME_SRC="${enable_scheme}"
  MAKE_BUILD_SCHEME=n
fi

if test "${enable_racket}" != "" ; then
  RACKET="${enable_racket}"
fi

SCHEME_CONFIG_ARGS="--machine=${MACH} --disable-x11 ${disable_curses_arg} ${extra_scheme_config_args}"
SCHEME_CROSS_CONFIG_ARGS="--machine=${TARGET_MACH} --disable-x11 ${disable_curses_arg} ${extra_scheme_config_args}"

if test "${enable_compress}" != "no" ; then
  COMPRESS_COMP="--compress"
fi

DIFF_MACH=skip
if test "${MACH}" != "${TARGET_MACH}" ; then
  DIFF_MACH=
fi

############## Strip tool ################

m4_include(../ac/strip.m4)

############## Makefile includes ################

if test "$INCLUDEDEP" = "#" ; then
  is_gmake=`make -v no-such-target-we-hope 2>&1 | grep "GNU Make"`
  if test "$is_gmake" = "" ; then
     INCLUDEDEP="-include"
  fi
fi

if test "$INCLUDEDEP" != "#" ; then
  RKTL_PRE="compiled/"
  RKTL_POST=".rktl"
fi

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

# Pass certain configure args on to rktio
keep_configure_args=
fixup_prev=
eval "set x $ac_configure_args"
shift
for fixup_arg
do
  case $fixup_arg in
  # Strip away all feature choices
  -enable* | --enable* | -disable* | --disable*)
    ;;
  *)
    case $fixup_arg in
    *\'*) fixup_arg=`echo "$fixup_arg" | sed "s/'/'\\\\\\\\''/g"` ;;
    esac
    keep_configure_args="$keep_configure_args '$fixup_arg'" ;;
  esac
done

SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CC="'"'"${CC}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CFLAGS="'"'"${CFLAGS}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} LD="'"'"${LD}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} LDFLAGS="'"'"${LDFLAGS}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} AR="'"'"${AR}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} ARFLAGS="'"'"${ARFLAGS}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} RANLIB="'"'"${RANLIB}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} WINDRES="'"'"${WINDRES}"'"'

############## final output ################

CPPFLAGS="$CPPFLAGS $PREFLAGS"

AC_SUBST(CC)
AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(LD)
AC_SUBST(LDFLAGS)
AC_SUBST(LIBS)
AC_SUBST(AR)
AC_SUBST(ARFLAGS)
AC_SUBST(RANLIB)
AC_SUBST(STATIC_AR)
AC_SUBST(WINDRES)
AC_SUBST(ICP)
AC_SUBST(STRIP_DEBUG)
AC_SUBST(STRIP_LIB_DEBUG)
AC_SUBST(RKTLINKER)
AC_SUBST(INCLUDEDEP)
AC_SUBST(RKTL_PRE)
AC_SUBST(RKTL_POST)
AC_SUBST(RACKET)
AC_SUBST(SCHEME_DIR)
AC_SUBST(MACH)
AC_SUBST(TARGET_MACH)
AC_SUBST(DIFF_MACH)
AC_SUBST(CROSS_MODE)
AC_SUBST(T_CROSS_MODE)
AC_SUBST(TT_CROSS_MODE)
AC_SUBST(OSX)
AC_SUBST(NOT_OSX)
AC_SUBST(MINGW)
AC_SUBST(NOT_MINGW)
AC_SUBST(CONFIGURE_RACKET_SO_COMPILE)
AC_SUBST(COMPRESS_COMP)
AC_SUBST(ELF_COMP)
AC_SUBST(SCHEME_SRC)
AC_SUBST(MAKE_BUILD_SCHEME)
AC_SUBST(SCHEME_CONFIG_ARGS)
AC_SUBST(SCHEME_CROSS_CONFIG_ARGS)
AC_SUBST(FRAMEWORK_INSTALL_DIR)
AC_SUBST(FRAMEWORK_PREFIX)
AC_SUBST(FRAMEWORK_REL_INSTALL)
AC_SUBST(CS_INSTALLED)
AC_SUBST(CS_COMPILED_SUBDIR)
AC_SUBST(COMP_SUBDIR_CONFIGURE_ARGS)
AC_SUBST(CROSS_COMPILE_TARGET_KIND)
AC_SUBST(RUN_RACKET)
AC_SUBST(INSTALL_SETUP_FLAGS)
AC_SUBST(INSTALL_SETUP_RACKET_FLAGS)

makefiles="Makefile"

AC_OUTPUT($makefiles)

mkdir -p rktio
abssrcdir=`(cd ${srcdir}; pwd)`
echo "=== configuring in rktio (${abssrcdir}/../../rktio)"
cd rktio; eval "${abssrcdir}/../../rktio/configure ${keep_configure_args} ${SUB_CONFIGURE_EXTRAS}"
