
#################################################################
# This is the source for the `configure` script, to be compiled #
# by autoconf (use `make-configure` in "../../ac").             #
#################################################################

AC_INIT([embed-boot.rkt])
AC_CONFIG_HEADERS([cs_config.h])

AC_CONFIG_AUX_DIR(../../lt)
AC_CANONICAL_SYSTEM

AC_DISABLE_OPTION_CHECKING

AC_ARG_ENABLE(shared,     [  --enable-shared         create shared libraries (not currently supported)])
AC_ARG_ENABLE(standalone, [  --enable-standalone     create a standalone shared library])
AC_ARG_ENABLE(pthread,    [  --enable-pthread        link with pthreads (usually auto-enabled if needed)])
AC_ARG_ENABLE(iconv,      [  --enable-iconv          use iconv (usually auto-enabled)])
AC_ARG_ENABLE(wpo,        [  --enable-wpo            agressively optimize Racket core (enabled by default)])
AC_ARG_ENABLE(compress,   [  --enable-compress       compress compiled code (enabled by default)])
AC_ARG_ENABLE(compressboot, [  --enable-compressboot   compress boot files])
m4_include(../ac/path_arg.m4)
AC_ARG_ENABLE(racket,     [  --enable-racket=<path>  use <path> as Racket for build; or "auto" to create])
AC_ARG_ENABLE(scheme,     [  --enable-scheme=<path>  use <path> as host's build directory for cross])
AC_ARG_ENABLE(mach,       [  --enable-mach=<mach>    use Chez Scheme machine type <mach>])
AC_ARG_ENABLE(target,     [  --enable-target=<mach>  cross-build for Chez Scheme machine type <mach>])
m4_include(../ac/natipkg_arg.m4)
m4_include(../ac/sdk_arg.m4)
m4_include(../ac/instlib_arg.m4)
m4_include(../ac/strip_arg.m4)
m4_include(../ac/ubsan_arg.m4)
m4_include(../ac/asan_arg.m4)
m4_include(../ac/vm_arg.m4)
m4_include(../ac/crossany_arg.m4)
AC_ARG_ENABLE(libz,       [  --enable-libz           use installed libz if available])
AC_ARG_ENABLE(liblz4,     [  --enable-liblz4         use installed liblz4 if available])

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

if test "${enable_shared}" = "yes" ; then
  echo "=== Canceling --enable-shared"
  echo "    Building Racket CS as a shared library is not currently"
  echo "    supported --- but Racket executables get the benefits of"
  echo "    sharing, anyway, by going through a shared executable"
  enable_shared=no
fi

show_explicitly_enabled "${enable_pthread}" "pthreads"
show_explicitly_disabled "${enable_pthread}" "pthreads"
show_explicitly_disabled "${enable_compress}" "Compressed code"
show_explicitly_enabled "${enable_compressboot}" "Compressed boot files"
show_explicitly_enabled "${enable_xonx}" "Unix style"
show_explicitly_set "${enable_racket}" "Racket"
show_explicitly_set "${enable_scheme}" "Chez Scheme build directory"
show_explicitly_set "${enable_mach}" "machine type"
show_explicitly_set "${enable_target}" "cross-build machine type"
m4_include(../ac/natipkg_show.m4)
show_explicitly_enabled "${enable_libz}" "Installed libz"
show_explicitly_enabled "${enable_liblz4}" "Installed liblz4"

m4_include(../ac/vm.m4)

m4_include(../ac/instlib.m4)
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

if test "${enable_csdefault}" = "yes" ; then
  CS_INSTALLED=""
else
  CS_INSTALLED=cs
  AC_DEFINE(CS_COMPILED_SUBDIR,1,[Use a subdirectory of "compiled"])
fi

show_explicitly_disabled "${enable_wpo}" "Agressive core optimization"
if test "${enable_wpo}" = "no" ; then
  ENABLE_OR_DISABLE_WPO="WHEN_WPO=no_ UNLESS_WPO="
else  
  ENABLE_OR_DISABLE_WPO=""
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
BOOT_COMPRESS_COMP=""
ELF_COMP=""
POST_LINKER="echo"

FRAMEWORK_INSTALL_DIR='$(libpltdir)'
FRAMEWORK_PREFIX='@executable_path/../lib/'

RACKET='$(DEFAULT_RACKET)'
BOOTFILE_RACKET='$(RACKET)'
RUN_RACKET='$(RUN_THIS_RACKET)'
SETUP_BOOT_MODE=--boot

INSTALL_SETUP_FLAGS=
INSTALL_SETUP_RACKET_FLAGS=

Z_LIB_DEP='$(OWN_Z_LIB)'
Z_LIB='$(OWN_Z_LIB)'
Z_LIB_UNPACK='$(AR) x $(OWN_Z_LIB)'
LZ4_LIB_DEP='$(OWN_LZ4_LIB)'
LZ4_LIB='$(OWN_LZ4_LIB)'
LZ4_LIB_UNPACK='$(AR) x $(OWN_LZ4_LIB)'

CS_HOST_WORKAREA_PREFIX=

PLT_CS_SLSP_SUFFIX=

enable_pthread_by_default=yes

cs_auto_flags=--disable-auto-flags

###### Autoconfigure #######

m4_include(../ac/sdk_ios.m4)

AC_PROG_CC

m4_include(../ac/is_gcc.m4)

# If using gcc, we want all warnings:
if test "$is_gcc" != "" ; then 
  CFLAGS="$CFLAGS -Wall"
  cs_auto_flags="${cs_auto_flags} --enable-warning-flags"
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
  ARFLAGS=rc
fi


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
    LIBS="$LIBS -lsocket -lnsl -lintl"
    use_flag_pthread="no"
    use_flag_posix_pthread="yes"
    enable_strip=no
    ;;
  aix*)
    ;;
  *freebsd*)
    MACH_OS=fb
    CFLAGS="${CFLAGS} -I/usr/local/include"
    LDFLAGS="${LDFLAGS} -L/usr/local/lib"
    LIBS="${LIBS} -liconv -lm -lpthread"
    CPPFLAGS="${CPPFLAGS} -DELF_FIND_BOOT_SECTION"
    ELF_COMP="--expect-elf"
    ;;
  openbsd*)
    MACH_OS=ob
    CFLAGS="${CFLAGS} -I/usr/local/include"
    LDFLAGS="${LDFLAGS} -L/usr/local/lib"
    LIBS="${LIBS} -liconv -lm -lpthread"
    LDFLAGS="${LDFLAGS} -Wl,-zwxneeded"
    CPPFLAGS="${CPPFLAGS} -DELF_FIND_BOOT_SECTION"
    ELF_COMP="--expect-elf"
    ;;
  bitrig*)
    ;;
  dragonfly*)
    ;;
  netbsd*)
    MACH_OS=nb
    LIBS="${LIBS} /usr/lib/i18n/libiconv_std.a -lm -lpthread"
    CPPFLAGS="${CPPFLAGS} -DELF_FIND_BOOT_SECTION"
    ELF_COMP="--expect-elf"
    POST_LINKER="paxctl +m"
    ;;
  irix*)
    ;;
  linux*)
    MACH_OS=le
    LIBS="${LIBS} -lrt"
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
    LIBS="${LIBS} -lshell32 -luser32 -lole32 -lrpcrt4 -luuid -lws2_32 -ladvapi32 -lwinmm"
    MINGW=""
    NOT_MINGW="mingw"
    if `which ${host}-windres > /dev/null` ; then
      WINDRES="${host}-windres"
    fi
    # ".a" is typically not useful, since we always build a DLL:
    if test "${enable_libs}" = "" ; then
      INSTALL_LIBS_ENABLE=no-install
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
    if test "${enable_embedfw}" = "yes" ; then
      strip_needs_dash_s=yes
    fi

    PROPAGATE_SUB_CONFIGURE=no
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

    # ncurses.h is always available, except on iOS
    if test "${enable_ios}" == "" ; then
      disable_curses_arg=""
    fi
    ;;
  nto-qnx*)
    MACH_OS=qnx
    use_flag_pthread=no
    ;;
  *)
    ;;
esac

if test "${enable_pthread}" = "" ; then
  if test "${enable_pthread_by_default}" = "yes" ; then
    enable_pthread=yes
  fi
fi

############## machine type ################

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
  aarch64*|arm64*)
    MACH="${thread_prefix}arm64${MACH_OS}"
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
    aarch64*|arm64*)
      BUILD_MACH="${BUILD_THREAD_PREFIX}arm64${BUILD_OS}"
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

############## Racket and Chez Scheme ################

case "$srcdir" in
    /*)
        upsrcdir=$srcdir
        ;;
    *)
        upsrcdir=../$srcdir
        ;;
esac

SCHEME_DIR=${srcdir}/../../ChezScheme
MAKE_BUILD_SCHEME=checkout

if test "${enable_scheme}" != "" ; then
  CS_HOST_WORKAREA_PREFIX="${enable_scheme}/"
fi

if test "${enable_racket}" != "" ; then
  if test "${enable_racket}" = "auto" ; then
    # See also "configure-parent.ac"
    enable_racket="`pwd`/../../local/cs/c/racketcs"
    CS_HOST_WORKAREA_PREFIX=../../local/cs/c/
  fi
  # In non-cross mode, we interpret `--enable-racket` to supply a
  # Racket used only for generating Chez Scheme boot files
  if test "${CROSS_MODE}" = "cross" ; then
    RACKET="${enable_racket}"
  else
    BOOTFILE_RACKET="${enable_racket}"
  fi
  SETUP_BOOT_MODE=--chain
fi

if test "${enable_compress}" != "no" ; then
  COMPRESS_COMP="--compress"
fi

if test "${enable_compressboot}" = "yes" ; then
  BOOT_COMPRESS_COMP="--compress"
fi

DIFF_MACH=skip
if test "${MACH}" != "${TARGET_MACH}" ; then
  DIFF_MACH=
fi

############## Strip tool ################

m4_include(../ac/strip.m4)

############## natipkg ################

if test "${enable_natipkg}" = "yes" ; then
  CONFIGURE_RACKET_SO_COMPILE="${CONFIGURE_RACKET_SO_COMPILE} env PLT_CS_SLSP_SUFFIX=-natipkg"
  PLT_CS_SLSP_SUFFIX="-natipkg"
fi

############## ubsan ################

m4_include(../ac/ubsan.m4)

############## ubsan ################

m4_include(../ac/asan.m4)

############## Makefile includes ################

if test "$INCLUDEDEP" = "#" ; then
  is_gmake=`make -v no-such-target-we-hope 2>&1 | grep "GNU Make"`
  if test "$is_gmake" != "" ; then
     INCLUDEDEP="-include"
  fi
fi

if test "$INCLUDEDEP" != "#" ; then
  RKTL_PRE="compiled/"
  RKTL_POST=".rktl"
fi

############## C flags ################

AC_LANG_C

m4_include(../ac/iconv.m4)
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

############## zlib and LZ4 ################

if test "${enable_libz}" != "no" ; then
  if test ! -d "$SCHEME_DIR/zlib" ; then
    echo "No libz in Scheme directory, so trying installed"
    enable_libz=yes
  fi
fi

if test "${enable_liblz4}" != "no" ; then
  if test ! -d "$SCHEME_DIR/lz4" ; then
    echo "No lz4 in Scheme directory, so trying installed"
    enable_liblz4=yes
  fi
fi

if test "${enable_libz}" = "yes" ; then
  AC_MSG_CHECKING([for libz])

  OLD_LIBS="${LIBS}"
  LIBS="${LIBS} -lz"
  AC_TRY_LINK([#include <zlib.h>],
              [zlibVersion();],
             have_zlib=yes,
             have_zlib=no)
  AC_MSG_RESULT($have_zlib)

  if test "${have_zlib}" = "no" ; then
    LIBS="${OLD_LIBS}"
    echo "Building own libz"
  else
    Z_LIB_DEP=""
    Z_LIB="-lz"
    Z_LIB_UNPACK='$(NOOP)'
    extra_scheme_config_args="${extra_scheme_config_args} ZLIB=-lz"
  fi
fi

if test "${enable_liblz4}" = "yes" ; then
  AC_MSG_CHECKING([for liblz4])

  OLD_LIBS="${LIBS}"
  LIBS="${LIBS} -llz4"
  AC_TRY_LINK([#include <lz4.h>],
              [LZ4_versionString();],
             have_lz4=yes,
             have_lz4=no)
  AC_MSG_RESULT($have_lz4)

  if test "${have_lz4}" = "no" ; then
    LIBS="${OLD_LIBS}"
    echo "Building own liblz4"
  else
    LZ4_LIB_DEP=""
    LZ4_LIB="-llz4"
    LZ4_LIB_UNPACK='$(NOOP)'
    extra_scheme_config_args="${extra_scheme_config_args} LZ4=-llz4"
  fi
fi

############## other inferred machine properties ################

AC_CHECK_SIZEOF([void *])

m4_include(../ac/valid_sizeof.m4)

############## subconfigure ################

# Before libtool setup

CPPFLAGS="$CPPFLAGS $PREFLAGS"

SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CC="'"'"${CC}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CFLAGS="'"'"${CFLAGS}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} LDFLAGS="'"'"${LDFLAGS}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} LIBS="'"'"${LIBS}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} AR="'"'"${AR}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} ARFLAGS="'"'"${ARFLAGS}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} RANLIB="'"'"${RANLIB}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} WINDRES="'"'"${WINDRES}"'"'
SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CPPFLAGS="'"'"${CPPFLAGS}"'"'

# Make sure the --enable-pthread result is propagated:
if test "${enable_pthread}" = "yes" ; then
  SUB_CONFIGURE_EXTRAS="$SUB_CONFIGURE_EXTRAS --enable-pthread"
else
  SUB_CONFIGURE_EXTRAS="$SUB_CONFIGURE_EXTRAS --disable-pthread"
fi

# Make sure the --enable-iconv result is propagated:
if test "${enable_iconv}" = "yes" ; then
  SUB_CONFIGURE_EXTRAS="$SUB_CONFIGURE_EXTRAS --enable-iconv"
else
  SUB_CONFIGURE_EXTRAS="$SUB_CONFIGURE_EXTRAS --disable-iconv"
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
    (cd ../../lt; sh ${abssrcdir}/../../lt/configure --enable-shared --disable-static)
    LIBTOOLPROG=`pwd`/../../lt/libtool
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

  CONFIGURE_RACKET_SO_COMPILE="env PLT_CS_MAKE_LINK_SHARED=y"
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

m4_include(../ac/rktio_keep.m4)

SCHEME_CONFIG_ARGS="--machine=${MACH} --disable-x11 ${disable_curses_arg} ${cs_auto_flags} ${extra_scheme_config_args}"
SCHEME_CROSS_CONFIG_ARGS="--machine=${TARGET_MACH} --disable-x11 ${cs_auto_flags} ${disable_curses_arg} ${extra_scheme_config_args}"

AC_SUBST(upsrcdir)
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
AC_SUBST(ICP_LIB)
AC_SUBST(STRIP_DEBUG)
AC_SUBST(STRIP_LIB_DEBUG)
AC_SUBST(STRIP_SIGNATURE)
AC_SUBST(RESTORE_SIGNATURE)
AC_SUBST(RKTLINKER)
AC_SUBST(POST_LINKER)
AC_SUBST(INCLUDEDEP)
AC_SUBST(RKTL_PRE)
AC_SUBST(RKTL_POST)
AC_SUBST(RACKET)
AC_SUBST(BOOTFILE_RACKET)
AC_SUBST(SCHEME_DIR)
AC_SUBST(MACH)
AC_SUBST(TARGET_MACH)
AC_SUBST(DIFF_MACH)
AC_SUBST(CROSS_MODE)
AC_SUBST(T_CROSS_MODE)
AC_SUBST(TT_CROSS_MODE)
AC_SUBST(SETUP_BOOT_MODE)
AC_SUBST(OSX)
AC_SUBST(NOT_OSX)
AC_SUBST(MINGW)
AC_SUBST(NOT_MINGW)
AC_SUBST(Z_LIB_DEP)
AC_SUBST(Z_LIB)
AC_SUBST(Z_LIB_UNPACK)
AC_SUBST(LZ4_LIB_DEP)
AC_SUBST(LZ4_LIB)
AC_SUBST(LZ4_LIB_UNPACK)
AC_SUBST(CONFIGURE_RACKET_SO_COMPILE)
AC_SUBST(COMPRESS_COMP)
AC_SUBST(BOOT_COMPRESS_COMP)
AC_SUBST(ELF_COMP)
AC_SUBST(MAKE_BUILD_SCHEME)
AC_SUBST(SCHEME_CONFIG_ARGS)
AC_SUBST(SCHEME_CROSS_CONFIG_ARGS)
AC_SUBST(FRAMEWORK_INSTALL_DIR)
AC_SUBST(FRAMEWORK_PREFIX)
AC_SUBST(FRAMEWORK_REL_INSTALL)
AC_SUBST(CS_INSTALLED)
AC_SUBST(CS_COMPILED_SUBDIR)
AC_SUBST(ENABLE_OR_DISABLE_WPO)
AC_SUBST(COMP_SUBDIR_CONFIGURE_ARGS)
AC_SUBST(CROSS_COMPILE_TARGET_KIND)
AC_SUBST(CS_HOST_WORKAREA_PREFIX)
AC_SUBST(RUN_RACKET)
AC_SUBST(INSTALL_SETUP_FLAGS)
AC_SUBST(INSTALL_SETUP_RACKET_FLAGS)
AC_SUBST(INSTALL_LIBS_ENABLE)
AC_SUBST(PLT_CS_SLSP_SUFFIX)

makefiles="Makefile"

AC_OUTPUT($makefiles)

mkdir -p rktio
abssrcdir=`(cd ${srcdir}; pwd)`
echo "=== configuring in rktio (${abssrcdir}/../../rktio)"
cd rktio; eval "${abssrcdir}/../../rktio/configure ${keep_configure_args} ${SUB_CONFIGURE_EXTRAS}"
