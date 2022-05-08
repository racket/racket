





am__is_gnu_make = { \
  if test -z '$(MAKELEVEL)'; then \
    false; \
  elif test -n '$(MAKE_HOST)'; then \
    true; \
  elif test -n '$(MAKE_VERSION)' && test -n '$(CURDIR)'; then \
    true; \
  else \
    false; \
  fi; \
}
am__make_running_with_option = \
  case $${target_option-} in \
      ?) ;; \
      *) echo "am__make_running_with_option: internal error: invalid" \
              "target option '$${target_option-}' specified" >&2; \
         exit 1;; \
  esac; \
  has_opt=no; \
  sane_makeflags=$$MAKEFLAGS; \
  if $(am__is_gnu_make); then \
    sane_makeflags=$$MFLAGS; \
  else \
    case $$MAKEFLAGS in \
      *\\[\ \	]*) \
        bs=\\; \
        sane_makeflags=`printf '%s\n' "$$MAKEFLAGS" \
          | sed "s/$$bs$$bs[$$bs $$bs	]*//g"`;; \
    esac; \
  fi; \
  skip_next=no; \
  strip_trailopt () \
  { \
    flg=`printf '%s\n' "$$flg" | sed "s/$$1.*$$//"`; \
  }; \
  for flg in $$sane_makeflags; do \
    test $$skip_next = yes && { skip_next=no; continue; }; \
    case $$flg in \
      *=*|--*) continue;; \
        -*I) strip_trailopt 'I'; skip_next=yes;; \
      -*I?*) strip_trailopt 'I';; \
        -*O) strip_trailopt 'O'; skip_next=yes;; \
      -*O?*) strip_trailopt 'O';; \
        -*l) strip_trailopt 'l'; skip_next=yes;; \
      -*l?*) strip_trailopt 'l';; \
      -[dEDm]) skip_next=yes;; \
      -[JT]) skip_next=yes;; \
    esac; \
    case $$flg in \
      *$$target_option*) has_opt=yes; break;; \
    esac; \
  done; \
  test $$has_opt = yes
am__make_dryrun = (target_option=n; $(am__make_running_with_option))
am__make_keepgoing = (target_option=k; $(am__make_running_with_option))
pkgdatadir = $(datadir)/libffi
pkgincludedir = $(includedir)/libffi
pkglibdir = $(libdir)/libffi
pkglibexecdir = $(libexecdir)/libffi
am__cd = CDPATH="$${ZSH_VERSION+.}$(PATH_SEPARATOR)" && cd
install_sh_DATA = $(install_sh) -c -m 644
install_sh_PROGRAM = $(install_sh) -c
install_sh_SCRIPT = $(install_sh) -c
INSTALL_HEADER = $(INSTALL_DATA)
transform = $(program_transform_name)
NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
build_triplet = aarch64-apple-darwin20.6.0
host_triplet = aarch64-apple-darwin20.6.0
target_triplet = aarch64-apple-darwin20.6.0
am__append_1 = doc
subdir = .
ACLOCAL_M4 = $(top_srcdir)/aclocal.m4
am__aclocal_m4_deps = $(top_srcdir)/m4/asmcfi.m4 \
 $(top_srcdir)/m4/ax_append_flag.m4 \
 $(top_srcdir)/m4/ax_cc_maxopt.m4 \
 $(top_srcdir)/m4/ax_cflags_warn_all.m4 \
 $(top_srcdir)/m4/ax_check_compile_flag.m4 \
 $(top_srcdir)/m4/ax_compiler_vendor.m4 \
 $(top_srcdir)/m4/ax_configure_args.m4 \
 $(top_srcdir)/m4/ax_enable_builddir.m4 \
 $(top_srcdir)/m4/ax_gcc_archflag.m4 \
 $(top_srcdir)/m4/ax_gcc_x86_cpuid.m4 \
 $(top_srcdir)/m4/ax_require_defined.m4 \
 $(top_srcdir)/m4/libtool.m4 $(top_srcdir)/m4/ltoptions.m4 \
 $(top_srcdir)/m4/ltsugar.m4 $(top_srcdir)/m4/ltversion.m4 \
 $(top_srcdir)/m4/lt~obsolete.m4 $(top_srcdir)/acinclude.m4 \
 $(top_srcdir)/configure.ac
am__configure_deps = $(am__aclocal_m4_deps) $(CONFIGURE_DEPENDENCIES) \
 $(ACLOCAL_M4)
DIST_COMMON = $(srcdir)/Makefile.am $(top_srcdir)/configure \
 $(am__configure_deps) $(noinst_HEADERS) $(am__DIST_COMMON)
am__CONFIG_DISTCLEAN_FILES = config.status config.cache config.log \
 configure.lineno config.status.lineno
mkinstalldirs = $(install_sh) -d
CONFIG_HEADER = fficonfig.h
CONFIG_CLEAN_FILES = libffi.pc
CONFIG_CLEAN_VPATH_FILES =
am__vpath_adj_setup = srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`;
am__vpath_adj = case $$p in \
    $(srcdir)/*) f=`echo "$$p" | sed "s|^$$srcdirstrip/||"`;; \
    *) f=$$p;; \
  esac;
am__strip_dir = f=`echo $$p | sed -e 's|^.*/||'`;
am__install_max = 40
am__nobase_strip_setup = \
  srcdirstrip=`echo "$(srcdir)" | sed 's/[].[^$$\\*|]/\\\\&/g'`
am__nobase_strip = \
  for p in $$list; do echo "$$p"; done | sed -e "s|$$srcdirstrip/||"
am__nobase_list = $(am__nobase_strip_setup); \
  for p in $$list; do echo "$$p $$p"; done | \
  sed "s| $$srcdirstrip/| |;"' / .*\//!s/ .*/ ./; s,\( .*\)/[^/]*$$,\1,' | \
  $(AWK) 'BEGIN { files["."] = "" } { files[$$2] = files[$$2] " " $$1; \
    if (++n[$$2] == $(am__install_max)) \
      { print $$2, files[$$2]; n[$$2] = 0; files[$$2] = "" } } \
    END { for (dir in files) print dir, files[dir] }'
am__base_list = \
  sed '$$!N;$$!N;$$!N;$$!N;$$!N;$$!N;$$!N;s/\n/ /g' | \
  sed '$$!N;$$!N;$$!N;$$!N;s/\n/ /g'
am__uninstall_files_from_dir = { \
  test -z "$$files" \
    || { test ! -d "$$dir" && test ! -f "$$dir" && test ! -r "$$dir"; } \
    || { echo " ( cd '$$dir' && rm -f" $$files ")"; \
         $(am__cd) "$$dir" && rm -f $$files; }; \
  }
am__installdirs = "$(DESTDIR)$(toolexeclibdir)" \
 "$(DESTDIR)$(pkgconfigdir)"
LTLIBRARIES = $(noinst_LTLIBRARIES) $(toolexeclib_LTLIBRARIES)
am__DEPENDENCIES_1 =
am__libffi_la_SOURCES_DIST = src/prep_cif.c src/types.c src/raw_api.c \
 src/java_raw_api.c src/closures.c src/debug.c
am__dirstamp = $(am__leading_dot)dirstamp
am_libffi_la_OBJECTS = src/prep_cif.lo src/types.lo src/raw_api.lo \
 src/java_raw_api.lo src/closures.lo $(am__objects_1)
libffi_la_OBJECTS = $(am_libffi_la_OBJECTS)
AM_V_lt = $(am__v_lt_$(V))
am__v_lt_ = $(am__v_lt_$(AM_DEFAULT_VERBOSITY))
am__v_lt_0 = --silent
am__v_lt_1 = 
libffi_la_LINK = $(LIBTOOL) $(AM_V_lt) --tag=CC $(AM_LIBTOOLFLAGS) \
 $(LIBTOOLFLAGS) --mode=link $(CCLD) $(AM_CFLAGS) $(CFLAGS) \
 $(libffi_la_LDFLAGS) $(LDFLAGS) -o $@
am__DEPENDENCIES_2 = $(am__DEPENDENCIES_1)
am__libffi_convenience_la_SOURCES_DIST = src/prep_cif.c src/types.c \
 src/raw_api.c src/java_raw_api.c src/closures.c src/debug.c
am__objects_2 = src/prep_cif.lo src/types.lo src/raw_api.lo \
 src/java_raw_api.lo src/closures.lo $(am__objects_1)
am_libffi_convenience_la_OBJECTS = $(am__objects_2)
nodist_libffi_convenience_la_OBJECTS =
libffi_convenience_la_OBJECTS = $(am_libffi_convenience_la_OBJECTS) \
 $(nodist_libffi_convenience_la_OBJECTS)
AM_V_P = $(am__v_P_$(V))
am__v_P_ = $(am__v_P_$(AM_DEFAULT_VERBOSITY))
am__v_P_0 = false
am__v_P_1 = :
AM_V_GEN = $(am__v_GEN_$(V))
am__v_GEN_ = $(am__v_GEN_$(AM_DEFAULT_VERBOSITY))
am__v_GEN_0 = @echo "  GEN     " $@;
am__v_GEN_1 = 
AM_V_at = $(am__v_at_$(V))
am__v_at_ = $(am__v_at_$(AM_DEFAULT_VERBOSITY))
am__v_at_0 = @
am__v_at_1 = 
DEFAULT_INCLUDES = -I. -I$(srcdir)
depcomp = $(SHELL) $(top_srcdir)/depcomp
am__maybe_remake_depfiles = depfiles
am__depfiles_remade = src/$(DEPDIR)/closures.Plo \
 src/$(DEPDIR)/debug.Plo src/$(DEPDIR)/java_raw_api.Plo \
 src/$(DEPDIR)/prep_cif.Plo src/$(DEPDIR)/raw_api.Plo \
 src/$(DEPDIR)/types.Plo src/aarch64/$(DEPDIR)/ffi.Plo \
 src/aarch64/$(DEPDIR)/sysv.Plo \
 src/aarch64/$(DEPDIR)/win64_armasm.Plo \
 src/alpha/$(DEPDIR)/ffi.Plo src/alpha/$(DEPDIR)/osf.Plo \
 src/arc/$(DEPDIR)/arcompact.Plo src/arc/$(DEPDIR)/ffi.Plo \
 src/arm/$(DEPDIR)/ffi.Plo src/arm/$(DEPDIR)/sysv.Plo \
 src/arm/$(DEPDIR)/sysv_msvc_arm32.Plo \
 src/avr32/$(DEPDIR)/ffi.Plo src/avr32/$(DEPDIR)/sysv.Plo \
 src/bfin/$(DEPDIR)/ffi.Plo src/bfin/$(DEPDIR)/sysv.Plo \
 src/cris/$(DEPDIR)/ffi.Plo src/cris/$(DEPDIR)/sysv.Plo \
 src/frv/$(DEPDIR)/eabi.Plo src/frv/$(DEPDIR)/ffi.Plo \
 src/ia64/$(DEPDIR)/ffi.Plo src/ia64/$(DEPDIR)/unix.Plo \
 src/m32r/$(DEPDIR)/ffi.Plo src/m32r/$(DEPDIR)/sysv.Plo \
 src/m68k/$(DEPDIR)/ffi.Plo src/m68k/$(DEPDIR)/sysv.Plo \
 src/m88k/$(DEPDIR)/ffi.Plo src/m88k/$(DEPDIR)/obsd.Plo \
 src/metag/$(DEPDIR)/ffi.Plo src/metag/$(DEPDIR)/sysv.Plo \
 src/microblaze/$(DEPDIR)/ffi.Plo \
 src/microblaze/$(DEPDIR)/sysv.Plo src/mips/$(DEPDIR)/ffi.Plo \
 src/mips/$(DEPDIR)/n32.Plo src/mips/$(DEPDIR)/o32.Plo \
 src/moxie/$(DEPDIR)/eabi.Plo src/moxie/$(DEPDIR)/ffi.Plo \
 src/nios2/$(DEPDIR)/ffi.Plo src/nios2/$(DEPDIR)/sysv.Plo \
 src/or1k/$(DEPDIR)/ffi.Plo src/or1k/$(DEPDIR)/sysv.Plo \
 src/pa/$(DEPDIR)/ffi.Plo src/pa/$(DEPDIR)/hpux32.Plo \
 src/pa/$(DEPDIR)/linux.Plo src/powerpc/$(DEPDIR)/aix.Plo \
 src/powerpc/$(DEPDIR)/aix_closure.Plo \
 src/powerpc/$(DEPDIR)/darwin.Plo \
 src/powerpc/$(DEPDIR)/darwin_closure.Plo \
 src/powerpc/$(DEPDIR)/ffi.Plo \
 src/powerpc/$(DEPDIR)/ffi_darwin.Plo \
 src/powerpc/$(DEPDIR)/ffi_linux64.Plo \
 src/powerpc/$(DEPDIR)/ffi_sysv.Plo \
 src/powerpc/$(DEPDIR)/linux64.Plo \
 src/powerpc/$(DEPDIR)/linux64_closure.Plo \
 src/powerpc/$(DEPDIR)/ppc_closure.Plo \
 src/powerpc/$(DEPDIR)/sysv.Plo src/riscv/$(DEPDIR)/ffi.Plo \
 src/riscv/$(DEPDIR)/sysv.Plo src/s390/$(DEPDIR)/ffi.Plo \
 src/s390/$(DEPDIR)/sysv.Plo src/sh/$(DEPDIR)/ffi.Plo \
 src/sh/$(DEPDIR)/sysv.Plo src/sh64/$(DEPDIR)/ffi.Plo \
 src/sh64/$(DEPDIR)/sysv.Plo src/sparc/$(DEPDIR)/ffi.Plo \
 src/sparc/$(DEPDIR)/ffi64.Plo src/sparc/$(DEPDIR)/v8.Plo \
 src/sparc/$(DEPDIR)/v9.Plo src/tile/$(DEPDIR)/ffi.Plo \
 src/tile/$(DEPDIR)/tile.Plo src/vax/$(DEPDIR)/elfbsd.Plo \
 src/vax/$(DEPDIR)/ffi.Plo src/x86/$(DEPDIR)/ffi.Plo \
 src/x86/$(DEPDIR)/ffi64.Plo src/x86/$(DEPDIR)/ffiw64.Plo \
 src/x86/$(DEPDIR)/sysv.Plo src/x86/$(DEPDIR)/sysv_intel.Plo \
 src/x86/$(DEPDIR)/unix64.Plo src/x86/$(DEPDIR)/win64.Plo \
 src/x86/$(DEPDIR)/win64_intel.Plo src/xtensa/$(DEPDIR)/ffi.Plo \
 src/xtensa/$(DEPDIR)/sysv.Plo
am__mv = mv -f
CPPASCOMPILE = $(CCAS) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
 $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CCASFLAGS) $(CCASFLAGS)
LTCPPASCOMPILE = $(LIBTOOL) $(AM_V_lt) --tag=CC $(AM_LIBTOOLFLAGS) \
 $(LIBTOOLFLAGS) --mode=compile $(CCAS) $(DEFS) \
 $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) \
 $(AM_CCASFLAGS) $(CCASFLAGS)
AM_V_CPPAS = $(am__v_CPPAS_$(V))
am__v_CPPAS_ = $(am__v_CPPAS_$(AM_DEFAULT_VERBOSITY))
am__v_CPPAS_0 = @echo "  CPPAS   " $@;
am__v_CPPAS_1 = 
COMPILE = $(CC) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
 $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS)
LTCOMPILE = $(LIBTOOL) $(AM_V_lt) --tag=CC $(AM_LIBTOOLFLAGS) \
 $(LIBTOOLFLAGS) --mode=compile $(CC) $(DEFS) \
 $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) \
 $(AM_CFLAGS) $(CFLAGS)
AM_V_CC = $(am__v_CC_$(V))
am__v_CC_ = $(am__v_CC_$(AM_DEFAULT_VERBOSITY))
am__v_CC_0 = @echo "  CC      " $@;
am__v_CC_1 = 
CCLD = $(CC)
LINK = $(LIBTOOL) $(AM_V_lt) --tag=CC $(AM_LIBTOOLFLAGS) \
 $(LIBTOOLFLAGS) --mode=link $(CCLD) $(AM_CFLAGS) $(CFLAGS) \
 $(AM_LDFLAGS) $(LDFLAGS) -o $@
AM_V_CCLD = $(am__v_CCLD_$(V))
am__v_CCLD_ = $(am__v_CCLD_$(AM_DEFAULT_VERBOSITY))
am__v_CCLD_0 = @echo "  CCLD    " $@;
am__v_CCLD_1 = 
SOURCES = $(libffi_la_SOURCES) $(EXTRA_libffi_la_SOURCES) \
 $(libffi_convenience_la_SOURCES) \
 $(EXTRA_libffi_convenience_la_SOURCES) \
 $(nodist_libffi_convenience_la_SOURCES)
DIST_SOURCES = $(am__libffi_la_SOURCES_DIST) \
 $(EXTRA_libffi_la_SOURCES) \
 $(am__libffi_convenience_la_SOURCES_DIST) \
 $(EXTRA_libffi_convenience_la_SOURCES)
RECURSIVE_TARGETS = all-recursive check-recursive cscopelist-recursive \
 ctags-recursive dvi-recursive html-recursive info-recursive \
 install-data-recursive install-dvi-recursive \
 install-exec-recursive install-html-recursive \
 install-info-recursive install-pdf-recursive \
 install-ps-recursive install-recursive installcheck-recursive \
 installdirs-recursive pdf-recursive ps-recursive \
 tags-recursive uninstall-recursive
am__can_run_installinfo = \
  case $$AM_UPDATE_INFO_DIR in \
    n|no|NO) false;; \
    *) (install-info --version) >/dev/null 2>&1;; \
  esac
DATA = $(pkgconfig_DATA)
HEADERS = $(noinst_HEADERS)
RECURSIVE_CLEAN_TARGETS = mostlyclean-recursive clean-recursive	\
  distclean-recursive maintainer-clean-recursive
am__recursive_targets = \
  $(RECURSIVE_TARGETS) \
  $(RECURSIVE_CLEAN_TARGETS) \
  $(am__extra_recursive_targets)
AM_RECURSIVE_TARGETS = $(am__recursive_targets:-recursive=) TAGS CTAGS \
 cscope distdir distdir-am dist dist-all distcheck
am__tagged_files = $(HEADERS) $(SOURCES) $(TAGS_FILES) \
 $(LISP)fficonfig.h.in
am__uniquify_input = $(AWK) '\
  BEGIN { nonempty = 0; } \
  { items[$$0] = 1; nonempty = 1; } \
  END { if (nonempty) { for (i in items) print i; }; } \
'
am__define_uniq_tagged_files = \
  list='$(am__tagged_files)'; \
  unique=`for i in $$list; do \
    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
  done | $(am__uniquify_input)`
ETAGS = etags
CTAGS = ctags
CSCOPE = cscope
DIST_SUBDIRS = include testsuite man doc
am__DIST_COMMON = $(srcdir)/Makefile.in $(srcdir)/fficonfig.h.in \
 $(srcdir)/libffi.pc.in compile config.guess config.sub depcomp \
 install-sh ltmain.sh missing
DISTFILES = $(DIST_COMMON) $(DIST_SOURCES) $(TEXINFOS) $(EXTRA_DIST)
distdir = $(PACKAGE)-$(VERSION)
top_distdir = $(distdir)
am__remove_distdir = \
  if test -d "$(distdir)"; then \
    find "$(distdir)" -type d ! -perm -200 -exec chmod u+w {} ';' \
      && rm -rf "$(distdir)" \
      || { sleep 5 && rm -rf "$(distdir)"; }; \
  else :; fi
am__post_remove_distdir = $(am__remove_distdir)
am__relativize = \
  dir0=`pwd`; \
  sed_first='s,^\([^/]*\)/.*$$,\1,'; \
  sed_rest='s,^[^/]*/*,,'; \
  sed_last='s,^.*/\([^/]*\)$$,\1,'; \
  sed_butlast='s,/*[^/]*$$,,'; \
  while test -n "$$dir1"; do \
    first=`echo "$$dir1" | sed -e "$$sed_first"`; \
    if test "$$first" != "."; then \
      if test "$$first" = ".."; then \
        dir2=`echo "$$dir0" | sed -e "$$sed_last"`/"$$dir2"; \
        dir0=`echo "$$dir0" | sed -e "$$sed_butlast"`; \
      else \
        first2=`echo "$$dir2" | sed -e "$$sed_first"`; \
        if test "$$first2" = "$$first"; then \
          dir2=`echo "$$dir2" | sed -e "$$sed_rest"`; \
        else \
          dir2="../$$dir2"; \
        fi; \
        dir0="$$dir0"/"$$first"; \
      fi; \
    fi; \
    dir1=`echo "$$dir1" | sed -e "$$sed_rest"`; \
  done; \
  reldir="$$dir2"
DIST_ARCHIVES = $(distdir).tar.gz
GZIP_ENV = --best
DIST_TARGETS = dist-gzip
distuninstallcheck_listfiles = find . -type f -print
am__distuninstallcheck_listfiles = $(distuninstallcheck_listfiles) \
  | sed 's|^\./|$(prefix)/|' | grep -v '$(infodir)/dir$$'
distcleancheck_listfiles = find . -type f -print
ACLOCAL = ${SHELL} /Users/mflatt/plt/racket/src/bc/foreign/libffi/missing aclocal-1.16
ALLOCA = 
AMTAR = $${TAR-tar}
AM_DEFAULT_VERBOSITY = 1
AM_LTLDFLAGS = 
AM_RUNTESTFLAGS = 
AR = ar
AUTOCONF = ${SHELL} /Users/mflatt/plt/racket/src/bc/foreign/libffi/missing autoconf
AUTOHEADER = ${SHELL} /Users/mflatt/plt/racket/src/bc/foreign/libffi/missing autoheader
AUTOMAKE = ${SHELL} /Users/mflatt/plt/racket/src/bc/foreign/libffi/missing automake-1.16
AWK = awk
CC = gcc
CCAS = gcc
CCASDEPMODE = depmode=gcc3
CCASFLAGS = 
CCDEPMODE = depmode=gcc3
CFLAGS = -O3 -Wall -fexceptions
CPP = gcc -E
CPPFLAGS = 
CXX = g++
CXXCPP = g++ -E
CXXDEPMODE = depmode=gcc3
CXXFLAGS = -g -O2
CYGPATH_W = echo
DEFS = -DHAVE_CONFIG_H
DEPDIR = .deps
DLLTOOL = false
DSYMUTIL = dsymutil
DUMPBIN = 
ECHO_C = \c
ECHO_N = 
ECHO_T = 
EGREP = /usr/bin/grep -E
EXEEXT = 
FFI_EXEC_TRAMPOLINE_TABLE = 0
FGREP = /usr/bin/grep -F
GREP = /usr/bin/grep
HAVE_LONG_DOUBLE = 0
HAVE_LONG_DOUBLE_VARIANT = 0
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT = ${INSTALL}
INSTALL_STRIP_PROGRAM = $(install_sh) -c -s
LD = /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ld
LDFLAGS = 
LIBOBJS = 
LIBS = 
LIBTOOL = $(SHELL) $(top_builddir)/libtool
LIPO = lipo
LN_S = ln -s
LTLIBOBJS = 
LT_SYS_LIBRARY_PATH = 
MAINT = #
MAKEINFO = ${SHELL} /Users/mflatt/plt/racket/src/bc/foreign/libffi/missing makeinfo
MANIFEST_TOOL = :
MKDIR_P = /opt/homebrew/bin/gmkdir -p
NM = /usr/bin/nm -B
NMEDIT = nmedit
OBJDUMP = objdump
OBJEXT = o
OPT_LDFLAGS = 
OTOOL = otool
OTOOL64 = :
PACKAGE = libffi
PACKAGE_BUGREPORT = http://github.com/libffi/libffi/issues
PACKAGE_NAME = libffi
PACKAGE_STRING = libffi 3.3
PACKAGE_TARNAME = libffi
PACKAGE_URL = 
PACKAGE_VERSION = 3.3
PATH_SEPARATOR = :
PRTDIAG = 
RANLIB = ranlib
SECTION_LDFLAGS = 
SED = /usr/bin/sed
SET_MAKE = 
SHELL = /bin/sh
STRIP = strip
TARGET = AARCH64
TARGETDIR = aarch64
TARGET_OBJ =  src/aarch64/ffi.lo src/aarch64/sysv.lo
VERSION = 3.3
abs_builddir = /Users/mflatt/plt/racket/src/build/sgc/bc/foreign/libffi
abs_srcdir = /Users/mflatt/plt/racket/src/build/sgc/bc/foreign/libffi/../../../../../bc/foreign/libffi
abs_top_builddir = /Users/mflatt/plt/racket/src/build/sgc/bc/foreign/libffi
abs_top_srcdir = /Users/mflatt/plt/racket/src/build/sgc/bc/foreign/libffi/../../../../../bc/foreign/libffi
ac_ct_AR = ar
ac_ct_CC = gcc
ac_ct_CXX = g++
ac_ct_DUMPBIN = 
am__include = include
am__leading_dot = .
am__quote = 
am__tar = $${TAR-tar} chof - "$$tardir"
am__untar = $${TAR-tar} xf -
ax_enable_builddir_sed = sed
bindir = ${exec_prefix}/bin
build = aarch64-apple-darwin20.6.0
build_alias = 
build_cpu = aarch64
build_os = darwin20.6.0
build_vendor = apple
builddir = .
datadir = ${datarootdir}
datarootdir = ${prefix}/share
docdir = ${datarootdir}/doc/${PACKAGE_TARNAME}
dvidir = ${docdir}
exec_prefix = ${prefix}
host = aarch64-apple-darwin20.6.0
host_alias = 
host_cpu = aarch64
host_os = darwin20.6.0
host_vendor = apple
htmldir = ${docdir}
includedir = ${prefix}/include
infodir = ${datarootdir}/info
install_sh = ${SHELL} /Users/mflatt/plt/racket/src/bc/foreign/libffi/install-sh
libdir = ${exec_prefix}/lib
libexecdir = ${exec_prefix}/libexec
localedir = ${datarootdir}/locale
localstatedir = ${prefix}/var
mandir = ${datarootdir}/man
mkdir_p = $(MKDIR_P)
oldincludedir = /usr/include
pdfdir = ${docdir}
prefix = /usr/local
program_transform_name = s,x,x,
psdir = ${docdir}
sbindir = ${exec_prefix}/sbin
sharedstatedir = ${prefix}/com
srcdir = .
sys_symbol_underscore = yes
sysconfdir = ${prefix}/etc
target = aarch64-apple-darwin20.6.0
target_alias = 
target_cpu = aarch64
target_os = darwin20.6.0
target_vendor = apple
toolexecdir = ${libdir}/gcc-lib/$(target_alias)
toolexeclibdir = ${libdir}
top_build_prefix = 
top_builddir = .
top_srcdir = .
AUTOMAKE_OPTIONS = foreign subdir-objects
ACLOCAL_AMFLAGS = -I m4
SUBDIRS = include testsuite man $(am__append_1)
EXTRA_DIST = LICENSE ChangeLog.old					\
 m4/libtool.m4 m4/lt~obsolete.m4					\
  m4/ltoptions.m4 m4/ltsugar.m4 m4/ltversion.m4			\
  m4/ltversion.m4 src/debug.c msvcc.sh				\
 generate-darwin-source-and-headers.py				\
 libffi.xcodeproj/project.pbxproj				\
 libtool-ldflags libtool-version configure.host README.md        \
 libffi.map.in LICENSE-BUILDTOOLS msvc_build make_sunver.pl	

DISTCLEANFILES = local.exp

FLAGS_TO_PASS = $(AM_MAKEFLAGS)
MAKEOVERRIDES = 
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libffi.pc
toolexeclib_LTLIBRARIES = libffi.la
noinst_LTLIBRARIES = libffi_convenience.la
libffi_la_SOURCES = src/prep_cif.c src/types.c src/raw_api.c \
 src/java_raw_api.c src/closures.c $(am__append_2)
noinst_HEADERS = \
 src/aarch64/ffitarget.h src/aarch64/internal.h			\
 src/alpha/ffitarget.h src/alpha/internal.h			\
 src/arc/ffitarget.h						\
 src/arm/ffitarget.h src/arm/internal.h				\
 src/avr32/ffitarget.h						\
 src/bfin/ffitarget.h						\
 src/cris/ffitarget.h						\
 src/frv/ffitarget.h						\
 src/ia64/ffitarget.h src/ia64/ia64_flags.h			\
 src/m32r/ffitarget.h						\
 src/m68k/ffitarget.h						\
 src/m88k/ffitarget.h						\
 src/metag/ffitarget.h						\
 src/microblaze/ffitarget.h					\
 src/mips/ffitarget.h						\
 src/moxie/ffitarget.h						\
 src/nios2/ffitarget.h						\
 src/or1k/ffitarget.h						\
 src/pa/ffitarget.h						\
 src/powerpc/ffitarget.h src/powerpc/asm.h src/powerpc/ffi_powerpc.h \
 src/riscv/ffitarget.h						\
 src/s390/ffitarget.h src/s390/internal.h			\
 src/sh/ffitarget.h						\
 src/sh64/ffitarget.h						\
 src/sparc/ffitarget.h src/sparc/internal.h			\
 src/tile/ffitarget.h						\
 src/vax/ffitarget.h						\
 src/x86/ffitarget.h src/x86/internal.h src/x86/internal64.h src/x86/asmnames.h \
 src/xtensa/ffitarget.h						\
 src/dlmalloc.c

EXTRA_libffi_la_SOURCES = src/aarch64/ffi.c src/aarch64/sysv.S \
 src/aarch64/win64_armasm.S src/alpha/ffi.c src/alpha/osf.S \
 src/arc/ffi.c src/arc/arcompact.S src/arm/ffi.c	\
 src/arm/sysv.S src/arm/ffi.c src/arm/sysv_msvc_arm32.S \
 src/avr32/ffi.c src/avr32/sysv.S src/bfin/ffi.c	\
 src/bfin/sysv.S src/cris/ffi.c src/cris/sysv.S src/frv/ffi.c \
 src/frv/eabi.S src/ia64/ffi.c src/ia64/unix.S src/m32r/ffi.c \
 src/m32r/sysv.S src/m68k/ffi.c src/m68k/sysv.S src/m88k/ffi.c \
 src/m88k/obsd.S src/metag/ffi.c src/metag/sysv.S \
 src/microblaze/ffi.c src/microblaze/sysv.S src/mips/ffi.c \
 src/mips/o32.S src/mips/n32.S src/moxie/ffi.c \
 src/moxie/eabi.S src/nios2/ffi.c src/nios2/sysv.S \
 src/or1k/ffi.c src/or1k/sysv.S src/pa/ffi.c src/pa/linux.S \
 src/pa/hpux32.S src/powerpc/ffi.c src/powerpc/ffi_sysv.c \
 src/powerpc/ffi_linux64.c src/powerpc/sysv.S \
 src/powerpc/linux64.S src/powerpc/linux64_closure.S \
 src/powerpc/ppc_closure.S src/powerpc/aix.S \
 src/powerpc/darwin.S src/powerpc/aix_closure.S \
 src/powerpc/darwin_closure.S src/powerpc/ffi_darwin.c \
 src/riscv/ffi.c src/riscv/sysv.S src/s390/ffi.c	\
 src/s390/sysv.S src/sh/ffi.c src/sh/sysv.S src/sh64/ffi.c \
 src/sh64/sysv.S src/sparc/ffi.c src/sparc/ffi64.c \
 src/sparc/v8.S src/sparc/v9.S src/tile/ffi.c src/tile/tile.S \
 src/vax/ffi.c src/vax/elfbsd.S src/x86/ffi.c src/x86/sysv.S \
 src/x86/ffiw64.c src/x86/win64.S src/x86/ffi64.c \
 src/x86/unix64.S src/x86/sysv_intel.S src/x86/win64_intel.S \
 src/xtensa/ffi.c src/xtensa/sysv.S

libffi_la_LIBADD = $(TARGET_OBJ)
libffi_convenience_la_SOURCES = $(libffi_la_SOURCES)
EXTRA_libffi_convenience_la_SOURCES = $(EXTRA_libffi_la_SOURCES)
libffi_convenience_la_LIBADD = $(libffi_la_LIBADD)
libffi_convenience_la_DEPENDENCIES = $(libffi_la_DEPENDENCIES)
nodist_libffi_convenience_la_SOURCES = $(nodist_libffi_la_SOURCES)
LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/libtool-ldflags $(LDFLAGS))
AM_CFLAGS = $(am__append_3)
libffi_version_script = 
libffi_version_dep = 
libffi_version_info = -version-info `grep -v '^\#' $(srcdir)/libtool-version`
libffi_la_LDFLAGS = -no-undefined $(libffi_version_info) $(libffi_version_script) $(LTLDFLAGS) $(AM_LTLDFLAGS)
libffi_la_DEPENDENCIES = $(libffi_la_LIBADD) $(libffi_version_dep)
AM_CPPFLAGS = -I. -I$(top_srcdir)/include -Iinclude -I$(top_srcdir)/src
AM_CCASFLAGS = $(AM_CPPFLAGS)
all-configured : all-all
all all-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

am--refresh am--refresh-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
$(srcdir)/Makefile.in:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
Makefile:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

$(top_builddir)/config.status:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

$(top_srcdir)/configure:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
$(ACLOCAL_M4):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
$(am__aclocal_m4_deps):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

fficonfig.h:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

stamp-h1 stamp-h1-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
$(srcdir)/fficonfig.h.in:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

distclean-hdr distclean-hdr-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
libffi.pc:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

clean-noinstLTLIBRARIES:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-toolexeclibLTLIBRARIES:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

uninstall-toolexeclibLTLIBRARIES:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

clean-toolexeclibLTLIBRARIES:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/prep_cif.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/types.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/raw_api.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/java_raw_api.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/closures.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/debug.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/aarch64/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/aarch64/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/aarch64/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/aarch64/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/aarch64/win64_armasm.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/alpha/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/alpha/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/alpha/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/alpha/osf.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/arc/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/arc/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/arc/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/arc/arcompact.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/arm/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/arm/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/arm/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/arm/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/arm/sysv_msvc_arm32.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/avr32/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/avr32/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/avr32/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/avr32/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/bfin/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/bfin/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/bfin/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/bfin/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/cris/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/cris/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/cris/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/cris/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/frv/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/frv/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/frv/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/frv/eabi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/ia64/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/ia64/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/ia64/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/ia64/unix.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m32r/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m32r/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m32r/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m32r/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m68k/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m68k/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m68k/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m68k/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m88k/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m88k/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m88k/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/m88k/obsd.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/metag/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/metag/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/metag/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/metag/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/microblaze/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/microblaze/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/microblaze/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/microblaze/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/mips/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/mips/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/mips/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/mips/o32.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/mips/n32.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/moxie/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/moxie/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/moxie/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/moxie/eabi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/nios2/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/nios2/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/nios2/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/nios2/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/or1k/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/or1k/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/or1k/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/or1k/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/pa/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/pa/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/pa/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/pa/linux.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/pa/hpux32.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/ffi_sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/ffi_linux64.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/linux64.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/linux64_closure.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/ppc_closure.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/aix.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/darwin.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/aix_closure.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/darwin_closure.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/powerpc/ffi_darwin.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/riscv/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/riscv/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/riscv/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/riscv/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/s390/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/s390/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/s390/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/s390/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sh/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sh/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sh/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sh/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sh64/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sh64/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sh64/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sh64/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sparc/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sparc/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sparc/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sparc/ffi64.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sparc/v8.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/sparc/v9.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/tile/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/tile/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/tile/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/tile/tile.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/vax/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/vax/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/vax/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/vax/elfbsd.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/x86/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/x86/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/x86/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/x86/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/x86/ffiw64.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/x86/win64.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/x86/ffi64.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/x86/unix64.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/x86/sysv_intel.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/x86/win64_intel.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/xtensa/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/xtensa/$(DEPDIR)/$(am__dirstamp):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/xtensa/ffi.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
src/xtensa/sysv.lo:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

libffi.la:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

libffi_convenience.la:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

mostlyclean-compile mostlyclean-compile-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

distclean-compile distclean-compile-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done


$(am__depfiles_remade):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

am--depfiles am--depfiles-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done







mostlyclean-libtool mostlyclean-libtool-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

clean-libtool clean-libtool-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

distclean-libtool distclean-libtool-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
install-pkgconfigDATA:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

uninstall-pkgconfigDATA:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

$(am__recursive_targets):
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

ID:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
tags tags-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
TAGS:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

tags-am tags-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
ctags ctags-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

CTAGS:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
ctags-am ctags-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

GTAGS:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
cscope cscope-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
clean-cscope clean-cscope-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
cscope.files:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
cscopelist cscopelist-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

cscopelist-am cscopelist-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

distclean-tags distclean-tags-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

distdir distdir-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

distdir-am distdir-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
dist-gzip dist-gzip-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh ./config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; found=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$found $(PACKAGE)-$(VERSION).*" \
	; if test "$$found" -eq "0" ; then : \
	; BUILD=` grep "^#### .*|" Makefile |tail -1| sed -e 's/.*|//' ` \
	; fi ; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; for f in $$i/$(PACKAGE)-$(VERSION).* \
	; do test -f "$$f" && mv "$$f" $(PUB). ; done ; break ; done

dist-bzip2 dist-bzip2-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh ./config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; found=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$found $(PACKAGE)-$(VERSION).*" \
	; if test "$$found" -eq "0" ; then : \
	; BUILD=` grep "^#### .*|" Makefile |tail -1| sed -e 's/.*|//' ` \
	; fi ; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; for f in $$i/$(PACKAGE)-$(VERSION).* \
	; do test -f "$$f" && mv "$$f" $(PUB). ; done ; break ; done

dist-lzip dist-lzip-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh ./config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; found=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$found $(PACKAGE)-$(VERSION).*" \
	; if test "$$found" -eq "0" ; then : \
	; BUILD=` grep "^#### .*|" Makefile |tail -1| sed -e 's/.*|//' ` \
	; fi ; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; for f in $$i/$(PACKAGE)-$(VERSION).* \
	; do test -f "$$f" && mv "$$f" $(PUB). ; done ; break ; done

dist-xz dist-xz-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh ./config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; found=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$found $(PACKAGE)-$(VERSION).*" \
	; if test "$$found" -eq "0" ; then : \
	; BUILD=` grep "^#### .*|" Makefile |tail -1| sed -e 's/.*|//' ` \
	; fi ; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; for f in $$i/$(PACKAGE)-$(VERSION).* \
	; do test -f "$$f" && mv "$$f" $(PUB). ; done ; break ; done

dist-tarZ dist-tarZ-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh ./config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; found=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$found $(PACKAGE)-$(VERSION).*" \
	; if test "$$found" -eq "0" ; then : \
	; BUILD=` grep "^#### .*|" Makefile |tail -1| sed -e 's/.*|//' ` \
	; fi ; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; for f in $$i/$(PACKAGE)-$(VERSION).* \
	; do test -f "$$f" && mv "$$f" $(PUB). ; done ; break ; done

dist-shar dist-shar-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh ./config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; found=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$found $(PACKAGE)-$(VERSION).*" \
	; if test "$$found" -eq "0" ; then : \
	; BUILD=` grep "^#### .*|" Makefile |tail -1| sed -e 's/.*|//' ` \
	; fi ; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; for f in $$i/$(PACKAGE)-$(VERSION).* \
	; do test -f "$$f" && mv "$$f" $(PUB). ; done ; break ; done

dist-zip dist-zip-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh ./config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; found=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$found $(PACKAGE)-$(VERSION).*" \
	; if test "$$found" -eq "0" ; then : \
	; BUILD=` grep "^#### .*|" Makefile |tail -1| sed -e 's/.*|//' ` \
	; fi ; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; for f in $$i/$(PACKAGE)-$(VERSION).* \
	; do test -f "$$f" && mv "$$f" $(PUB). ; done ; break ; done

dist dist-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; found=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$found $(PACKAGE)-$(VERSION).tar.*" \
	; if test "$$found" -eq "0" ; then : \
	; BUILD=` grep "^#### .*|" Makefile |tail -1| sed -e 's/.*|//' ` \
	; fi ; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; for f in $$i/$(PACKAGE)-$(VERSION).tar.* \
	; do test -f "$$f" && mv "$$f" $(PUB). ; done ; break ; done

distcheck distcheck-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
distuninstallcheck distuninstallcheck-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
distcleancheck distcleancheck-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
check-am check-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
check check-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
all-am all-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
installdirs installdirs-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
installdirs-am installdirs-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
install install-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
install-exec install-exec-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
install-data install-data-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
uninstall uninstall-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-am install-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

installcheck installcheck-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
install-strip install-strip-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
mostlyclean-generic mostlyclean-generic-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

clean-generic clean-generic-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

distclean-generic distclean-generic-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

maintainer-clean-generic maintainer-clean-generic-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
clean clean-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

clean-am clean-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

distclean distclean-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### .*|" Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@ (all local builds)" \
	; test ".$$BUILD" = "." && BUILD="." \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; echo "# rm -r $$i"; done ; echo "# (sleep 3)" ; sleep 3 \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; echo "$$i" | grep "^/" > /dev/null && continue \
	; echo "$$i" | grep "^../" > /dev/null && continue \
	; echo "rm -r $$i"; (rm -r "$$i") ; done ; rm Makefile
distclean-am distclean-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

dvi dvi-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

dvi-am dvi-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

html html-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

html-am html-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

info info-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

info-am info-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-data-am install-data-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-dvi install-dvi-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-dvi-am install-dvi-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-exec-am install-exec-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-html install-html-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-html-am install-html-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-info install-info-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-info-am install-info-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-man install-man-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-pdf install-pdf-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-pdf-am install-pdf-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-ps install-ps-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

install-ps-am install-ps-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

installcheck-am installcheck-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

maintainer-clean maintainer-clean-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
maintainer-clean-am maintainer-clean-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

mostlyclean mostlyclean-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

mostlyclean-am mostlyclean-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

pdf pdf-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

pdf-am pdf-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

ps ps-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

ps-am ps-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

uninstall-am uninstall-am-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done





libffi.map:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done

dist-hook dist-hook-all:
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh /Users/mflatt/plt/racket/src/bc/foreign/libffi/config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; use=` basename "$@" -all `; n=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$n * $@"; if test "$$n" -eq "0" ; then : \
	; BUILD=` grep "^####.*|" Makefile |tail -1| sed -e 's/.*|//' ` ; fi \
	; test ".$$BUILD" = "." && BUILD="." \
	; test "$$use" = "$@" && BUILD=` echo "$$BUILD" | tail -1 ` \
	; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; (cd "$$i" && test ! -f configure && $(MAKE) $$use) || exit; done
	@ HOST="$(HOST)" \
	; test ".$$HOST" = "." && HOST=` sh ./config.guess ` \
	; BUILD=` grep "^#### $$HOST " Makefile | sed -e 's/.*|//' ` \
	; found=` echo $$BUILD | wc -w ` \
	; echo "MAKE $$HOST : $$found $(PACKAGE)-$(VERSION).*" \
	; if test "$$found" -eq "0" ; then : \
	; BUILD=` grep "^#### .*|" Makefile |tail -1| sed -e 's/.*|//' ` \
	; fi ; for i in $$BUILD ; do test ".$$i" = "." && continue \
	; for f in $$i/$(PACKAGE)-$(VERSION).* \
	; do test -f "$$f" && mv "$$f" $(PUB). ; done ; break ; done

#### CONFIGURATIONS FOR TOPLEVEL MAKEFILE: 
####  |no
