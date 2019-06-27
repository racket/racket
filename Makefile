# This makefile is meant to parse with both normal `make' and `nmake'
# (on Windows). On Windows, prefix each target with `win32-'.

# The targets here do not use dependencies (mostly), so it's a strange
# use of `make'.  Really, this makefile is an alternative to a pile of
# scripts, where each target plays the role of a script.

# The main targets are
#
#  in-place = build in "racket" with all packages in development mode
#
#  base = build in "racket" only (i.e., first step of `in-place')
#
#  server = build base, build packages listed in $(PKGS) or specified
#           via $(CONFIG), start server at port $(SERVER_PORT)
#
#  client = build base, create an installer with $(PKGS) with the help
#           of $(SERVER); result is recorded in "bundle/installer.txt"
#
#  installers = `server' plus `client' via $(CONFIG)

# Packages (separated by spaces) to link in development mode or
# to include in a distribution:
PKGS = main-distribution main-distribution-test

# ------------------------------------------------------------
# In-place build

PLAIN_RACKET = racket/bin/racket
WIN32_PLAIN_RACKET = racket\racket

# For -M, etc., to pick the target machine for compilation:
SETUP_MACHINE_FLAGS =

# In case of cross-installation, point explicitly to local content:
RUN_RACKET = $(PLAIN_RACKET) $(SETUP_MACHINE_FLAGS) -G racket/etc -X racket/collects
WIN32_RUN_RACKET = $(WIN32_PLAIN_RACKET) $(SETUP_MACHINE_FLAGS) -G racket/etc -X racket/collects

RUN_RACO = $(RUN_RACKET) -N raco -l- raco
WIN32_RUN_RACO = $(WIN32_RUN_RACKET) -N raco -l- raco

DEFAULT_SRC_CATALOG = https://pkgs.racket-lang.org

# Options passed along to any `raco setup` run:
PLT_SETUP_OPTIONS = 

# Belongs in the "Configuration options" section, but here
# to accomodate nmake:
SRC_CATALOG = "https://download.racket-lang.org/releases/7.3/catalog/"

CPUS = 

in-place:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-in-place PKGS="$(PKGS)" ; \
         else $(MAKE) cpus-in-place CPUS="$(CPUS)" PKGS="$(PKGS)" ; fi

cpus-in-place:
	$(MAKE) -j $(CPUS) plain-in-place JOB_OPTIONS="-j $(CPUS)" PKGS="$(PKGS)"

# Explicitly propagate variables for non-GNU `make's:
LIBSETUP = -N raco -l- raco setup

# Update before install to avoid needless work on the initial build,
# and use `--no-setup` plus an explicit `raco setup` for the same reason.
UPDATE_PKGS_ARGS = --all --auto --no-setup --scope installation
INSTALL_PKGS_ARGS = $(JOB_OPTIONS) --no-setup --pkgs \
                    --skip-installed --scope installation --deps search-auto \
                    $(REQUIRED_PKGS) $(PKGS)
ALL_PLT_SETUP_OPTIONS = $(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)

# Allow `--error-out`, etc., for final setup setp, so `make both` can
# continue from errors at that level
IN_PLACE_SETUP_OPTIONS =

plain-in-place:
	$(MAKE) plain-minimal-in-place
	$(MAKE) in-place-setup

plain-in-place-after-base:
	$(MAKE) plain-minimal-in-place-after-base
	$(MAKE) in-place-setup

plain-minimal-in-place:
	$(MAKE) plain-base
	$(MAKE) plain-minimal-in-place-after-base

plain-minimal-in-place-after-base:
	$(MAKE) pkgs-catalog
	$(RUN_RACO) pkg update $(UPDATE_PKGS_ARGS)
	$(RUN_RACO) pkg install $(INSTALL_PKGS_ARGS)
	$(RUN_RACO) setup --only-foreign-libs $(ALL_PLT_SETUP_OPTIONS)

in-place-setup:
	$(RUN_RACO) setup $(ALL_PLT_SETUP_OPTIONS) $(IN_PLACE_SETUP_OPTIONS)

win32-in-place:
	$(MAKE) win32-base
	$(MAKE) win32-in-place-after-base PKGS="$(PKGS)" SRC_CATALOG="$(SRC_CATALOG)" WIN32_PLAIN_RACKET="$(WIN32_PLAIN_RACKET)"

win32-minimal-in-place:
	$(MAKE) win32-base
	$(MAKE) win32-minimal-in-place-after-base PKGS="$(PKGS)" SRC_CATALOG="$(SRC_CATALOG)" WIN32_PLAIN_RACKET="$(WIN32_PLAIN_RACKET)"

win32-minimal-in-place-after-base:
	$(MAKE) win32-pkgs-catalog SRC_CATALOG="$(SRC_CATALOG)" WIN32_PLAIN_RACKET="$(WIN32_PLAIN_RACKET)"
	$(WIN32_RUN_RACO) pkg update $(UPDATE_PKGS_ARGS)
	$(WIN32_RUN_RACO) pkg install $(INSTALL_PKGS_ARGS)
	$(WIN32_RUN_RACO) setup --only-foreign-libs $(ALL_PLT_SETUP_OPTIONS)

win32-in-place-after-base:
	$(MAKE) win32-minimal-in-place-after-base PKGS="$(PKGS)" SRC_CATALOG="$(SRC_CATALOG)" WIN32_PLAIN_RACKET="$(WIN32_PLAIN_RACKET)"
	$(WIN32_RUN_RACO) setup $(ALL_PLT_SETUP_OPTIONS) $(IN_PLACE_SETUP_OPTIONS)

# Rebuild without consulting catalogs or package sources:

as-is:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-as-is PKGS="$(PKGS)" ; \
         else $(MAKE) cpus-as-is CPUS="$(CPUS)" PKGS="$(PKGS)" ; fi

cpus-as-is:
	$(MAKE) -j $(CPUS) plain-as-is JOB_OPTIONS="-j $(CPUS)" PKGS="$(PKGS)"

plain-as-is:
	$(MAKE) base
	$(MAKE) in-place-setup

win32-as-is:
	$(MAKE) win32-base
	$(WIN32_RUN_RACO) setup $(ALL_PLT_SETUP_OPTIONS) $(IN_PLACE_SETUP_OPTIONS)

# ------------------------------------------------------------
# Unix-style build (Unix and Mac OS, only)

PREFIX = 

CONFIG_PREFIX_ARGS = --prefix="$(PREFIX)" --enable-macprefix
UNIX_CATALOG = build/local/catalog
UNIX_RACO_ARGS = $(JOB_OPTIONS) --catalog $(UNIX_CATALOG) --auto -i
UNIX_BASE_ARGS = SELF_FLAGS_qq="" SKIP_DESTDIR_FIX="skip"

unix-style:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-unix-style ; \
         else $(MAKE) cpus-unix-style ; fi

cpus-unix-style:
	$(MAKE) -j $(CPUS) plain-unix-style JOB_OPTIONS="-j $(CPUS)"

plain-unix-style:
	if [ "$(PREFIX)" = "" ] ; then $(MAKE) error-need-prefix ; fi
	$(MAKE) base MORE_CONFIGURE_ARGS="$(MORE_CONFIGURE_ARGS) $(CONFIG_PREFIX_ARGS)" $(UNIX_BASE_ARGS)
	$(MAKE) set-src-catalog
	$(MAKE) local-catalog
	"$(DESTDIR)$(PREFIX)/bin/raco" pkg install $(UNIX_RACO_ARGS) $(REQUIRED_PKGS) $(PKGS)
	cd racket/src/build; $(MAKE) fix-paths

error-need-prefix:
	: ================================================================
	: Please supply PREFIX="<dest-dir>" to set the install destination
	: ================================================================
	exit 1

LOC_CATALOG = build/local/pkgs-catalog

local-catalog:
	"$(DESTDIR)$(PREFIX)/bin/racket" -l- pkg/dirs-catalog --check-metadata $(LOC_CATALOG) pkgs
	"$(DESTDIR)$(PREFIX)/bin/raco" pkg catalog-copy --force --from-config $(LOC_CATALOG) $(UNIX_CATALOG)

set-src-catalog:
	if [ ! "$(SRC_CATALOG)" = "$(DEFAULT_SRC_CATALOG)" ] ; \
	 then "$(DESTDIR)$(PREFIX)/bin/raco" pkg config -i --set catalogs "$(SRC_CATALOG)" ""; fi

# ------------------------------------------------------------
# Base build

# During this step, we use a configuration file that indicates
# an empty set of link files, so that any installation-wide
# links or packages are ignored during the base build.

CONFIGURE_ARGS_qq =
MORE_CONFIGURE_ARGS = 

SELF_FLAGS_qq = SELF_RACKET_FLAGS="-G `cd ../../../build/config; pwd`"
PLT_SETUP_OPTIONS_qq = PLT_SETUP_OPTIONS="$(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)"
INSTALL_SETUP_ARGS = $(SELF_FLAGS_qq) $(PLT_SETUP_OPTIONS_qq) SETUP_MACHINE_FLAGS="$(SETUP_MACHINE_FLAGS)"

BASE_INSTALL_TARGET = plain-base-install

WIN32_BUILD_LEVEL = 3m

base:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-base ; \
         else $(MAKE) cpus-base CPUS="$(CPUS)" ; fi

cpus-base:
	$(MAKE) -j $(CPUS) plain-base JOB_OPTIONS="-j $(CPUS)"

plain-base:
	$(MAKE) base-config
	$(MAKE) racket/src/build/Makefile
	cd racket/src/build; $(MAKE) reconfigure
	cd racket/src/build; $(MAKE) racket-variant $(SELF_FLAGS_qq)
	$(MAKE) $(BASE_INSTALL_TARGET)

plain-base-install:
	cd racket/src/build; $(MAKE) install-racket-variant $(INSTALL_SETUP_ARGS)

base-config:
	mkdir -p build/config
	echo '#hash((links-search-files . ()))' > build/config/config.rktd

win32-base:
	$(MAKE) win32-remove-setup-dlls
	IF NOT EXIST build\config cmd /c mkdir build\config
	cmd /c echo #hash((links-search-files . ())) > build\config\config.rktd
	cmd /c racket\src\worksp\build-at racket\src\worksp ..\..\..\build\config $(WIN32_BUILD_LEVEL) $(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)

# Start by removing DLLs that may be loaded by `raco setup`
win32-remove-setup-dlls:
	IF EXIST racket\lib\longdouble.dll cmd /c del racket\lib\longdouble.dll
	IF EXIST racket\lib\libiconv-2.dll cmd /c del racket\lib\libiconv-2.dll
	IF EXIST racket\lib\iconv.dll cmd /c del racket\lib\iconv.dll
	IF EXIST racket\lib\libeay32.dll cmd /c del racket\lib\libeay32.dll
	IF EXIST racket\lib\ssleay32.dll cmd /c del racket\lib\ssleay32.dll

SRC_MAKEFILE_CONFIG = configure

racket/src/build/Makefile: racket/src/$(SRC_MAKEFILE_CONFIG) racket/src/Makefile.in
	mkdir -p racket/src/build
	cd racket/src/build; ../$(SRC_MAKEFILE_CONFIG) $(CONFIGURE_ARGS_qq) $(MORE_CONFIGURE_ARGS)

MORE_CROSS_CONFIGURE_ARGS =

# For cross-compilation, build a native executable with no configure options:
native-for-cross:
	mkdir -p racket/src/build/cross
	$(MAKE) racket/src/build/cross/Makefile
	cd racket/src/build/cross; $(MAKE) reconfigure MORE_CONFIGURE_ARGS="$(MORE_CROSS_CONFIGURE_ARGS)"
	cd racket/src/build/cross/racket; $(MAKE)

racket/src/build/cross/Makefile: racket/src/configure racket/src/Makefile.in
	cd racket/src/build/cross; ../../configure $(MORE_CROSS_CONFIGURE_ARGS)

# ------------------------------------------------------------
# Racket-on-Chez build

# If `RACKETCS_SUFFIX` is set to the empty string, the Racket-on-Chez
# is build as `racket` instead of `racketcs`. Also, if `RACKET`
# is not set, then `--enable-csdefault` is added to 
RACKETCS_SUFFIX = cs

# If `RACKET` is not set, then we bootstrap by first building the
# traditional virtual machine
RACKET =

# The built traditional Racket:
RACKET_BUILT_FOR_CS = racket/src/build/racket/racket3m

# If `SCHEME_SRC` is not set, then we'll download a copy of
# Chez Scheme from `CHEZ_SCHEME_REPO`
SCHEME_SRC = 
DEFAULT_SCHEME_SRC = racket/src/build/ChezScheme
MAKE_BUILD_SCHEME = y

CHEZ_SCHEME_REPO = https://github.com/mflatt/ChezScheme
GIT_CLONE_ARGS_qq = -q --depth 1

# Altenative source for Chez Scheme repo, normally set by
# the distro-build client driver
EXTRA_REPOS_BASE =

# Set to "-cross" for a cross build:
CS_CROSS_SUFFIX =

# Redirected for `cs-as-is` and `cs-base`:
CS_SETUP_TARGET = plain-in-place-after-base
WIN32_CS_SETUP_TARGET = win32-in-place-after-base

cs:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-cs ; \
         else $(MAKE) cpus-cs CPUS="$(CPUS)" ; fi

plain-cs:
	if [ "$(SCHEME_SRC)" = "" ] ; \
         then $(MAKE) scheme-src ; fi
	if [ "$(RACKET)" = "" ] ; \
         then $(MAKE) racket-then-cs ; \
         else $(MAKE) cs-only RACKET="$(RACKET)" ; fi

cpus-cs:
	$(MAKE) -j $(CPUS) plain-cs JOB_OPTIONS="-j $(CPUS)"

cs-in-place:
	$(MAKE) cs

cs-base:
	$(MAKE) cs CS_SETUP_TARGET=nothing-after-base

cs-as-is:
	$(MAKE) cs CS_SETUP_TARGET=in-place-setup

CS_CONFIG_TARGET = no-cfg-cs

cs-after-racket:
	if [ "$(RACKET)" = "" ] ; \
         then $(MAKE) cs-after-racket-with-racket RACKET="$(RACKET_BUILT_FOR_CS)" SETUP_BOOT_MODE=--boot ; \
         else $(MAKE) cs-after-racket-with-racket RACKET="$(RACKET)" CS_CONFIG_TARGET=run-cfg-cs ; fi

RACKETCS_NOSUFFIX_CONFIG = MORE_CONFIGURE_ARGS="$(MORE_CONFIGURE_ARGS) --enable-csdefault"

racket-then-cs:
	if [ "$(RACKETCS_SUFFIX)" = "" ] ; \
         then $(MAKE) racket-configured-then-cs $(RACKETCS_NOSUFFIX_CONFIG) PLAIN_RACKET="$(PLAIN_RACKET)3m" ; \
         else $(MAKE) racket-configured-then-cs ; fi

racket-configured-then-cs:
	$(MAKE) plain-base BASE_INSTALL_TARGET=nothing-after-base
	$(MAKE) cs-after-racket-with-racket RACKET="$(RACKET_BUILT_FOR_CS)" SETUP_BOOT_MODE=--boot

cs-only:
	$(MAKE) racket/src/build/Makefile SRC_MAKEFILE_CONFIG=cfg-cs
	if [ "$(RACKETCS_SUFFIX)" = "" ] ; \
	  then $(MAKE) cs-after-racket-with-racket $(RACKETCS_NOSUFFIX_CONFIG) RACKET="$(RACKET)" ; \
	  else $(MAKE) cs-after-racket-with-racket RACKET="$(RACKET)" ; fi

SETUP_BOOT_MODE = --chain
ABS_SETUP_BOOT = -l- setup $(SETUP_BOOT_MODE) racket/src/setup-go.rkt racket/src/build/compiled
ABS_BOOT = $(ABS_SETUP_BOOT) ignored racket/src/build/ignored.d
ABS_RACKET = `$(RACKET) $(ABS_BOOT) racket/src/cs/absify.rkt --exec $(RACKET)`
ABS_SCHEME_SRC = `$(RACKET) $(ABS_BOOT) racket/src/cs/absify.rkt $(SCHEME_SRC)`

cs-after-racket-with-racket:
	if [ "$(SCHEME_SRC)" = "" ] ; \
	  then $(MAKE) cs-after-racket-with-racket-and-scheme-src RACKET="$(RACKET)" SCHEME_SRC="$(DEFAULT_SCHEME_SRC)" ; \
	  else $(MAKE) cs-after-racket-with-racket-and-scheme-src RACKET="$(RACKET)" SCHEME_SRC="$(SCHEME_SRC)" MAKE_BUILD_SCHEME=n ; fi

cs-after-racket-with-racket-and-scheme-src:
	$(RACKET) -O "info@compiler/cm" $(ABS_BOOT) racket/src/cs/absify.rkt just-to-compile-absify
	$(MAKE) cs-after-racket-with-abs-paths RACKET="$(ABS_RACKET)" SCHEME_SRC="$(ABS_SCHEME_SRC)"

cs-after-racket-with-abs-paths:
	$(MAKE) racket/src/build/cs/c/Makefile
	cd racket/src/build/cs/c; $(MAKE) RACKET="$(RACKET)" SCHEME_SRC="$(SCHEME_SRC)" MAKE_BUILD_SCHEME="$(MAKE_BUILD_SCHEME)"
	$(MAKE) base-config
	cd racket/src/build; $(MAKE) install-cs RACKET="$(RACKET)" CS_INSTALLED=$(RACKETCS_SUFFIX) $(INSTALL_SETUP_ARGS)
	$(MAKE) cs-setup$(CS_CROSS_SUFFIX)

cs-setup:
	$(MAKE) $(CS_SETUP_TARGET) PLAIN_RACKET=racket/bin/racket$(RACKETCS_SUFFIX)

cs-setup-cross:
	$(MAKE) $(CS_SETUP_TARGET) PLAIN_RACKET="$(RACKET)" PLT_SETUP_OPTIONS="--no-pkg-deps $(PLT_SETUP_OPTIONS)"

nothing-after-base:
	echo base done

racket/src/build/cs/c/Makefile: racket/src/cs/c/configure racket/src/cs/c/Makefile.in racket/src/cfg-cs
	mkdir -p cd racket/src/build/cs/c
	cd racket/src/build/cs/c; ../../../cs/c/configure $(CONFIGURE_ARGS_qq) $(MORE_CONFIGURE_ARGS)
	$(MAKE) $(CS_CONFIG_TARGET)

run-cfg-cs:
	cd racket/src/build; ../cfg-cs $(CONFIGURE_ARGS_qq) $(MORE_CONFIGURE_ARGS)

no-cfg-cs:
	echo done

BUILD_FOR_FOR_SCHEME_DIR = racket/src/build

scheme-src:
	$(MAKE) $(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme
	$(MAKE) update-ChezScheme

$(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme:
	mkdir -p $(BUILD_FOR_FOR_SCHEME_DIR)
	if [ "$(EXTRA_REPOS_BASE)" = "" ] ; \
          then cd $(BUILD_FOR_FOR_SCHEME_DIR) && git clone $(GIT_CLONE_ARGS_qq) $(CHEZ_SCHEME_REPO) ChezScheme ; \
          else $(MAKE) clone-ChezScheme-as-extra GIT_CLONE_ARGS_qq="" ; fi

# For this target, `GIT_CLONE_ARGS_qq` normally should not include "--depth 1",
# because `EXTRA_REPOS_BASE` is likely to be a dumb transport that does not
# support shallow copies
clone-ChezScheme-as-extra:
	cd $(BUILD_FOR_FOR_SCHEME_DIR) && git clone $(GIT_CLONE_ARGS_qq) $(EXTRA_REPOS_BASE)ChezScheme/.git
	cd $(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme && git clone $(GIT_CLONE_ARGS_qq) $(EXTRA_REPOS_BASE)nanopass/.git
	cd $(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme && git clone $(GIT_CLONE_ARGS_qq) $(EXTRA_REPOS_BASE)stex/.git
	cd $(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme && git clone $(GIT_CLONE_ARGS_qq) $(EXTRA_REPOS_BASE)zlib/.git
	cd $(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme && git clone $(GIT_CLONE_ARGS_qq) $(EXTRA_REPOS_BASE)lz4/.git

update-ChezScheme:
	if [ "$(EXTRA_REPOS_BASE)" = "" ] ; \
         then $(MAKE) update-ChezScheme-normal ; \
         else $(MAKE) update-ChezScheme-as-extra ; fi

update-ChezScheme-normal:
	cd $(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme && git pull -q && git submodule -q init && git submodule -q update

update-ChezScheme-as-extra:
	cd $(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme && git pull -q
	cd $(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme/nanopass && git pull origin master -q
	cd $(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme/stex && git pull origin master -q
	cd $(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme/zlib && git pull origin master -q
	cd $(BUILD_FOR_FOR_SCHEME_DIR)/ChezScheme/lz4 && git pull origin master -q

WIN32_CS_COPY_ARGS_EXCEPT_PKGS_SUT = SRC_CATALOG="$(SRC_CATALOG)" RACKETCS_SUFFIX="$(RACKETCS_SUFFIX)" \
                                     SCHEME_SRC="$(SCHEME_SRC)" EXTRA_REPOS_BASE="$(EXTRA_REPOS_BASE)"
WIN32_CS_COPY_ARGS_EXCEPT_SUT = PKGS="$(PKGS)" $(WIN32_CS_COPY_ARGS_EXCEPT_PKGS_SUT)
WIN32_CS_COPY_ARGS = PKGS="$(PKGS)" WIN32_CS_SETUP_TARGET=$(WIN32_CS_SETUP_TARGET) $(WIN32_CS_COPY_ARGS_EXCEPT_PKGS_SUT)
WIN32_CS_COPY_ARGS_BOOT = $(WIN32_CS_COPY_ARGS) SETUP_BOOT_MODE="$(SETUP_BOOT_MODE)" WIN32_BUILD_LEVEL="$(WIN32_BUILD_LEVEL)"

WIN32_BOOT_ARGS = SETUP_BOOT_MODE=--boot WIN32_BUILD_LEVEL=cgc WIN32_PLAIN_RACKET=racket\racketcgc

win32-cs:
	IF "$(RACKET)" == "" $(MAKE) win32-racket-then-cs $(WIN32_BOOT_ARGS) $(WIN32_CS_COPY_ARGS)
	IF not "$(RACKET)" == "" $(MAKE) win32-just-cs RACKET="$(RACKET)" $(WIN32_CS_COPY_ARGS)

win32-cs-base:
	$(MAKE) win32-cs $(WIN32_CS_COPY_ARGS_EXCEPT_SUT) RACKET="$(RACKET)" WIN32_CS_SETUP_TARGET=nothing-after-base

win32-racket-then-cs:
	$(MAKE) win32-base PKGS="" $(WIN32_CS_COPY_ARGS_EXCEPT_PKGS_SUT) WIN32_BUILD_LEVEL="$(WIN32_BUILD_LEVEL)"
	$(MAKE) win32-just-cs RACKET=$(WIN32_PLAIN_RACKET) $(WIN32_CS_COPY_ARGS_BOOT)

CSBUILD_ARGUMENTS = --scheme-dir "$(SCHEME_SRC)" --pull \
                    --racketcs-suffix "$(RACKETCS_SUFFIX)" \
                    --boot-mode "$(SETUP_BOOT_MODE)" \
                    --extra-repos-base "$(EXTRA_REPOS_BASE)" \
                    -- $(GIT_CLONE_ARGS_qq)

WIN32_SETUP_BOOT = -O "info@compiler/cm" \
                   -l- setup $(SETUP_BOOT_MODE) racket/src/setup-go.rkt racket/src/build/compiled \
                   ignored racket/src/build/ignored.d

win32-just-cs:
	IF NOT EXIST racket\src\build cmd /c mkdir racket\src\build
	cmd /c $(RACKET) $(WIN32_SETUP_BOOT) racket\src\worksp\csbuild.rkt $(CSBUILD_ARGUMENTS)
	IF NOT EXIST build\config cmd /c mkdir build\config
	cmd /c echo #hash((links-search-files . ())) > build\config\config.rktd
	racket\racket$(RACKETCS_SUFFIX) -G build\config -N raco -l- raco setup $(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)
	$(MAKE) $(WIN32_CS_SETUP_TARGET) WIN32_PLAIN_RACKET=racket\racket$(RACKETCS_SUFFIX) $(WIN32_CS_COPY_ARGS)


# For cross-compilation, build a native executable with no configure options:
native-cs-for-cross:
	if [ "$(SCHEME_SRC)" = "" ] ; \
         then $(MAKE) scheme-src-then-cross ; \
         else $(MAKE) native-cs-for-cross-after-scheme-src MAKE_BUILD_SCHEME=n ; fi

CS_CROSS_SCHEME_CONFIG = SCHEME_SRC="`pwd`/racket/src/build/cross/ChezScheme" MAKE_BUILD_SCHEME=y

scheme-src-then-cross:
	$(MAKE) scheme-src BUILD_FOR_FOR_SCHEME_DIR="racket/src/build/cross/"
	$(MAKE) native-cs-for-cross-after-scheme-src $(CS_CROSS_SCHEME_CONFIG)

native-cs-for-cross-after-scheme-src:
	if [ "$(RACKET)" = "" ] ; \
         then $(MAKE) native-for-cross-racket-then-cross ; \
         else $(MAKE) native-cs-for-cross-finish ; fi

CS_CROSS_CONFIG_CONFIG = MORE_CROSS_CONFIGURE_ARGS="$(MORE_CROSS_CONFIGURE_ARGS) --enable-csdefault"

native-for-cross-racket-then-cross:
	$(MAKE) native-for-cross $(CS_CROSS_CONFIG_CONFIG)
	$(MAKE) native-cs-for-cross-finish RACKET="`pwd`/racket/src/build/cross/racket/racket3m"

native-cs-for-cross-finish:
	mkdir -p racket/src/build/cross/cs/c
	$(MAKE) racket/src/build/cross/cs/c/Makefile
	cd racket/src/build/cross/cs/c; $(MAKE) reconfigure
	cd racket/src/build/cross/cs/c; $(MAKE)

racket/src/build/cross/cs/c/Makefile: racket/src/cs/c/configure racket/src/cs/c/Makefile.in
	cd racket/src/build/cross/cs/c; ../../../../cs/c/configure --enable-csdefault

# ------------------------------------------------------------
# Both traditional Racket and RacketCS
# ... but update packages and builds docs only once

both:
	$(MAKE) in-place IN_PLACE_SETUP_OPTIONS="--error-out build/step"
	$(MAKE) also-cs IN_PLACE_SETUP_OPTIONS="--error-in build/step"

also-cs:
	$(MAKE) cs CS_SETUP_TARGET=in-place-setup PLT_SETUP_OPTIONS="-D $(PLT_SETUP_OPTIONS)"

# ------------------------------------------------------------
# Configuration options for building installers

# On variable definitions: Spaces are allowed where noted and
# disallowed otherwise. If a variable name ends in "_q", then it means
# that the variable can expand to include double-quote marks. If a
# variable's name ends in "_qq", then it expands to a combination of
# single-quote and double-quote marks. If a variable's name does not
# end in "_q" or "_qq", don't use any quote marks on the right-hand
# side of its definition.

# Catalog for package sources (defined above):
# SRC_CATALOG = $(DEFAULT_SRC_CATALOG)

# A URL embedded in documentation for remote searches, where a Racket
# version and search key are added as query fields to the URL, and ""
# is replaced by default:
DOC_SEARCH = 

# Server for built packages (i.e., the host where you'll run the
# server):
SERVER = localhost
SERVER_PORT = 9440

# Paths on the server to reach catalog content and "collects.tgz",
# if not the root:
SERVER_CATALOG_PATH =
SERVER_COLLECTS_PATH =

# Set `SERVER_HOSTS` to a comma-delimited set of server addresses
# that determine the interfaces on which the server listens; the
# default, "localhost", listens only on the loopback device, while
# anf empty value listens on all interfaces:
SERVER_HOSTS = localhost

# Set to "--release" to create release-mode installers (as opposed to
# snapshot installers):
RELEASE_MODE =

# Set to "--source" to create an archive (instead of an "installer"
# proper) on a client that has the run-time system in source form:
SOURCE_MODE =

# Set to "--versionless" to avoid a version number in an installer's
# name or installation path:
VERSIONLESS_MODE =

# Set to "--mac-pkg" to create ".pkg"-based installers for Mac OS,
# instead of a ".dmg" for drag-and-drop installation:
MAC_PKG_MODE =

# Set to "--tgz" to create a ".tgz" archive instead of an installer:
TGZ_MODE =

# Set to "--source --no-setup" to include packages in an installer
# (or archive) only in source form:
PKG_SOURCE_MODE = 

# Set a base64-encoded list of strings for an executable and argument
# to run on an installer (on the client machine) before the installer
# is uploaded, or empty for no post-process action:
INSTALLER_POST_PROCESS_BASE64 =

# Human-readable name (spaces allowed), installation name base, and
# Unix installation directory name for the generated installers:
DIST_NAME = Racket
DIST_BASE = racket
DIST_DIR = racket
# An extra suffix for the installer name, usually used to specify
# a variant of an OS:
DIST_SUFFIX = 
# A human-readable description (spaces allowed) of the generated
# installer, usually describing a platform, used for upload:
DIST_DESC =

# Package catalog URLs (individually quoted as needed, separated by
# spaces) to install as the initial configuration in generated
# installers, where "" is replaced by the default configuration:
DIST_CATALOGS_q = ""

# An identifier for this build; if not specified, a build identifier
# is inferred from the date and git repository
BUILD_STAMP = 

# "Name" of the installation used for `user' package scope by default
# in an installation from an installer, where an empty value leaves
# the default as the version number:
INSTALL_NAME =

# For Mac OS, a signing identity (spaces allowed) for binaries in an
# installer:
SIGN_IDENTITY = 

# For Windows, `osslsigncode' arguments other than `-n', `-t', `-in',
# and `-out' as a Base64-encoded, S-expression, list of strings:
OSSLSIGNCODE_ARGS_BASE64 =

# URL for a README file to include in an installer (empty for none,
# spaces allowed):
README = http://$(SVR_PRT)/README.txt

# URL destination to upload an installer file after it is created
# (empty for no upload, spaces allowed); the file name is added to the
# end of the URL, and DIST_DESC is passed as a "Description:" header:
UPLOAD = 

# Configuration module that describes a build, normally implemented
# with `#lang distro-build/config':
CONFIG = build/site.rkt

# A mode that is made available to the site-configuration module
# through the `current-mode' parameter:
CONFIG_MODE = default

# Set to "--clean" to flush client directories in a build farm
# (except as overridden in the `CONFIG' module):
CLEAN_MODE =

# Determines the number of parallel jobs used for package and
# setup operations:
JOB_OPTIONS =

# $(USER_RACKET) arguments for a command to run after the server has
# started; normally set by the `installers' target:
SERVE_DURING_CMD_qq =

# ------------------------------------------------------------
# Helpers

# Needed for any distribution:
REQUIRED_PKGS = racket-lib

# Packages needed for building distribution:
DISTRO_BUILD_PKGS = distro-build-lib

SVR_PRT = $(SERVER):$(SERVER_PORT)

SVR_CAT = http://$(SVR_PRT)/$(SERVER_CATALOG_PATH)

# To configure package installations on the server:
SERVER_PKG_INSTALL_OPTIONS =

# Catch problems due to malformed distribution-build packages
RECOMPILE_OPTIONS = --recompile-only

# Helper macros:
USER_CONFIG = -G build/user/config -X racket/collects -A build/user $(SETUP_MACHINE_FLAGS)
USER_RACKET = $(PLAIN_RACKET) $(USER_CONFIG)
USER_RACO = $(PLAIN_RACKET) $(USER_CONFIG) -N raco -l- raco
WIN32_RACKET = $(WIN32_PLAIN_RACKET) $(USER_CONFIG)
WIN32_RACO = $(WIN32_PLAIN_RACKET) $(USER_CONFIG) -N raco -l- raco
X_AUTO_OPTIONS = --skip-installed --deps search-auto --pkgs $(JOB_OPTIONS)
USER_AUTO_OPTIONS = --scope user $(X_AUTO_OPTIONS)
SOURCE_USER_AUTO_q = --catalog build/catalog-copy $(USER_AUTO_OPTIONS) $(SERVER_PKG_INSTALL_OPTIONS)
REMOTE_USER_AUTO = --catalog $(SVR_CAT) $(USER_AUTO_OPTIONS)
REMOTE_INST_AUTO = --catalog $(SVR_CAT) --scope installation $(X_AUTO_OPTIONS) $(RECOMPILE_OPTIONS)
CONFIG_MODE_q = "$(CONFIG)" "$(CONFIG_MODE)"
BUNDLE_CONFIG = bundle/racket/etc/config.rktd
BUNDLE_RACO_FLAGS = -G bundle/racket/etc -X bundle/racket/collects -C -A bundle/user -l raco
BUNDLE_RACO = $(PLAIN_RACKET) $(BUNDLE_RACO_FLAGS)
WIN32_BUNDLE_RACO = $(WIN32_PLAIN_RACKET) $(BUNDLE_RACO_FLAGS)
IN_BUNDLE_RACO = bundle/racket/bin/raco
WIN32_IN_BUNDLE_RACO = bundle\racket\raco

# ------------------------------------------------------------
# Linking all packages (development mode; not an installer build)

PKGS_CATALOG = -U -G build/config -l- pkg/dirs-catalog --link --check-metadata --immediate
PKGS_CONFIG = -U -G build/config racket/src/pkgs-config.rkt

pkgs-catalog:
	$(RUN_RACKET) $(PKGS_CATALOG) racket/share/pkgs-catalog pkgs racket/src/expander
	$(RUN_RACKET) $(PKGS_CONFIG) "$(DEFAULT_SRC_CATALOG)" "$(SRC_CATALOG)"
	$(RUN_RACKET) racket/src/pkgs-check.rkt racket/share/pkgs-catalog

COPY_PKGS_ARGS = PLAIN_RACKET="$(WIN32_PLAIN_RACKET)" SRC_CATALOG="$(SRC_CATALOG)"

win32-pkgs-catalog:
	$(MAKE) pkgs-catalog $(COPY_PKGS_ARGS)

# ------------------------------------------------------------
# Handle `SERVER_COMPILE_MACHINE` for various targets

SERVER_COMPILE_MACHINE = -M
ANY_COMPILE_MACHINE_ARGS_qq = SETUP_MACHINE_FLAGS="-MCR `pwd`/build/zo:" \
                              MORE_CONFIGURE_ARGS="$(MORE_CONFIGURE_ARGS) --enable-crossany"

with-setup-flags:
	if [ "$(SERVER_COMPILE_MACHINE)" = "-M" ] ; \
         then $(MAKE) $(NEXT_TARGET) $(ANY_COMPILE_MACHINE_ARGS_qq) ; \
         else $(MAKE) $(NEXT_TARGET) ; fi

random:
	echo $(MORE_CONFIGURE_ARGS)

# ------------------------------------------------------------
# On a server platform (for an installer build):

# These targets require GNU `make', so that we don't have to propagate
# variables through all of the target layers.

server:
	$(MAKE) with-setup-flags NEXT_TARGET=plain-server

plain-server:
	rm -rf build/zo
	$(MAKE) base
	$(MAKE) server-from-base

server-from-base:
	$(MAKE) build/site.rkt
	$(MAKE) stamp
	$(MAKE) build-from-catalog
	$(MAKE) origin-collects
	$(MAKE) built-catalog
	$(MAKE) built-catalog-server

build/site.rkt:
	mkdir -p build
	echo "#lang distro-build/config" > build/site.rkt
	echo "(machine)" >> build/site.rkt

stamp:
	if [ "$(BUILD_STAMP)" = '' ] ; \
          then $(MAKE) stamp-as-inferred ; \
          else $(MAKE) stamp-as-given ; fi
stamp-as-given:
	echo "$(BUILD_STAMP)" > build/stamp.txt
stamp-as-inferred:
	if [ -d ".git" ] ; then $(MAKE) stamp-from-git ; else $(MAKE) stamp-from-date ; fi
stamp-from-git:
	echo `date +"%Y%m%d"`-`git log -1 --pretty=format:%h` > build/stamp.txt
stamp-from-date:
	date +"%Y%m%d" > build/stamp.txt

# Created a copy of `SRC_CATALOG', so that we snapshot checksums, and
# start building from it. The packages are installed in user scope,
# but we set the add-on directory to "build/user", so that we don't
# affect the actual current user's installation (and to a large degree
# we're insulated from it):
build-from-catalog:
	rm -rf build/user
	rm -rf build/catalog-copy
	$(USER_RACO) pkg catalog-copy "$(SRC_CATALOG)" build/catalog-copy
	$(MAKE) server-cache-config
	$(USER_RACO) pkg install --all-platforms $(SOURCE_USER_AUTO_q) $(REQUIRED_PKGS) $(DISTRO_BUILD_PKGS)
	$(MAKE) set-server-config
	$(USER_RACKET) -l- distro-build/pkg-info -o build/pkgs.rktd build/catalog-copy
	$(USER_RACKET) -l distro-build/install-pkgs $(CONFIG_MODE_q) "$(PKGS) $(TEST_PKGS)" $(SOURCE_USER_AUTO_q) --all-platforms
	$(USER_RACO) setup --avoid-main $(JOB_OPTIONS)

server-cache-config:
	$(USER_RACO) pkg config -i --set download-cache-dir build/cache
	$(USER_RACO) pkg config -i --set download-cache-max-files 1023
	$(USER_RACO) pkg config -i --set download-cache-max-bytes 671088640

set-server-config:
	$(USER_RACKET) -l distro-build/set-server-config build/user/config/config.rktd $(CONFIG_MODE_q) "" "" "$(DOC_SEARCH)" ""

# Although a client will build its own "collects", pack up the
# server's version to be used by each client, so that every client has
# exactly the same bytecode (which matters for SHA1-based dependency
# tracking):
origin-collects:
	$(USER_RACKET) -l distro-build/pack-collects

# Now that we've built packages from local sources, create "built"
# versions of the packages from the installation into "build/user":
built-catalog:
	$(USER_RACKET) -l distro-build/pack-built build/pkgs.rktd

# Run a catalog server to provide pre-built packages, as well
# as the copy of the server's "collects" tree:
built-catalog-server:
	if [ -d ".git" ]; then git update-server-info ; fi
	$(USER_RACKET) -l distro-build/serve-catalog $(CONFIG_MODE_q) "$(SERVER_HOSTS)" $(SERVER_PORT) $(USER_RACKET) $(SERVE_DURING_CMD_qq)

# Demonstrate how a catalog server for binary packages works,
# which involves creating package archives in "binary" mode
# instead of "built" mode:
binary-catalog:
	$(USER_RACKET) -l- distro-build/pack-built --mode binary build/pkgs.rktd
binary-catalog-server:
	$(USER_RACKET) -l- distro-build/serve-catalog --mode binary $(CONFIG_MODE_q) "$(SERVER_HOSTS)" $(SERVER_PORT)

# ------------------------------------------------------------
# On each supported platform (for an installer build):
#
# The `client' and `win32-client' targets are also used by
# `distro-buid/drive-clients', which is in turn run by the
# `installers' target.
#
# For a non-Windows machine, if "build/log" exists, then
# keep the "build/user" directory on the grounds that the
# client is the same as the server.

# These can get replaced by `cs-base` and `win32-cs-base`:
CLIENT_BASE = base
WIN32_CLIENT_BASE = win32-base

PROP_ARGS = SERVER=$(SERVER) SERVER_PORT=$(SERVER_PORT) SERVER_HOSTS="$(SERVER_HOSTS)" \
            PKGS="$(PKGS)" PLAIN_RACKET="$(PLAIN_RACKET)" BUILD_STAMP="$(BUILD_STAMP)" \
	    EXTRA_REPOS_BASE="$(EXTRA_REPOS_BASE)" RACKETCS_SUFFIX="$(RACKETCS_SUFFIX)" \
	    RELEASE_MODE=$(RELEASE_MODE) SOURCE_MODE=$(SOURCE_MODE) \
            VERSIONLESS_MODE=$(VERSIONLESS_MODE) MAC_PKG_MODE=$(MAC_PKG_MODE) \
            PKG_SOURCE_MODE="$(PKG_SOURCE_MODE)" INSTALL_NAME="$(INSTALL_NAME)" \
            UNPACK_COLLECTS_FLAGS="$(UNPACK_COLLECTS_FLAGS)" \
            DIST_NAME="$(DIST_NAME)" DIST_BASE=$(DIST_BASE) \
            DIST_DIR=$(DIST_DIR) DIST_SUFFIX=$(DIST_SUFFIX) UPLOAD="$(UPLOAD)" \
            DIST_DESC="$(DIST_DESC)" README="$(README)" SIGN_IDENTITY="$(SIGN_IDENTITY)" \
            OSSLSIGNCODE_ARGS_BASE64="$(OSSLSIGNCODE_ARGS_BASE64)" JOB_OPTIONS="$(JOB_OPTIONS)" \
            TGZ_MODE=$(TGZ_MODE) TEST_PKGS="$(TEST_PKGS)" \
            INSTALLER_POST_PROCESS_BASE64="$(INSTALLER_POST_PROCESS_BASE64)"

COPY_ARGS = $(PROP_ARGS) \
            SERVER_CATALOG_PATH=$(SERVER_CATALOG_PATH) SERVER_COLLECTS_PATH=$(SERVER_COLLECTS_PATH)

# Not copied, because used only immediately: DOC_SEARCH and DIST_CATALOGS_q

SET_BUNDLE_CONFIG_q = $(BUNDLE_CONFIG) "$(INSTALL_NAME)" "$(BUILD_STAMP)" "$(DOC_SEARCH)" $(DIST_CATALOGS_q)

# Can be redirected to `bundle-cross-from-server`:
BUNDLE_FROM_SERVER_TARGET = bundle-from-server

client:
	if [ ! -d build/log ] ; then rm -rf build/user ; fi
	$(MAKE) $(CLIENT_BASE) $(COPY_ARGS)
	$(MAKE) distro-build-from-server $(COPY_ARGS)
	$(MAKE) $(BUNDLE_FROM_SERVER_TARGET) $(COPY_ARGS)
	$(USER_RACKET) -l distro-build/set-config $(SET_BUNDLE_CONFIG_q)
	$(MAKE) installer-from-bundle $(COPY_ARGS)

win32-client:
	IF EXIST build\user cmd /c del /f /s /q build\user
	$(MAKE) $(WIN32_CLIENT_BASE) $(COPY_ARGS)
	$(MAKE) win32-distro-build-from-server $(COPY_ARGS)
	$(MAKE) win32-bundle-from-server $(COPY_ARGS)
	$(WIN32_RACKET) -l distro-build/set-config $(SET_BUNDLE_CONFIG_q)
	$(MAKE) win32-installer-from-bundle $(COPY_ARGS)

# Install the "distro-build" package from the server into
# a local build:
distro-build-from-server:
	$(USER_RACO) pkg install $(REMOTE_USER_AUTO) distro-build-client

# Copy our local build into a "bundle/racket" build, dropping in the
# process things that should not be in an installer (such as the "src"
# directory). Then, replace the "collects" tree with the one from the
# server. Run `raco setup` in case the replacing "collects" tree needs
# recompiling. Install required packages next, because they may include
# packages that are needed to make core functionality work right
# (which as the SQLite3 library). At last, install the selected packages
# from the server, and the run a post-adjustment script.
bundle-from-server:
	rm -rf bundle
	mkdir -p bundle/racket
	$(USER_RACKET) -l setup/unixstyle-install bundle racket bundle/racket
	$(USER_RACKET) -l setup/winstrip bundle/racket
	$(USER_RACKET) -l setup/winvers-change bundle/racket
	$(USER_RACKET) -l- distro-build/unpack-collects $(UNPACK_COLLECTS_FLAGS) http://$(SVR_PRT)/$(SERVER_COLLECTS_PATH)
	$(IN_BUNDLE_RACO) setup $(JOB_OPTIONS) $(RECOMPILE_OPTIONS)
	$(IN_BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(REQUIRED_PKGS)
	$(IN_BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(PKGS)
	$(USER_RACKET) -l setup/unixstyle-install post-adjust "$(SOURCE_MODE)" "$(PKG_SOURCE_MODE)" racket bundle/racket

# For a cross build, we still need to use `$(BUNDLE_RACO)` for
# installing packages. The host build must have all native libraries
# that installation will need.
bundle-cross-from-server:
	rm -rf "build/zo`pwd`/bundle"
	$(MAKE) bundle-from-server $(COPY_ARGS) IN_BUNDLE_RACO="$(PLAIN_RACKET) $(SETUP_MACHINE_FLAGS) $(BUNDLE_RACO_FLAGS)"

UPLOAD_q = --readme "$(README)" --upload "$(UPLOAD)" --desc "$(DIST_DESC)"
DIST_ARGS_q = $(UPLOAD_q) $(RELEASE_MODE) $(SOURCE_MODE) $(VERSIONLESS_MODE) \
              $(MAC_PKG_MODE) $(TGZ_MODE) --post-process "$(INSTALLER_POST_PROCESS_BASE64)" \
              "$(DIST_NAME)" $(DIST_BASE) $(DIST_DIR) "$(DIST_SUFFIX)" \
              "$(SIGN_IDENTITY)" "$(OSSLSIGNCODE_ARGS_BASE64)"

# Create an installer from the build (with installed packages) that's
# in "bundle/racket":
installer-from-bundle:
	$(USER_RACKET) -l- distro-build/installer $(DIST_ARGS_q)

win32-distro-build-from-server:
	$(WIN32_RACO) pkg install $(REMOTE_USER_AUTO) distro-build-client

win32-bundle:
	IF EXIST bundle cmd /c rmdir /S /Q bundle
	cmd /c mkdir bundle\racket
	$(WIN32_RACKET) -l setup/unixstyle-install bundle$(SOURCE_MODE) racket bundle\racket
	$(WIN32_RACKET) -l setup/winstrip bundle\racket
	$(WIN32_RACKET) -l setup/winvers-change bundle\racket

win32-bundle-from-server:
	$(MAKE) win32-bundle $(COPY_ARGS)
	$(WIN32_RACKET) -l- distro-build/unpack-collects $(UNPACK_COLLECTS_FLAGS) http://$(SVR_PRT)/$(SERVER_COLLECTS_PATH)
	$(WIN32_IN_BUNDLE_RACO) setup -l racket/base
	$(WIN32_IN_BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(REQUIRED_PKGS)
	$(WIN32_IN_BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(PKGS)

win32-installer-from-bundle:
	$(WIN32_RACKET) -l- distro-build/installer $(DIST_ARGS_q)

# The `test-client` and `win32-test-client` targets are optional test
# step for an installer build, were `TEST_PKGS` names extra packages
# to install, and `TEST_ARGS_q` is a set of arguments to `raco test`.
# This step will not make sense for some kinds of builds, such as
# source builds or cross-platform builds.

test-client:
	$(BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(TEST_PKGS)
	$(IN_BUNDLE_RACO) test $(TEST_ARGS_q)

win32-test-client:
	$(WIN32_BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(TEST_PKGS)
	$(WIN32_IN_BUNDLE_RACO) test $(TEST_ARGS_q)

# ------------------------------------------------------------
# On a supported platform (for an installer build) after a `make site'
# has completed; SERVER, SERVER_PORT (usually 80), and SITE_PATH
# should be set, and other configurations are propagated; normally,
# README should be set (possibly to empty), because a site doesn't
# provide a generic "README.txt".

# Relative path on server for the site; include a trailing "/"
# if non-empty:
SITE_PATH =

FROM_SITE_ARGS = SERVER_CATALOG_PATH=$(SITE_PATH)catalog/ SERVER_COLLECTS_PATH=$(SITE_PATH)origin/ \
                 DIST_CATALOGS_q='http://$(SERVER):$(SERVER_PORT)/$(SITE_PATH)catalog/ ""' \
                 DOC_SEARCH="http://$(SERVER):$(SERVER_PORT)/$(SITE_PATH)doc/local-redirect/index.html" \
                 $(PROP_ARGS)

client-from-site:
	$(MAKE) with-setup-flags NEXT_TARGET=plain-client-from-site

plain-client-from-site:
	make client $(FROM_SITE_ARGS)

# ------------------------------------------------------------
# Drive installer build across server and clients:

DRIVE_ARGS_q = $(RELEASE_MODE) $(VERSIONLESS_MODE) $(SOURCE_MODE) \
               $(CLEAN_MODE) $(SERVER_COMPILE_MACHINE) "$(CONFIG)" "$(CONFIG_MODE)" \
               $(SERVER) $(SERVER_PORT) "$(SERVER_HOSTS)" \
               "$(PKGS)" "$(DOC_SEARCH)" "$(DIST_NAME)" $(DIST_BASE) $(DIST_DIR)
DRIVE_DESCRIBE =
DRIVE_CMD_q = -l- distro-build/drive-clients $(DRIVE_DESCRIBE) $(DRIVE_ARGS_q)

# Full server build and clients drive, based on `CONFIG':
installers:
	rm -rf build/installers
	$(MAKE) server SERVE_DURING_CMD_qq='$(DRIVE_CMD_q)'

# Server is already built; start it and drive clients:
installers-from-built:
	$(MAKE) with-setup-flags NEXT_TARGET=plain-installers-from-built

plain-installers-from-built:
	$(MAKE) built-catalog-server SERVE_DURING_CMD_qq='$(DRIVE_CMD_q)'

# Just the clients, assuming server is already running:
drive-clients:
	$(PLAIN_RACKET) $(DRIVE_CMD_q)

describe-clients:
	$(MAKE) drive-clients DRIVE_DESCRIBE=--describe

# ------------------------------------------------------------
# Create installers, then assemble as a web site:

site:
	$(MAKE) installers
	$(MAKE) site-from-installers

DOC_CATALOGS = build/built/catalog build/native/catalog

site-from-installers:
	$(MAKE) with-setup-flags NEXT_TARGET=plain-site-from-installers

plain-site-from-installers:
	rm -rf build/docs
	rm -rf "build/zo`pwd`/build/docs"
	$(USER_RACKET) -l- distro-build/install-for-docs build/docs $(CONFIG_MODE_q) "$(PKGS)" $(DOC_CATALOGS)
	$(USER_RACKET) -l- distro-build/assemble-site $(CONFIG_MODE_q)

# ------------------------------------------------------------
# Create a snapshot site:

snapshot-site:
	$(MAKE) site
	$(MAKE) snapshot-at-site

snapshot-at-site:
	$(MAKE) with-setup-flags NEXT_TARGET=plain-snapshot-at-site

plain-snapshot-at-site:
	$(USER_RACKET) -l- distro-build/manage-snapshots $(CONFIG_MODE_q)
