# This makefile is meant to be converted to "Makefile" by
# "racket/src/makemake.rkt". See the end of this file for information
# on the `make` dialect that it uses.
#
# The targets here do not use dependencies (mostly), so it's a strange
# use of `make'. Really, this makefile is an alternative to a pile of
# scripts, where each target plays the role of a script. It's written
# as a makefile, because then the intersection of `make` and `nmake`
# acts as a kind of always-available scripting language.
#
# The main targets are
#
#  in-place = build in "racket" with all packages in development mode
#
#  as-is = like `in-place`, but don't download anything new
#
#  base = build in "collects" only (i.e., first step of `in-place`)
#
#    [`cs-` or `bc-` can prefix any of the above to select a Racket CS
#     or Racket BC build.]
#
#  server = build base, build packages listed in $(PKGS) or specified
#           via $(CONFIG), start server at port $(SERVER_PORT)
#
#  client = build base, create an installer with $(PKGS) with the help
#           of $(SERVER); result is recorded in "bundle/installer.txt"
#
#  installers = `server' plus `client' via $(CONFIG)
#
# Various configiration options can be provided as arguments to
# `make`. For example, `PKGS` can be supplied with `make PKGS="..."`.
# Not all variables in the makefile are intended as arguments,
# though.

# Packages (separated by spaces) to link in development mode or
# to include in a distribution:
PKGS = main-distribution main-distribution-test

main:
	$(MAKE) in-place

win:
	$(MAKE) win-in-place

# ------------------------------------------------------------
# In-place build

PLAIN_RACKET = racket/bin/racket
WIN32_PLAIN_RACKET = racket\racket

# For -M, etc., to pick the target machine for compilation:
SETUP_MACHINE_FLAGS =

# In case of cross-installation, point explicitly to local content:
RUN_RACKET == $(PLAIN_RACKET) $(SETUP_MACHINE_FLAGS) -G racket/etc -X racket/collects
RUN_RACO == $(RUN_RACKET) -N raco -l- raco

DEFAULT_SRC_CATALOG = https://pkgs.racket-lang.org

# Options passed along to any `raco pkg update` run:
PKG_UPDATE_OPTIONS = 

# Options passed along to any `raco setup` run:
PLT_SETUP_OPTIONS = 

# Makefile parallelism propoagated to `raco setup`:
CPUS =

# Target selector: `cs` or `bc`
VM = cs

# Target selector: `minimal` or `skip`
INITIAL_SETUP_MODE = minimal

# Target selector: `` or `-as-is`
AS_IS=

in-place:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-in-place ; \
         else $(MAKE) -j $(CPUS) plain-in-place JOB_OPTIONS="-j $(CPUS)" ; fi

# The `$(VM)-minimal-in-place` target should make `$(VM)-base` and then set
# `PLAIN_RACKET` while bouncing to `plain-minimal-in-place-after-base`.
# Similarly, `$(VM)-in-place-setup` should set `PLAIN_RACKET` and bounce
# to `plain-in-place-setup`.

plain-in-place:
	$(MAKE) base-config
	$(MAKE) $(VM)-$(INITIAL_SETUP_MODE)-in-place
	$(MAKE) $(VM)-in-place-setup

win-in-place:
	$(MAKE) win-base-config
	$(MAKE) win-$(VM)-$(INITIAL_SETUP_MODE)-in-place
	$(MAKE) win-$(VM)-in-place-setup

# Update before install to avoid needless work on the initial build,
# and use `--no-setup` plus an explicit `raco setup` for the same reason.
ALL_PKG_UPDATE_OPTIONS == --all --auto --no-setup --scope installation \
                          $(PKG_UPDATE_OPTIONS)
ALL_PKG_INSTALL_OPTIONS == $(JOB_OPTIONS) --no-setup --pkgs --skip-installed \
                           --scope installation --deps search-auto \
                           $(REQUIRED_PKGS) $(PKGS)
ALL_PLT_SETUP_OPTIONS == $(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)

# Allow `--error-out`, etc., for final setup step, so `make both` can
# continue from errors at that level
IN_PLACE_SETUP_OPTIONS =

plain-minimal-in-place-after-base:
	$(MAKE) pkgs-catalog
	$(RUN_RACO) pkg update $(ALL_PKG_UPDATE_OPTIONS)
	$(RUN_RACO) pkg install $(ALL_PKG_INSTALL_OPTIONS)
	$(RUN_RACO) setup --only-foreign-libs $(ALL_PLT_SETUP_OPTIONS)

plain-in-place-setup:
	$(RUN_RACO) setup $(ALL_PLT_SETUP_OPTIONS) $(IN_PLACE_SETUP_OPTIONS)

# Rebuild without consulting catalogs or package sources:

as-is:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-as-is AS_IS="-as-is" ; \
         else $(MAKE) -j $(CPUS) plain-as-is JOB_OPTIONS="-j $(CPUS)" AS_IS="-as-is" ; fi

plain-as-is:
	$(MAKE) plain-base
	$(MAKE) $(VM)-in-place-setup

win-as-is:
	$(MAKE) win-base
	$(MAKE) win-$(VM)-in-place-setup

# ------------------------------------------------------------
# Unix-style build (Unix and Mac OS, only)

PREFIX = 

CONFIG_PREFIX_ARGS == --prefix=$(PREFIX) --enable-macprefix
UNIXSTYLE_CONFIG_qq == MORE_CONFIGURE_ARGS="$(MORE_CONFIGURE_ARGS) $(CONFIG_PREFIX_ARGS)" CONFIG_IN_PLACE_ARGS=""
UNIX_CATALOG == build/local/catalog
UNIX_RACO_ARGS == $(JOB_OPTIONS) --catalog $(UNIX_CATALOG) --auto -i
UNIX_BASE_ARGS == SELF_ROOT_CONFIG_FLAG="-Z" SKIP_DESTDIR_FIX="skip"

unix-style:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-unix-style ; \
         else $(MAKE) -j $(CPUS) plain-unix-style JOB_OPTIONS="-j $(CPUS)" ; fi

plain-unix-style:
	if [ "$(PREFIX)" = "" ] ; then $(MAKE) error-need-prefix ; fi
	$(MAKE) $(VM)-base $(UNIXSTYLE_CONFIG_qq) $(UNIX_BASE_ARGS)
	$(MAKE) set-src-catalog
	$(MAKE) local-catalog
	"$(DESTDIR)$(PREFIX)/bin/raco" pkg install $(UNIX_RACO_ARGS) $(REQUIRED_PKGS) $(PKGS)
	cd racket/src/build && $(MAKE) fix-paths

error-need-prefix:
	: ================================================================
	: Please supply PREFIX="<dest-dir>" to set the install destination
	: ================================================================
	exit 1

LOC_CATALOG == build/local/pkgs-catalog

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

# Although `CONFIGURE_ARGS_qq` is intended as a configuration argument that should
# be propagated, we can't handle it's quoting in general, so it only really works
# for GNU Make:
CONFIGURE_ARGS_qq :=

# A variant of `CONFIGURE_ARGS` that propagates, but does not support quotes:
CONFIGURE_ARGS = 

# Rules in this makefile add configuration arguments here:
MORE_CONFIGURE_ARGS = 

# Arrange for `raco setup` to run at first without packages by
# pointing to a configuration in "build/etc"; the `-G` flag can be
# changed to `-Z` (by certain rules) to disable this redirection:
SELF_ROOT_CONFIG_FLAG = -G
SELF_FLAGS_qq == SELF_ROOT_CONFIG_FLAG="$(SELF_ROOT_CONFIG_FLAG)"
PLT_SETUP_OPTIONS_qq == PLT_SETUP_OPTIONS="$(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)"
INSTALL_SETUP_ARGS == $(SELF_FLAGS_qq) $(PLT_SETUP_OPTIONS_qq) SETUP_MACHINE_FLAGS="$(SETUP_MACHINE_FLAGS)"

WIN32_BUILD_LEVEL = all

base:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-base ; \
         else $(MAKE) -j $(CPUS) plain-base JOB_OPTIONS="-j $(CPUS)" ; fi

plain-base:
	$(MAKE) base-config
	$(MAKE) $(VM)-base

win-base:
	$(MAKE) win-base-config
	$(MAKE) win-$(VM)-base


base-config:
	mkdir -p build/config
	echo '#hash((links-search-files . ()))' > build/config/config.rktd

win-base-config:
	IF NOT EXIST build\config cmd /c mkdir build\config
	cmd /c echo #hash((links-search-files . ())) > build\config\config.rktd

# ------------------------------------------------------------
# Racket BC

# Can be `bc` or empty:
RACKETBC_SUFFIX = bc

bc:
	$(MAKE) bc-in-place

win-bc:
	$(MAKE) win-bc-in-place

bc-in-place:
	$(MAKE) in-place VM=bc

win-bc-in-place:
	$(MAKE) win-in-place VM=bc

bc-as-is:
	$(MAKE) as-is VM=bc

win-bc-as-is:
	$(MAKE) win-as-is VM=bc

bc-unix-style:
	$(MAKE) unix-style VM=bc

bc-minimal-in-place:
	$(MAKE) bc-base
	$(MAKE) plain-minimal-in-place-after-base PLAIN_RACKET=racket/bin/racket$(RACKETBC_SUFFIX)

win-bc-minimal-in-place:
	$(MAKE) win-bc-base
	$(MAKE) plain-minimal-in-place-after-base PLAIN_RACKET=racket\racket$(RACKETBC_SUFFIX)

bc-skip-in-place:
	$(MAKE) bc-base

win-bc-skip-in-place:
	$(MAKE) win-bc-base

bc-in-place-setup:
	$(MAKE) plain-in-place-setup PLAIN_RACKET=racket/bin/racket$(RACKETBC_SUFFIX)

win-bc-in-place-setup:
	$(MAKE) plain-in-place-setup PLAIN_RACKET=racket\racket$(RACKETBC_SUFFIX)

bc-base:
	if [ "$(RACKETBC_SUFFIX)" = "" ] ; \
	  then $(MAKE) bc-configure MORE_CONFIGURE_ARGS="$(MORE_CONFIGURE_ARGS) --enable-bcdefault" ; \
	  else $(MAKE) bc-configure MORE_CONFIGURE_ARGS="$(MORE_CONFIGURE_ARGS) --disable-bcdefault" ; fi
	cd racket/src/build/bc && $(MAKE) $(SELF_FLAGS_qq)
	cd racket/src/build && $(MAKE) install-bc $(INSTALL_SETUP_ARGS)

win-bc-base:
	$(MAKE) win-remove-setup-dlls
	cmd /c racket\src\worksp\build-at racket\src\worksp ..\..\..\build\config $(WIN32_BUILD_LEVEL) _$(RACKETBC_SUFFIX) $(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)

# Start by removing DLLs that may be loaded by `raco setup`
win-remove-setup-dlls:
	IF EXIST racket\lib\longdouble.dll cmd /c del racket\lib\longdouble.dll
	IF EXIST racket\lib\libiconv-2.dll cmd /c del racket\lib\libiconv-2.dll
	IF EXIST racket\lib\iconv.dll cmd /c del racket\lib\iconv.dll
	IF EXIST racket\lib\libeay32.dll cmd /c del racket\lib\libeay32.dll
	IF EXIST racket\lib\ssleay32.dll cmd /c del racket\lib\ssleay32.dll

CONFIG_IN_PLACE_ARGS = --disable-useprefix --enable-origtree
BC_CONFIGURE_ARGS == $(CONFIGURE_ARGS) $(MORE_CONFIGURE_ARGS) $(CONFIG_IN_PLACE_ARGS)

bc-configure:
	$(MAKE) racket/src/build/bc/Makefile
	cd racket/src/build/bc && $(MAKE) reconfigure MORE_CONFIGURE_ARGS="$(BC_CONFIGURE_ARGS)"

racket/src/build/bc/Makefile: racket/src/bc/configure racket/src/bc/Makefile.in racket/src/cfg-bc racket/src/Makefile.in 
	mkdir -p racket/src/build/bc
	cd racket/src/build/bc && ../../bc/configure $(CONFIGURE_ARGS_qq) $(BC_CONFIGURE_ARGS)
	cd racket/src/build && ../cfg-bc $(CONFIGURE_ARGS_qq) $(CS_CONFIGURE_ARGS)

MORE_CROSS_CONFIGURE_ARGS =

# For cross-compilation, build a native executable with no configure options:
native-bc-for-cross:
	mkdir -p racket/src/build/cross/bc
	$(MAKE) racket/src/build/cross/bc/Makefile
	cd racket/src/build/cross/bc && $(MAKE) reconfigure MORE_CONFIGURE_ARGS="--enable-bcdefault $(MORE_CROSS_CONFIGURE_ARGS)"
	cd racket/src/build/cross/bc && $(MAKE)

racket/src/build/cross/bc/Makefile: racket/src/bc/configure racket/src/cfg-bc racket/src/bc/Makefile.in
	cd racket/src/build/cross/bc && ../../../bc/configure --enable-bcdefault $(MORE_CROSS_CONFIGURE_ARGS)

# Temporary compatibility for distro-build:
native-for-cross:
	$(MAKE) native-bc-for-cross

# ------------------------------------------------------------
# Racket CS

# Can be `cs` or empty:
RACKETCS_SUFFIX =

# If `RACKET` and `RACKET_FOR_BOOTFILES` are not set, the build uses the
# `pb` repo to get initial portable-byte Chez Scheme boot files.
# If `RACKET` is set, then it is always run in preference to the
# built Racket, so it's useful for cross-compilation, and it needs
# to be essentially the same version and variant as being built.
# If only `RACKET_FOR_BOOTFILES` is set, then it doesn't have to be so
# similar to the one being built, because it's used only to create
# initial Chez Scheme boot files.
RACKET :=
RACKET_FOR_BOOTFILES = $(RACKET)

# Propoagate `RACKET_FOR_BUILD` instead of `RACKET` to avoid conflicting
# with makefiles in subdirectories:
RACKET_FOR_BUILD = $(RACKET)

# This branch name changes each time the pb boot files are updated:
PB_BRANCH == circa-8.0.0.3-1
PB_REPO = https://github.com/racket/pb

# Alternative source for Chez Scheme boot files, normally set by
# the distro-build client driver
EXTRA_REPOS_BASE =

# Target selector: `-cross` or ``
CS_CROSS_SUFFIX =

# Points a cross build at a directory containing a host build; this path
# can be relative to the cross build directory, but it needs to end in
# a separator if non-empty
CS_HOST_WORKAREA_PREFIX = 

cs:
	$(MAKE) cs-in-place

win-cs:
	$(MAKE) win-cs-in-place

cs-in-place:
	$(MAKE) in-place VM=cs

win-cs-in-place:
	$(MAKE) win-in-place VM=cs

cs-as-is:
	$(MAKE) as-is VM=cs

win-cs-as-is:
	$(MAKE) win-as-is VM=cs

cs-unix-style:
	$(MAKE) unix-style VM=cs

cs-minimal-in-place:
	$(MAKE) cs-base
	$(MAKE) cs-minimal-in-place-after-base$(CS_CROSS_SUFFIX)

win-cs-minimal-in-place:
	$(MAKE) win-cs-base
	$(MAKE) plain-minimal-in-place-after-base PLAIN_RACKET=racket\racket$(RACKETCS_SUFFIX)

cs-skip-in-place:
	$(MAKE) cs-base

win-cs-skip-in-place:
	$(MAKE) win-cs-base

cs-in-place-setup:
	$(MAKE) plain-in-place-setup PLAIN_RACKET=racket/bin/racket$(RACKETCS_SUFFIX)

win-cs-in-place-setup:
	$(MAKE) plain-in-place-setup PLAIN_RACKET=racket\racket$(RACKETCS_SUFFIX)

cs-base:
	$(MAKE) maybe-fetch-pb$(AS_IS)
	if [ "$(RACKETCS_SUFFIX)" = "" ] ; \
	  then $(MAKE) cs-configure MORE_CONFIGURE_ARGS="$(MORE_CONFIGURE_ARGS) --enable-csdefault" ; \
	  else $(MAKE) cs-configure MORE_CONFIGURE_ARGS="$(MORE_CONFIGURE_ARGS) --disable-csdefault" ; fi
	cd racket/src/build/cs/c && $(MAKE) CS_HOST_WORKAREA_PREFIX="$(CS_HOST_WORKAREA_PREFIX)"
	$(MAKE) base-config
	cd racket/src/build && $(MAKE) install-cs $(INSTALL_SETUP_ARGS)

CS_CONFIGURE_ARGS == $(CONFIGURE_ARGS) $(MORE_CONFIGURE_ARGS) $(CONFIG_IN_PLACE_ARGS)

cs-configure:
	$(MAKE) racket/src/build/cs/c/Makefile
	cd racket/src/build/cs/c && $(MAKE) reconfigure MORE_CONFIGURE_ARGS="$(CS_CONFIGURE_ARGS)"

racket/src/build/cs/c/Makefile: racket/src/cs/c/configure racket/src/cs/c/Makefile.in racket/src/cfg-cs racket/src/Makefile.in 
	mkdir -p racket/src/build/cs/c
	cd racket/src/build/cs/c && ../../../cs/c/configure $(CONFIGURE_ARGS_qq) $(CS_CONFIGURE_ARGS)
	cd racket/src/build && ../cfg-cs $(CONFIGURE_ARGS_qq) $(CS_CONFIGURE_ARGS)

cs-minimal-in-place-after-base:
	$(MAKE) plain-minimal-in-place-after-base PLAIN_RACKET=racket/bin/racket$(RACKETCS_SUFFIX)

cs-minimal-in-place-after-base-cross:
	$(MAKE) plain-minimal-in-place-after-base PLAIN_RACKET="$(RACKET_FOR_BUILD)" PLT_SETUP_OPTIONS="--no-pkg-deps $(PLT_SETUP_OPTIONS)"

fetch-pb:
	if [ "$(EXTRA_REPOS_BASE)" = "" ] ; \
          then $(MAKE) fetch-pb-from ; \
          else $(MAKE) fetch-pb-from PB_REPO="$(EXTRA_REPOS_BASE)pb/.git" ; fi

maybe-fetch-pb:
	if [ "$(RACKET_FOR_BOOTFILES)" = "" ] ; \
          then $(MAKE) fetch-pb ; fi

maybe-fetch-pb-as-is:
	echo done

PB_DIR == racket/src/ChezScheme/boot/pb

fetch-pb-from:
	mkdir -p racket/src/ChezScheme/boot
	if [ ! -d racket/src/ChezScheme/boot/pb ] ; \
	  then git clone -q -b $(PB_BRANCH) $(PB_REPO) $(PB_DIR) ; \
	  else cd $(PB_DIR) && git fetch -q origin $(PB_BRANCH):remotes/origin/$(PB_BRANCH) ; fi
	cd $(PB_DIR) && git checkout -q $(PB_BRANCH)

pb-fetch:
	$(MAKE) fetch-pb

# Helpers for managing the "pb" repo:
#  * `make pb-build` to rebuild pb boot files
#  * `make pb-stage` after updating `PB_BRANCH`
#  * `make pb-push` to upload the branch after checking that
#    the staged branch looks right
# If you don't have push access to `PB_REPO`, you may need to
# change the origin of your "pb" checkout.
pb-build:
	cd racket/src/ChezScheme && racket rktboot/main.rkt --machine pb

pb-stage:
	cd $(PB_DIR) && git branch $(PB_BRANCH)
	cd $(PB_DIR) && git checkout $(PB_BRANCH)
	cd $(PB_DIR) && git add . && git commit --amend -m "new build"

pb-push:
	cd $(PB_DIR) && git push -u origin $(PB_BRANCH)

WIN32_BOOT_ARGS == SETUP_BOOT_MODE=--boot WIN32_BUILD_LEVEL=bc PLAIN_RACKET=racket\racketbc

win-cs-base:
	IF "$(RACKET_FOR_BUILD)" == "" $(MAKE) win-bc-then-cs-base $(WIN32_BOOT_ARGS)
	IF not "$(RACKET_FOR_BUILD)" == "" $(MAKE) win-just-cs-base SETUP_BOOT_MODE=--chain

WIN32_SETUP_BOOT == -O "info@compiler/cm" \
                    -l- setup $(SETUP_BOOT_MODE) racket/src/setup-go.rkt racket/src/build/compiled \
                    ignored racket/src/build/ignored.d

win-bc-then-cs-base:
	$(MAKE) win-bc-base
	$(MAKE) win-just-cs-base RACKET_FOR_BUILD="$(PLAIN_RACKET)"

CSBUILD_ARGUMENTS == --pull \
                     --racketcs-suffix "$(RACKETCS_SUFFIX)" $(DISABLE_STATIC_LIBS) \
                     --boot-mode "$(SETUP_BOOT_MODE)" \
                     --extra-repos-base "$(EXTRA_REPOS_BASE)"

win-just-cs-base:
	IF NOT EXIST racket\src\build cmd /c mkdir racket\src\build
	cmd /c $(RACKET_FOR_BUILD) $(WIN32_SETUP_BOOT) racket\src\worksp\csbuild.rkt $(CSBUILD_ARGUMENTS)
	IF NOT EXIST build\config cmd /c mkdir build\config
	cmd /c echo #hash((links-search-files . ())) > build\config\config.rktd
	racket\racket$(RACKETCS_SUFFIX) -G build\config -N raco -l- raco setup $(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)

# For cross-compilation, build a native executable with no configure options:
native-cs-for-cross:
	$(MAKE) maybe-fetch-pb
	mkdir -p racket/src/build/cross/cs/c
	$(MAKE) racket/src/build/cross/cs/c/Makefile
	cd racket/src/build/cross/cs/c && $(MAKE) reconfigure MORE_CONFIGURE_ARGS="--enable-csdefault $(MORE_CROSS_CONFIGURE_ARGS)"
	cd racket/src/build/cross/cs/c && $(MAKE)

racket/src/build/cross/cs/c/Makefile: racket/src/cs/c/configure racket/src/cs/c/Makefile.in
	cd racket/src/build/cross/cs/c; ../../../../cs/c/configure --enable-csdefault $(MORE_CROSS_CONFIGURE_ARGS)

# ------------------------------------------------------------
# Both traditional Racket and RacketCS
# ... but update packages and builds docs only once

both:
	$(MAKE) cs IN_PLACE_SETUP_OPTIONS="--error-out build/step"
	$(MAKE) also-bc IN_PLACE_SETUP_OPTIONS="--error-in build/step"

plain-also:
	$(MAKE) $(VM) INITIAL_SETUP_MODE=skip PLT_SETUP_OPTIONS="-D $(PLT_SETUP_OPTIONS)"

also-cs:
	$(MAKE) plain-also VM=cs

also-bc:
	$(MAKE) plain-also VM=bc


win-both:
	$(MAKE) win-cs
	$(MAKE) win-also-bc

win-plain-also:
	$(MAKE) win-$(VM) INITIAL_SETUP_MODE=skip PLT_SETUP_OPTIONS="-D $(PLT_SETUP_OPTIONS)"

win-also-cs:
	$(MAKE) win-plain-also VM=cs

win-also-bc:
	$(MAKE) win-plain-also VM=bc

# ------------------------------------------------------------
# Clean (which just gives advice)

clean:
	@echo "No makefile support for cleaning. Instead, try"
	@echo "  git clean -d -x -f ."
	@exit 1

# ------------------------------------------------------------
# Configuration options for building installers

# On variable definitions: Spaces are allowed where noted and
# disallowed otherwise. If a variable name ends in "_q", then it means
# that the variable can expand to include double-quote marks. If a
# variable's name ends in "_qq", then it expands to a combination of
# single-quote and double-quote marks. If a variable's name does not
# end in "_q" or "_qq", don't use any quote marks on the right-hand
# side of its definition.

# Catalog for package sources:
SRC_CATALOG = $(DEFAULT_SRC_CATALOG)

# A URL embedded in documentation for remote searches, where a Racket
# version and search key are added as query fields to the URL, and ""
# is replaced by default:
DOC_SEARCH = 

# Server for built packages (i.e., the host where you'll run the
# server):
SERVER = localhost
SERVER_PORT = 9440
SERVER_URL_SCHEME = http

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

# Comma-separated options for the `--packed-options` argument to
# `distro-build/installer`, which generalizes simple switches like
# `--mac-pkg` and `--tgz`; we don't just take a sequence of regular
# command-line switches here, because it's difficult to thread those
# through `make` variants like `nmake`:
INSTALLER_OPTIONS =

# Set to "--source --no-setup" to include packages in an installer
# (or archive) only in source form:
PKG_SOURCE_MODE =

# Set to "--disable-lib" to avoid including ".a" and ".boot" files
# for use in embedding Racket in other applications
DISABLE_STATIC_LIBS =

# Set to a base64-encoded list of strings for an executable and
# arguments to run on an assembled directory (on the client machine)
# before it is packaged into an installer, or empty for no pre-process
# action:
INSTALLER_PRE_PROCESS_BASE64 =

# Set to a base64-encoded list of strings for an executable and
# arguments to run on an installer (on the client machine) before the
# installer is uploaded, or empty for no post-process action:
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
DIST_CATALOGS_q := ""

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

# For Mac OS, set to a notarization configuration as a base64-encoded
# hash table <config> in `--notarization-config <config>`, where the
# distro-build documentation for `#:notarization-config` describes the
# keys and values:
NOTARIZATION_CONFIG =

# For Windows, `osslsigncode' arguments other than `-n', `-t', `-in',
# and `-out' as a Base64-encoded, S-expression, list of strings:
OSSLSIGNCODE_ARGS_BASE64 =

# URL for a README file to include in an installer (empty for none,
# spaces allowed):
README = $(SERVER_URL_SCHEME)://$(SVR_PRT)/README.txt

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
SERVE_DURING_CMD_qq :=

# ------------------------------------------------------------
# Helpers

# Needed for any distribution:
REQUIRED_PKGS := racket-lib

# Packages needed for building distribution:
DISTRO_BUILD_PKGS := distro-build-lib

SVR_PRT = $(SERVER):$(SERVER_PORT)

SVR_CAT = $(SERVER_URL_SCHEME)://$(SVR_PRT)/$(SERVER_CATALOG_PATH)

# To configure package installations on the server:
SERVER_PKG_INSTALL_OPTIONS =

# To configure package installations for the installer:
PKG_INSTALL_OPTIONS =

# Catch problems due to malformed distribution-build packages
RECOMPILE_OPTIONS == --recompile-only

# Helper macros:
USER_CONFIG == -G build/user/config -X racket/collects -A build/user $(SETUP_MACHINE_FLAGS)
USER_RACKET == $(PLAIN_RACKET) $(USER_CONFIG)
USER_RACO == $(PLAIN_RACKET) $(USER_CONFIG) -N raco -l- raco
WIN32_RACKET == $(WIN32_PLAIN_RACKET) $(USER_CONFIG)
WIN32_RACO == $(WIN32_PLAIN_RACKET) $(USER_CONFIG) -N raco -l- raco
X_AUTO_OPTIONS == --skip-installed --deps search-auto --pkgs $(JOB_OPTIONS)
USER_AUTO_OPTIONS == --scope user $(X_AUTO_OPTIONS)
SOURCE_USER_AUTO_q == --catalog build/catalog-copy $(USER_AUTO_OPTIONS) $(SERVER_PKG_INSTALL_OPTIONS)
REMOTE_USER_AUTO == --catalog $(SVR_CAT) $(USER_AUTO_OPTIONS)
REMOTE_INST_AUTO == --catalog $(SVR_CAT) --scope installation $(X_AUTO_OPTIONS) $(PKG_INSTALL_OPTIONS) $(RECOMPILE_OPTIONS)
CONFIG_MODE_q == "$(CONFIG)" "$(CONFIG_MODE)"
BUNDLE_CONFIG == bundle/racket/etc/config.rktd
BUNDLE_RACO_FLAGS == -G bundle/racket/etc -X bundle/racket/collects -C -A bundle/user -l raco
BUNDLE_RACO == $(PLAIN_RACKET) $(BUNDLE_RACO_FLAGS)
WIN32_BUNDLE_RACO == $(WIN32_PLAIN_RACKET) $(BUNDLE_RACO_FLAGS)
IN_BUNDLE_RACO = bundle/racket/bin/raco
WIN32_IN_BUNDLE_RACO == bundle\racket\raco

# ------------------------------------------------------------
# Linking all packages (development mode; not an installer build)

PKGS_CATALOG == -U -G build/config -l- pkg/dirs-catalog --link --check-metadata --immediate
PKGS_CONFIG == -U -G build/config racket/src/pkgs-config.rkt

pkgs-catalog:
	$(RUN_RACKET) $(PKGS_CATALOG) racket/share/pkgs-catalog pkgs racket/src/expander
	$(RUN_RACKET) $(PKGS_CONFIG) "$(DEFAULT_SRC_CATALOG)" "$(SRC_CATALOG)"
	$(RUN_RACKET) racket/src/pkgs-check.rkt racket/share/pkgs-catalog

# ------------------------------------------------------------
# Handle `SERVER_COMPILE_MACHINE` for various targets

SERVER_COMPILE_MACHINE = -M
ANY_COMPILE_MACHINE_ARGS_qq == SETUP_MACHINE_FLAGS="-MCR `pwd`/build/zo:" \
                               MORE_CONFIGURE_ARGS="$(MORE_CONFIGURE_ARGS) --enable-crossany"

# Target selector: `plain-server`, `plain-client-from-site`, `plain-installers-from-built`, \
#                  `plain-site-from-installers`, or `plain-snapshot-at-site`
NEXT_TARGET = 

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
	$(MAKE) plain-base
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
# versions of the packages from the installation into "build/user";
# set `PACK_BUILT_OPTIONS` `--mode <mode>` to force all packages to
# a specific mode, but the default infers `built` or `binary`
PACK_BUILT_OPTIONS =
built-catalog:
	$(USER_RACKET) -l- distro-build/pack-built $(PACK_BUILT_OPTIONS) build/pkgs.rktd

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
# The `client' and `win-client' targets are also used by
# `distro-build/drive-clients', which is in turn run by the
# `installers' target.
#
# For a non-Windows machine, if "build/log" exists, then
# keep the "build/user" directory on the grounds that the
# client is the same as the server.

# Target selector: `base` or `cs-base` or `bc-base`
CLIENT_BASE = base

# Target selector: `win-base` or `win-cs-base` or `win-bc-base`
WIN32_CLIENT_BASE = win-base

SET_BUNDLE_CONFIG_q == $(BUNDLE_CONFIG) "$(INSTALL_NAME)" "$(BUILD_STAMP)" "$(DOC_SEARCH)" $(DIST_CATALOGS_q)

# Target selector: `bundle-from-server` or `bundle-cross-from-server`
BUNDLE_FROM_SERVER_TARGET = bundle-from-server

client:
	if [ ! -d build/log ] ; then rm -rf build/user ; fi
	$(MAKE) $(CLIENT_BASE) MORE_CONFIGURE_ARGS="$(MORE_CONFIGURE_ARGS) $(DISABLE_STATIC_LIBS)"
	$(MAKE) distro-build-from-server
	$(MAKE) $(BUNDLE_FROM_SERVER_TARGET)
	$(USER_RACKET) -l distro-build/set-config $(SET_BUNDLE_CONFIG_q)
	$(MAKE) installer-from-bundle

win-client:
	IF EXIST build\user cmd /c del /f /s /q build\user
	$(MAKE) $(WIN32_CLIENT_BASE)
	$(MAKE) win-distro-build-from-server
	$(MAKE) win-bundle-from-server
	$(WIN32_RACKET) -l distro-build/set-config $(SET_BUNDLE_CONFIG_q)
	$(MAKE) win-installer-from-bundle

# Sensible when creating a source distribution with built packages:
client-compile-any:
	$(MAKE) client $(ANY_COMPILE_MACHINE_ARGS_qq) BUNDLE_FROM_SERVER_TARGET=bundle-cross-from-server

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
	$(USER_RACKET) -l- distro-build/unpack-collects $(UNPACK_COLLECTS_FLAGS) $(SERVER_URL_SCHEME)://$(SVR_PRT)/$(SERVER_COLLECTS_PATH)
	$(IN_BUNDLE_RACO) setup --no-user $(JOB_OPTIONS) $(RECOMPILE_OPTIONS)
	$(IN_BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(REQUIRED_PKGS)
	$(IN_BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(PKGS)
	$(USER_RACKET) -l setup/unixstyle-install post-adjust "$(SOURCE_MODE)" "$(PKG_SOURCE_MODE)" racket bundle/racket

# For a cross build, we still need to use `$(BUNDLE_RACO)` for
# installing packages. The host build must have all native libraries
# that installation will need.
bundle-cross-from-server:
	rm -rf "build/zo`pwd`/bundle"
	$(MAKE) bundle-from-server $(COPY_ARGS) IN_BUNDLE_RACO="$(PLAIN_RACKET) $(SETUP_MACHINE_FLAGS) $(BUNDLE_RACO_FLAGS)"

UPLOAD_q == --readme "$(README)" --upload "$(UPLOAD)" --desc "$(DIST_DESC)"
DIST_ARGS_q == $(UPLOAD_q) $(RELEASE_MODE) $(SOURCE_MODE) $(VERSIONLESS_MODE) \
               $(MAC_PKG_MODE) $(TGZ_MODE) --packed-options "$(INSTALLER_OPTIONS)" \
               --pre-process "$(INSTALLER_PRE_PROCESS_BASE64)" \
               --post-process "$(INSTALLER_POST_PROCESS_BASE64)" \
               $(NOTARIZATION_CONFIG) \
               "$(DIST_NAME)" $(DIST_BASE) $(DIST_DIR) "$(DIST_SUFFIX)" \
               "$(SIGN_IDENTITY)" "$(OSSLSIGNCODE_ARGS_BASE64)"

# Create an installer from the build (with installed packages) that's
# in "bundle/racket":
installer-from-bundle:
	$(USER_RACKET) -l- distro-build/installer $(DIST_ARGS_q)

win-distro-build-from-server:
	$(WIN32_RACO) pkg install $(REMOTE_USER_AUTO) distro-build-client

win-bundle:
	IF EXIST bundle cmd /c rmdir /S /Q bundle
	cmd /c mkdir bundle\racket
	$(WIN32_RACKET) -l setup/unixstyle-install bundle$(SOURCE_MODE) racket bundle\racket
	$(WIN32_RACKET) -l setup/winstrip bundle\racket
	$(WIN32_RACKET) -l setup/winvers-change bundle\racket

win-bundle-from-server:
	$(MAKE) win-bundle
	$(WIN32_RACKET) -l- distro-build/unpack-collects $(UNPACK_COLLECTS_FLAGS) $(SERVER_URL_SCHEME)://$(SVR_PRT)/$(SERVER_COLLECTS_PATH)
	$(WIN32_IN_BUNDLE_RACO) setup --no-user -l racket/base
	$(WIN32_IN_BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(REQUIRED_PKGS)
	$(WIN32_IN_BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(PKGS)

win-installer-from-bundle:
	$(WIN32_RACKET) -l- distro-build/installer $(DIST_ARGS_q)

# The `test-client` and `win-test-client` targets are optional test
# step for an installer build, were `TEST_PKGS` names extra packages
# to install, and `TEST_ARGS_q` is a set of arguments to `raco test`.
# This step will not make sense for some kinds of builds, such as
# source builds or cross-platform builds.

test-client:
	$(BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(TEST_PKGS)
	$(IN_BUNDLE_RACO) test $(TEST_ARGS_q)

win-test-client:
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

FROM_SITE_ARGS == SERVER_CATALOG_PATH=$(SITE_PATH)catalog/ SERVER_COLLECTS_PATH=$(SITE_PATH)origin/ \
                  DIST_CATALOGS_q='$(SERVER_URL_SCHEME)://$(SERVER):$(SERVER_PORT)/$(SITE_PATH)catalog/ ""' \
                  DOC_SEARCH="$(SERVER_URL_SCHEME)://$(SERVER):$(SERVER_PORT)/$(SITE_PATH)doc/local-redirect/index.html" \
                  $(PROP_ARGS)

client-from-site:
	$(MAKE) with-setup-flags NEXT_TARGET=plain-client-from-site

plain-client-from-site:
	make client $(FROM_SITE_ARGS)

# ------------------------------------------------------------
# Drive installer build across server and clients:

DRIVE_ARGS_q == $(RELEASE_MODE) $(VERSIONLESS_MODE) $(SOURCE_MODE) \
                $(CLEAN_MODE) $(SERVER_COMPILE_MACHINE) "$(CONFIG)" "$(CONFIG_MODE)" \
                $(SERVER) $(SERVER_PORT) "$(SERVER_HOSTS)" \
                "$(PKGS)" "$(DOC_SEARCH)" "$(DIST_NAME)" $(DIST_BASE) $(DIST_DIR)
DRIVE_DESCRIBE =
DRIVE_CMD_q == -l- distro-build/drive-clients $(DRIVE_DESCRIBE) $(DRIVE_ARGS_q)

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
	$(USER_RACKET) -l- distro-build/assemble-site $(CONFIG_MODE_q) "$(DIST_BASE)"

# ------------------------------------------------------------
# Create a snapshot site:

snapshot-site:
	$(MAKE) site
	$(MAKE) snapshot-at-site

snapshot-at-site:
	$(MAKE) with-setup-flags NEXT_TARGET=plain-snapshot-at-site

plain-snapshot-at-site:
	$(USER_RACKET) -l- distro-build/manage-snapshots $(CONFIG_MODE_q) "$(DIST_BASE)"

# ------------------------------------------------------------
# Compatibility targets

win32-in-place:
	$(MAKE) win-in-place

win32-as-is:
	$(MAKE) win-as-is

win32-base:
	$(MAKE) win-base

win32-cs:
	$(MAKE) win-cs

win32-cs-in-place:
	$(MAKE) win-cs-in-place

win32-cs-as-is:
	$(MAKE) win-cs-as-is

win32-cs-base:
	$(MAKE) win-cs-base

win32-both:
	$(MAKE) win-both

win32-also-cs:
	$(MAKE) win-also-cs

win32-client:
	$(MAKE) win-client

win32-distro-build-from-server:
	$(MAKE) win-distro-build-from-server

win32-bundle:
	$(MAKE) win-bundle

win32-bundle-from-server:
	$(MAKE) win-bundle-from-server

win32-installer-from-bundle:
	$(MAKE) win-installer-from-bundle

win32-test-client:
	$(MAKE) win-test-client

# ------------------------------------------------------------
# Remake "Makefile"

makemake: .makefile racket/src/makemake.rkt
	racket racket/src/makemake.rkt .makefile > Makefile

# ------------------------------------------------------------
# About the `make` dialect of this file
#
# Roughly, write this makefile as if you are using POSIX `make`,
# except that variable values are propagated to recursive `$(MAKE)`
# requests as in GNU Make. The "makemake.rkt" compiler constructs a
# call graph and changes each `$(MAKE)` call to add any variables that
# should be propagated as explicit arguments, which means that it
# works for all `make` implementations and even `nmake` on Windows.
#
# Some variables are "higher order" in the sense that they are used to
# construct a target of a recursive `$(MAKE)`. The declaration of each
# such variable must be preceded by a "Target selector" declaration
# that enumerates the variable's possible values, as in
#
#   # Target selector: `minimal` or `skip`
#
# That way, the call-graph construction can compute all of the
# potential targets of the recursive `$(MAKE)`.
#
# When a variable is declared with `==` instead of `=`, then it is not
# treated as a variable that must be propagated, but instead as a
# macro to be eagerly expanded. When a variable is defined with `:=`,
# then it is neither propagated nor expanded as a macro. Note that
# variables are propagated with the pattern VAR="$(VAR)", so it can
# work for variables with spaces, but not for variables with quotes.
#
# If a `$(MAKE)` is preceeded by `cd <dir> &&`, then it is not treated
# as a recursive `$(MAKE)` invocation in the sense of adding variables
# to propagate. Avoid variable names for propagated variables that
# might be the same as names in makefiles in other directories where
# the intent is *not* to propagate to those other makefiles, since GNU
# Make will continue to propagate any variable after it's propagated
# once.
