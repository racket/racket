# This makefile defines default values for various build modes and
# provides targets that mostly just dispatch to "main.zuo". It can
# be used either with `make` or `nmake`.
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
# Various configuration options can be provided as arguments to
# `make`. For example, `PKGS` can be supplied with `make PKGS="..."`.
# Not all variables in the makefile are intended as arguments,
# though.
#
# Instead of using `make`, you can use `zuo` with the same targets
# and variable options.

# ------------------------------------------------------------
# Zuo macros (with `make` versus `nmake` magic)

# Most `make` variants will continue the end of this comment
# to the next line, due to the final backslash, but `nmake` won't.
# Also, OpenBSD's `make` won't, so see the end of this file  \
ZUO = racket\src\build\zuo.exe

# Get `nmake` to skip this part \
!if 0

ZUO = racket/src/build/bin/zuo
PLUS_MODIFIER = +

#\
!endif

RUN_ZUO = $(PLUS_MODIFIER) $(ZUO) .

# ------------------------------------------------------------
# Base, in-place, and Unix-style builds

# Implementation selector: `cs` or `bc`
VM = cs

# Makefile parallelism propagated to `raco setup`:
JOBS =
# Backward compatibility:
CPUS =
# Parallelism from `make -j` with GNU make might also be detected

# Arguments to include in a call to `configure`
CONFIGURE_ARGS =

# Like `CONFIGURE_ARGS`, but for historical reasons, "`pwd`" is
# replaced with the build subdirectory where `configure` is run
CONFIGURE_ARGS_qq =

# ------------------------------------------------------------
# Cross-build support

# Cross-build mode: `-cross` or ``
CS_CROSS_SUFFIX =

# Racket to use for building `RACKET`, which must be the same
# version and virtual machine as being built:
RACKET = 

# Old name for `RACKET`:
PLAIN_RACKET = 

# For CS, if `RACKET` and `BOOTFILE_RACKET` are not set, the build
# uses the `pb` repo to get initial portable-byte Chez Scheme boot
# files; if `RACKET` is set, then it is always run in preference to
# the built Racket, so it's useful for cross-compilation, and it needs
# to be essentially the same version and variant as being built; if
# only `BOOTFILE_RACKET` is set, then it doesn't have to be so similar
# to the one being built, because it's used only to create initial
# Chez Scheme boot files
BOOTFILE_RACKET =

# For CS, `SCHEME` can be set to a Chez Scheme (v9.5.3 and up)
# executable that runs on the build platform; if set, this will be used
# to create the Chez Scheme boot files (for both cross and non-cross
# builds); this is a much more direct path than supplying `RACKET`; it
# does not need to match the Chez Scheme version as used in the Racket
# being built; a "reboot" bootstrapping path is able to reconstruct
# boot files across versions.
SCHEME =

# For CS, points a cross build at a directory containing a host build;
# this path can be relative to the cross build directory
CS_HOST_WORKAREA_PREFIX =

# For building Zuo:
CC_FOR_BUILD = $(CC) -O2
CFLAGS_FOR_BUILD =

# ------------------------------------------------------------
# Racket CS boot files

# This branch name must be changed each time the pb boot files are
# updated:
PB_BRANCH = v9.9.9-pre-release.17-1
PB_REPO = https://github.com/racket/pb

# Set to empty for Git before v1.7.10:
SINGLE_BRANCH_FLAG = --single-branch

# Alternative source for Chez Scheme boot files, normally set by
# the distro-build client driver
EXTRA_REPOS_BASE =

# ------------------------------------------------------------
# Package update and setup options

# Packages (separated by spaces) to link in development mode or
# to include in a distribution:
PKGS = main-distribution main-distribution-test

# Needed for any distribution (not meant to be configured):
REQUIRED_PKGS = racket-lib

# Needed for distro-build (not meant to be configured):
DISTRO_BUILD_PKGS = distro-build-lib

# Options passed along to any `raco pkg update` run:
PKG_UPDATE_OPTIONS =

# Options passed along to any `raco setup` run:
PLT_SETUP_OPTIONS =

# Catalog for package sources:
SRC_CATALOG = https://pkgs.racket-lang.org

# Built-in catalog for package sources (not meant to be configured):
DEFAULT_SRC_CATALOG = https://pkgs.racket-lang.org

# For -M, etc., to pick the target machine for compilation:
SETUP_MACHINE_FLAGS =

# Set to "-M" as a shorthand for
#  SETUP_MACHINE_FLAGS="-MCR `pwd`build/zo:"
#  CONFIGURE_ARGS="--enable-crossany"
# for `server` and derived targets
SERVER_COMPILE_MACHINE = -M

# ------------------------------------------------------------
# Installation options

# For Unix-style builds, where to install:
PREFIX =

# For Unix-style builds, a directory where an installation
# that's destined for `PREFIX` is staged (to be moved later):
DESTDIR =

# Can be "bc" or empty:
RACKETBC_SUFFIX = bc

# Can be "cs" or empty:
RACKETCS_SUFFIX =

# ------------------------------------------------------------
# Build targets

# Using `$(MAKE)` instead of `"$(MAKE)"` to work with Windows and NMAKE
BUILD_VARS = MAKE=$(MAKE) \
             VM="$(VM)" \
             JOBS="$(JOBS)" \
             CPUS="$(CPUS)" \
             CONFIGURE_ARGS_qq='$(CONFIGURE_ARGS_qq)' \
             CONFIGURE_ARGS="$(CONFIGURE_ARGS)" \
             CS_CROSS_SUFFIX="$(CS_CROSS_SUFFIX)" \
             RACKET="$(RACKET)" \
             PLAIN_RACKET="$(PLAIN_RACKET)" \
             BOOTFILE_RACKET="$(BOOTFILE_RACKET)" \
             SCHEME="$(SCHEME)" \
             CS_HOST_WORKAREA_PREFIX="$(CS_HOST_WORKAREA_PREFIX)" \
             PB_BRANCH="$(PB_BRANCH)" \
             PB_REPO="$(PB_REPO)" \
             SINGLE_BRANCH_FLAG="$(SINGLE_BRANCH_FLAG)" \
             EXTRA_REPOS_BASE="$(EXTRA_REPOS_BASE)" \
             PKGS="$(PKGS)" \
             PKG_UPDATE_OPTIONS="$(PKG_UPDATE_OPTIONS)" \
             PLT_SETUP_OPTIONS="$(PLT_SETUP_OPTIONS)" \
             SRC_CATALOG="$(SRC_CATALOG)" \
             SETUP_MACHINE_FLAGS="$(SETUP_MACHINE_FLAGS)" \
             PREFIX="$(PREFIX)" \
             RACKETBC_SUFFIX="$(RACKETBC_SUFFIX)" \
             RACKETCS_SUFFIX="$(RACKETCS_SUFFIX)" \
             DESTDIR="$(DESTDIR)"

in-place: $(ZUO)
	$(RUN_ZUO) in-place $(BUILD_VARS)

as-is: $(ZUO)
	$(RUN_ZUO) as-is $(BUILD_VARS)

unix-style: $(ZUO)
	$(RUN_ZUO) unix-style $(BUILD_VARS)

base: $(ZUO)
	$(RUN_ZUO) base $(BUILD_VARS)

cs: $(ZUO)
	$(RUN_ZUO) in-place $(BUILD_VARS) VM=cs

bc: $(ZUO)
	$(RUN_ZUO) in-place $(BUILD_VARS) VM=bc

both: $(ZUO)
	$(RUN_ZUO) both $(BUILD_VARS)

# Build of CS without pulling new packages or building docs
also-cs: $(ZUO)
	$(RUN_ZUO) also-in-place $(BUILD_VARS) VM=cs

# Build of BC without pulling new packages or building docs
also-bc: $(ZUO)
	$(RUN_ZUO) also-in-place $(BUILD_VARS) VM=bc

# For cross-compilation, build a native executable with no configure options:
native-cs-for-cross: $(ZUO)
	$(RUN_ZUO) native-cs-for-cross $(BUILD_VARS)
native-bc-for-cross: $(ZUO)
	$(RUN_ZUO) native-bc-for-cross $(BUILD_VARS)

local-catalog: $(ZUO)
	$(RUN_ZUO) local-catalog $(BUILD_VARS)

# ------------------------------------------------------------
# pb update

# Helpers for managing the "pb" repo:
#  * `make pb-build` to rebuild pb boot files
#  * `make pb-stage` after updating `PB_BRANCH`
#  * `make pb-push` to upload the branch after checking that
#    the staged branch looks right
# If you don't have push access to `PB_REPO`, you may need to
# change the origin of your "pb" checkout.

pb-fetch: $(ZUO)
	$(RUN_ZUO) pb-fetch $(BUILD_VARS)

pb-build: $(ZUO)
	$(RUN_ZUO) pb-build $(BUILD_VARS)

pb-stage: $(ZUO)
	$(RUN_ZUO) pb-stage $(BUILD_VARS)

pb-push: $(ZUO)
	$(RUN_ZUO) pb-push $(BUILD_VARS)

# ------------------------------------------------------------
# Clean (which just gives advice)

clean:
	@echo "No makefile support for cleaning. Instead, try"
	@echo "  git clean -d -x -f ."
	@exit 1

# ------------------------------------------------------------
# Linking all packages (development mode; not an installer build)

pkgs-catalog: $(ZUO)
	$(RUN_ZUO) pkgs-catalog $(BUILD_VARS)

# ------------------------------------------------------------
# Configuration options for building installers

# On variable definitions: Spaces are allowed where noted and
# disallowed otherwise. If a variable name ends in "_q" or "_qq", then
# it means that the variable can expand to include double-quote marks.
# (If a variable's name ends in "_qq", then it used to allow a
# combination of single-quote and double-quote marks, sortof, but
# that's no longer supported at all; use `zuo` directly if that's
# needed). If a variable's name does not end in "_q" or "_qq", don't
# use any quote marks on the right-hand side of its definition.

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

# Set `PACK_BUILT_OPTIONS` to "--mode <mode>" to force all packages to
# a specific mode, but the default infers "built" or "binary"
PACK_BUILT_OPTIONS =

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

# Alternate way to set the number of parallel jobs used for package and
# setup operations; leave empty or set to "-j <jobs>":
JOB_OPTIONS =

# $(USER_RACKET) arguments for a command to run after the server has
# started; normally set by the `installers' target:
SERVE_DURING_CMD_qq =

# To configure package installations for the installer:
PKG_INSTALL_OPTIONS =

# Set to `--skip` to avoid unpacking collects from the server:
UNPACK_COLLECTS_FLAGS = 

# The `test-client` atarget is an optional test step for an installer
# build, were `TEST_PKGS` names extra packages to install, and
# `TEST_ARGS_q` is a set of arguments to `raco test`. This step will
# not make sense for some kinds of builds, such as source builds or
# cross-platform builds.
TEST_PKGS =
TEST_ARGS_q =

# Backward-compatibility target selector: `base` or `cs-base` or `bc-base`
CLIENT_BASE = base

# Backward-compatibility  target selector: `win-base` or `win-cs-base` or `win-bc-base`
WIN32_CLIENT_BASE = win-base

# Backward-compatibility target selector: `bundle-from-server` or `bundle-cross-from-server`
BUNDLE_FROM_SERVER_TARGET = bundle-from-server

# For `client-from-site`, relative path on server for the site
SITE_PATH =

DISTRO_BUILD_VARS = SERVER_COMPILE_MACHINE="$(SERVER_COMPILE_MACHINE)" \
                    DOC_SEARCH="$(DOC_SEARCH)" \
                    SERVER="$(SERVER)" \
                    SERVER_PORT="$(SERVER_PORT)" \
                    SERVER_URL_SCHEME="$(SERVER_URL_SCHEME)" \
                    SERVER_CATALOG_PATH="$(SERVER_CATALOG_PATH)" \
                    SERVER_COLLECTS_PATH="$(SERVER_COLLECTS_PATH)" \
                    SERVER_HOSTS="$(SERVER_HOSTS)" \
                    PACK_BUILT_OPTIONS="$(PACK_BUILT_OPTIONS)" \
                    RELEASE_MODE="$(RELEASE_MODE)" \
                    SOURCE_MODE="$(SOURCE_MODE)" \
                    VERSIONLESS_MODE="$(VERSIONLESS_MODE)" \
                    MAC_PKG_MODE="$(MAC_PKG_MODE)" \
                    TGZ_MODE="$(TGZ_MODE)" \
                    INSTALLER_OPTIONS="$(INSTALLER_OPTIONS)" \
                    PKG_SOURCE_MODE="$(PKG_SOURCE_MODE)" \
                    DISABLE_STATIC_LIBS="$(DISABLE_STATIC_LIBS)" \
                    INSTALLER_PRE_PROCESS_BASE64="$(INSTALLER_PRE_PROCESS_BASE64)" \
                    INSTALLER_POST_PROCESS_BASE64="$(INSTALLER_POST_PROCESS_BASE64)" \
                    DIST_NAME="$(DIST_NAME)" \
                    DIST_BASE="$(DIST_BASE)" \
                    DIST_DIR="$(DIST_DIR)" \
                    DIST_SUFFIX="$(DIST_SUFFIX)" \
                    DIST_DESC="$(DIST_DESC)" \
                    DIST_CATALOGS_q='$(DIST_CATALOGS_q)' \
                    BUILD_STAMP="$(BUILD_STAMP)" \
                    INSTALL_NAME="$(INSTALL_NAME)" \
                    SIGN_IDENTITY="$(SIGN_IDENTITY)" \
                    NOTARIZATION_CONFIG="$(NOTARIZATION_CONFIG)" \
                    OSSLSIGNCODE_ARGS_BASE64="$(OSSLSIGNCODE_ARGS_BASE64)" \
                    README="$(README)" \
                    UPLOAD="$(UPLOAD)" \
                    CONFIG="$(CONFIG)" \
                    CONFIG_MODE="$(CONFIG_MODE)" \
                    CLEAN_MODE="$(CLEAN_MODE)" \
                    JOB_OPTIONS="$(JOB_OPTIONS)" \
                    SERVE_DURING_CMD_qq='$(SERVE_DURING_CMD_qq)' \
                    PKG_INSTALL_OPTIONS="$(PKG_INSTALL_OPTIONS)" \
                    UNPACK_COLLECTS_FLAGS="$(UNPACK_COLLECTS_FLAGS)" \
                    TEST_PKGS="$(TEST_PKGS)" \
                    TEST_ARGS_q='$(TEST_ARGS_q)' \
                    CLIENT_BASE="$(CLIENT_BASE)" \
                    WIN32_CLIENT_BASE="$(WIN32_CLIENT_BASE)" \
                    BUNDLE_FROM_SERVER_TARGET="$(BUNDLE_FROM_SERVER_TARGET)"

# ------------------------------------------------------------
# On a server platform (for an installer build):

# These targets require GNU `make`, so that we don't have to propagate
# variables through all of the target layers.

server: $(ZUO)
	$(RUN_ZUO) server $(BUILD_VARS) $(DISTRO_BUILD_VARS)

server-from-base: $(ZUO)
	$(RUN_ZUO) server-from-base $(BUILD_VARS) $(DISTRO_BUILD_VARS)

# ------------------------------------------------------------
# On each supported platform (for an installer build):
#
# The `client` (and `win-client`) targets are also used by
# `distro-build/drive-clients`, which is in turn run by the
# `installers` target.
#
# For a non-Windows machine, if "build/log" exists, then
# keep the "build/user" directory on the grounds that the
# client is the same as the server.

client: $(ZUO)
	$(RUN_ZUO) client $(BUILD_VARS) $(DISTRO_BUILD_VARS)

client-compile-any: $(ZUO)
	$(RUN_ZUO) client-compile-any $(BUILD_VARS) $(DISTRO_BUILD_VARS)


test-client: $(ZUO)
	$(RUN_ZUO) test-client $(BUILD_VARS) $(DISTRO_BUILD_VARS)

# ------------------------------------------------------------
# On a supported platform (for an installer build) after a `make site'
# has completed; SERVER, SERVER_PORT (usually 80), and SITE_PATH
# should be set, and other configurations are propagated; normally,
# README should be set (possibly to empty), because a site doesn't
# provide a generic "README.txt".

client-from-site:
	$(RUN_ZUO) client-from-site $(BUILD_VARS) $(DISTRO_BUILD_VARS)

# ------------------------------------------------------------
# Drive installer build across server and clients:

# Full server build and clients drive, based on `CONFIG':
installers: $(ZUO)
	$(RUN_ZUO) installers $(BUILD_VARS) $(DISTRO_BUILD_VARS)

# Server is already built; start it and drive clients:
installers-from-built: $(ZUO)
	$(RUN_ZUO) installers-from-built $(BUILD_VARS) $(DISTRO_BUILD_VARS)

describe-clients: $(ZUO)
	$(RUN_ZUO) describe-clients $(BUILD_VARS) $(DISTRO_BUILD_VARS)

# ------------------------------------------------------------
# Create installers, then assemble as a web site:

site: $(ZUO)
	$(RUN_ZUO) site $(BUILD_VARS) $(DISTRO_BUILD_VARS)

site-from-installers: $(ZUO)
	$(RUN_ZUO) site-from-installers $(BUILD_VARS) $(DISTRO_BUILD_VARS)

# ------------------------------------------------------------
# Create a snapshot site:

snapshot-site: $(ZUO)
	$(RUN_ZUO) snapshot-site $(BUILD_VARS) $(DISTRO_BUILD_VARS)

snapshot-at-site: $(ZUO)
	$(RUN_ZUO) snapshot-at-site $(BUILD_VARS) $(DISTRO_BUILD_VARS)

# ------------------------------------------------------------
# Run steps that require a working `racket` to build things
# that need to be in sync to build `racket` in the first place

derived: $(ZUO)
	$(RUN_ZUO) derived $(BUILD_VARS)

# ------------------------------------------------------------
# Compatibility targets

cs-base: $(ZUO)
	$(RUN_ZUO) base $(BUILD_VARS) VM=cs

bc-base: $(ZUO)
	$(RUN_ZUO) base $(BUILD_VARS) VM=bc

cs-in-place: $(ZUO)
	$(RUN_ZUO) in-place $(BUILD_VARS) VM=cs

bc-in-place: $(ZUO)
	$(RUN_ZUO) in-place $(BUILD_VARS) VM=bc

win32-in-place: $(ZUO)
	$(RUN_ZUO) in-place $(BUILD_VARS)

win32-as-is: $(ZUO)
	$(RUN_ZUO) as-is $(BUILD_VARS)

win32-base: $(ZUO)
	$(RUN_ZUO) base $(BUILD_VARS)

win32-cs: $(ZUO)
	$(RUN_ZUO) in-place $(BUILD_VARS) VM=cs

win32-cs-in-place: $(ZUO)
	$(RUN_ZUO) in-place $(BUILD_VARS) VM=cs

win32-cs-as-is: $(ZUO)
	$(RUN_ZUO) as-is $(BUILD_VARS) VM=cs

win32-cs-base: $(ZUO)
	$(RUN_ZUO) base $(BUILD_VARS) VM=cs

win32-both: $(ZUO)
	$(RUN_ZUO) both $(BUILD_VARS)

win32-also-cs: $(ZUO)
	$(RUN_ZUO) also-cs $(BUILD_VARS)

win-client: $(ZUO)
	$(RUN_ZUO) client $(BUILD_VARS) $(DISTRO_BUILD_VARS)

win-test-client:  $(ZUO)
	$(RUN_ZUO) test-client $(BUILD_VARS) $(DISTRO_BUILD_VARS)

fetch-pb: $(ZUO)
	$(RUN_ZUO) pb-fetch $(BUILD_VARS)

ping: $(ZUO)
	$(RUN_ZUO) ping $(BUILD_VARS)

# ------------------------------------------------------------
# Zuo build rules

racket/src/build/bin/zuo: racket/src/zuo/zuo.c
	mkdir -p racket/src/build/bin
	$(CC_FOR_BUILD) $(CFLAGS_FOR_BUILD) -DZUO_LIB_PATH='"../../zuo/lib"' -o $(ZUO) racket/src/zuo/zuo.c

racket\src\build\zuo.exe: racket\src\zuo\zuo.c
	IF NOT EXIST racket\src\build cmd /c mkdir racket\src\build
	cd racket\src\build && cl.exe /O2 /DZUO_LIB_PATH=\"..\\zuo\\lib\" /Fezuo.exe ..\zuo\zuo.c

# ------------------------------------------------------------
# OpenBSD `make` workaround

# Open BSD sees the `!` lines that are intended for `nmake`, and it
# treats them as an empty target name with dependencies `if`, `0`,
# and `endif` --- and since that empty target is the first target, it's
# treated as the default, so bounce to the actual default target
# \
!if 0
#\
if endif 0: in-place
# \
!endif
