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
PKGS = main-distribution plt-services

# ------------------------------------------------------------
# In-place build

PLAIN_RACKET = racket/bin/racket
WIN32_PLAIN_RACKET = racket\racket

MACOSX_CHECK_ARGS = -I racket/base -e '(case (system-type) [(macosx) (exit 0)] [else (exit 1)])'
MACOSX_CHECK = $(PLAIN_RACKET) -G build/config $(MACOSX_CHECK_ARGS)

LINK_MODE = --save

CPUS = 

in-place:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-in-place PKGS="$(PKGS)" ; \
         else $(MAKE) cpus-in-place CPUS="$(CPUS)" PKGS="$(PKGS)" ; fi

cpus-in-place:
	$(MAKE) -j $(CPUS) plain-in-place JOB_OPTIONS="-j $(CPUS)" PKGS="$(PKGS)"

# Explicitly propagate variables for non-GNU `make's:
PKG_LINK_COPY_ARGS = PKGS="$(PKGS)" LINK_MODE="$(LINK_MODE)"
LIBSETUP = -N raco -l- raco setup

plain-in-place:
	$(MAKE) base
	if $(MACOSX_CHECK) ; then $(MAKE) native-from-git ; fi
	$(MAKE) pkg-links $(PKG_LINK_COPY_ARGS)
	$(PLAIN_RACKET) $(LIBSETUP) $(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)

# For Windows: set up the following collections first, so that native
# libraries are in place for use by a full setup:
LIB_PRE_COLLECTS = racket db com

win32-in-place:
	$(MAKE) win32-base
	$(MAKE) win32-pkg-links $(PKG_LINK_COPY_ARGS)
	$(WIN32_PLAIN_RACKET) $(LIBSETUP) -nxiID $(JOB_OPTIONS) $(PLT_SETUP_OPTIONS) $(LIB_PRE_COLLECTS)
	$(WIN32_PLAIN_RACKET) $(LIBSETUP) $(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)

again:
	$(MAKE) LINK_MODE="--restore"

IN_PLACE_COPY_ARGS = JOB_OPTIONS="$(JOB_OPTIONS)" PLT_SETUP_OPTIONS="$(PLT_SETUP_OPTIONS)"

win32-again:
	$(MAKE) LINK_MODE="--restore" $(IN_PLACE_COPY_ARGS)


# ------------------------------------------------------------
# Unix-style build (Unix and Mac OS X, only)

PREFIX = 

CONFIG_PREFIX_ARGS = --prefix="$(PREFIX)" --enable-macprefix
UNIX_RACO_ARGS = $(JOB_OPTIONS) --catalog build/local/catalog --auto -i
UNIX_BASE_ARGS = SELF_FLAGS_qq="" SKIP_DESTDIR_FIX="skip"

unix-style:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-unix-style ; \
         else $(MAKE) cpus-unix-style ; fi

cpus-unix-style:
	$(MAKE) -j $(CPUS) plain-unix-style JOB_OPTIONS="-j $(CPUS)"

plain-unix-style:
	if [ "$(PREFIX)" = "" ] ; then $(MAKE) error-need-prefix ; fi
	$(MAKE) base CONFIGURE_ARGS_qq='$(CONFIGURE_ARGS_qq) $(CONFIG_PREFIX_ARGS)' $(UNIX_BASE_ARGS)
	$(MAKE) local-catalog-maybe-native RACKET="$(DESTDIR)$(PREFIX)/bin/racket"
	"$(DESTDIR)$(PREFIX)/bin/raco" pkg install $(UNIX_RACO_ARGS) $(REQUIRED_PKGS) $(PKGS)
	cd racket/src/build; $(MAKE) fix-paths

error-need-prefix:
	: ================================================================
	: Please supply PREFIX="<dest-dir>" to set the install destination
	: ================================================================
	exit 1

local-catalog-maybe-native:
	if $(RACKET) $(MACOSX_CHECK_ARGS) ; \
         then $(MAKE) local-catalog ; \
         else $(MAKE) local-source-catalog ; fi

# ------------------------------------------------------------
# Base build

# During this step, we use a configuration file that indicates
# an empty set of link files, so that any installation-wide
# links or packages are ignored during the base build.

CONFIGURE_ARGS_qq = 

SELF_FLAGS_qq = SELF_RACKET_FLAGS="-G `cd ../../../build/config; pwd`"

base:
	mkdir -p build/config
	echo '#hash((links-search-files . ()))' > build/config/config.rktd
	mkdir -p racket/src/build
	$(MAKE) racket/src/build/Makefile
	cd racket/src/build; $(MAKE) reconfigure
	cd racket/src/build; $(MAKE) $(SELF_FLAGS_qq)
	cd racket/src/build; $(MAKE) install $(SELF_FLAGS_qq) PLT_SETUP_OPTIONS="$(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)"

win32-base:
	$(MAKE) win32-remove-setup-dlls
	IF NOT EXIST build\config cmd /c mkdir build\config
	cmd /c echo #hash((links-search-files . ())) > build\config\config.rktd
	cmd /c racket\src\worksp\build-at racket\src\worksp ..\..\..\build\config $(JOB_OPTIONS) $(PLT_SETUP_OPTIONS)

# Start by removing DLLs that may be loaded by `raco setup`
win32-remove-setup-dlls:
	IF EXIST racket\lib\longdouble.dll cmd /c del racket\lib\longdouble.dll
	IF EXIST racket\lib\libiconv-2.dll cmd /c del racket\lib\libiconv-2.dll
	IF EXIST racket\lib\iconv.dll cmd /c del racket\lib\iconv.dll
	IF EXIST racket\lib\libeay32.dll cmd /c del racket\lib\libeay32.dll
	IF EXIST racket\lib\ssleay32.dll cmd /c del racket\lib\ssleay32.dll

racket/src/build/Makefile: racket/src/configure racket/src/Makefile.in
	cd racket/src/build; ../configure $(CONFIGURE_ARGS_qq)

# ------------------------------------------------------------
# Configuration options for building installers

# On variable definitions: Spaces are allowed where noted and
# disallowed otherwise. If a variable name ends in "_q", then it means
# that the variable can expand to include double-quote marks. If a
# variable's name ends in "_qq", then it expands to a combination of
# single-quote and double-quote marks. If a variable's name does not
# end in "_q" or "_qq", don't use any quote marks on the right-hand
# side of its definition.

# Catalog for sources and native packages; use "local" to bootstrap
# from package directories (in the same directory as this makefile)
# plus the GitHub repository of raw native libraries. Otherwise, it's
# a URL (spaces allowed).
SRC_CATALOG = local

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

# Set to "--mac-pkg" to create ".pkg"-based installers for Mac OS X,
# instead of a ".dmg" for drag-and-drop installation:
MAC_PKG_MODE =

# Set to "--source --no-setup" to include packages in an installer
# (or archive) only in source form:
PKG_SOURCE_MODE = 

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

# A signing identity (spaces allowed) for Mac OS X binaries in an
# installer:
SIGN_IDENTITY = 

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

# A command to run after the server has started; normally set by
# the `installers' target:
SERVE_DURING_CMD_qq =

# ------------------------------------------------------------
# Helpers

# Needed for any distribution:
REQUIRED_PKGS = racket-lib

# Packages needed for building distribution:
DISTRO_BUILD_PKGS = distro-build-lib

SVR_PRT = $(SERVER):$(SERVER_PORT)

SVR_CAT = http://$(SVR_PRT)/$(SERVER_CATALOG_PATH)

# Helper macros:
USER_CONFIG = -G build/user/config -A build/user
RACKET = racket/bin/racket $(USER_CONFIG)
RACO = racket/bin/racket $(USER_CONFIG) -N raco -l- raco
WIN32_RACKET = racket\racket $(USER_CONFIG)
WIN32_RACO = racket\racket $(USER_CONFIG) -N raco -l- raco
X_AUTO_OPTIONS = --skip-installed --deps search-auto --pkgs $(JOB_OPTIONS)
USER_AUTO_OPTIONS = --scope user $(X_AUTO_OPTIONS)
LOCAL_USER_AUTO = --catalog build/local/catalog $(USER_AUTO_OPTIONS)
SOURCE_USER_AUTO_q = --catalog "$(SRC_CATALOG)" $(USER_AUTO_OPTIONS)
REMOTE_USER_AUTO = --catalog $(SVR_CAT) $(USER_AUTO_OPTIONS)
REMOTE_INST_AUTO = --catalog $(SVR_CAT) --scope installation $(X_AUTO_OPTIONS)
CONFIG_MODE_q = "$(CONFIG)" "$(CONFIG_MODE)"
BUNDLE_CONFIG = bundle/racket/etc/config.rktd
BUNDLE_RACO_FLAGS = -A bundle/user -l raco
BUNDLE_RACO = bundle/racket/bin/racket $(BUNDLE_RACO_FLAGS)
WIN32_BUNDLE_RACO = bundle\racket\racket $(BUNDLE_RACO_FLAGS)

# ------------------------------------------------------------
# Linking all packages (development mode; not an installer build)

LINK_ALL = -U -G build/config racket/src/link-all.rkt ++dir pkgs ++dir native-pkgs

pkg-links:
	$(PLAIN_RACKET) $(LINK_ALL) $(LINK_MODE) $(PKGS) $(REQUIRED_PKGS)

win32-pkg-links:
	IF NOT EXIST native-pkgs\racket-win32-i386 $(MAKE) complain-no-submodule
	$(MAKE) pkg-links PLAIN_RACKET="$(WIN32_PLAIN_RACKET)" LINK_MODE="$(LINK_MODE)" PKGS="$(PKGS)"

# ------------------------------------------------------------
# On a server platform (for an installer build):

# These targets require GNU `make', so that we don't have to propagate
# variables through all of the target layers.

server:
	$(MAKE) base
	$(MAKE) server-from-base

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

local-from-base:
	$(MAKE) build/site.rkt
	$(MAKE) stamp
	if [ "$(SRC_CATALOG)" = 'local' ] ; \
          then $(MAKE) build-from-local ; \
          else $(MAKE) build-from-catalog ; fi

server-from-base:
	$(MAKE) local-from-base
	$(MAKE) origin-collects
	$(MAKE) built-catalog
	$(MAKE) built-catalog-server

# Boostrap mode: make packages from local directories:
build-from-local:
	$(MAKE) local-catalog
	$(MAKE) local-build

# Set up a local catalog (useful on its own):
local-catalog:
	$(MAKE) native-from-git
	$(MAKE) native-catalog
	$(MAKE) local-source-catalog

# Get pre-built native libraries from the repo:
native-from-git:
	if [ ! -d native-pkgs/racket-win32-i386 ]; then $(MAKE) complain-no-submodule ; fi
complain-no-submodule:
	: ================================================================
	: Native packages are not in the expected subdirectory. Probably,
	: you need to use 'git submodule init' and 'git submodule update' to get
	: the submodule for native packages.
	: ================================================================
	exit 1

# Create packages and a catalog for all native libraries:
PACK_NATIVE = --native --pack build/native/pkgs \
              ++catalog build/native/catalog \
	      ++catalog build/local/catalog
native-catalog:
	$(RACKET) racket/src/pack-all.rkt --mods $(PACK_NATIVE) native-pkgs

# Create a catalog for all packages in this directory:
local-source-catalog:
	$(RACKET) racket/src/pack-all.rkt --mods ++catalog build/local/catalog pkgs

# Clear out a package build in "build/user", and then install
# packages:
local-build:
	$(MAKE) fresh-user
	$(MAKE) packages-from-local

fresh-user:
	rm -rf build/user

set-server-config:
	$(RACKET) -l distro-build/set-server-config build/user/config/config.rktd $(CONFIG_MODE_q) "$(DOC_SEARCH)" "" "" ""

# Install packages from the source copies in this directory. The
# packages are installed in user scope, but we set the add-on
# directory to "build/user", so that we don't affect the actual
# current user's installation (and to a large degree we're insulated
# from it):
packages-from-local:
	$(RACO) pkg install $(LOCAL_USER_AUTO) $(REQUIRED_PKGS) $(DISTRO_BUILD_PKGS)
	$(MAKE) set-server-config
	$(RACKET) -l- distro-build/pkg-info -o build/pkgs.rktd build/local/catalog
	$(RACKET) -l distro-build/install-pkgs $(CONFIG_MODE_q) "$(PKGS)" $(LOCAL_USER_AUTO)
	$(RACO) setup --avoid-main $(JOB_OPTIONS)

# Install packages from a source catalog (as an alternative to
# `build-from-local'), where the source catalog is specified as
# `SRC_CATALOG':
build-from-catalog:
	$(MAKE) fresh-user
	$(RACO) pkg install --all-platforms $(SOURCE_USER_AUTO_q) $(REQUIRED_PKGS) $(DISTRO_BUILD_PKGS)
	$(MAKE) set-server-config
	$(RACKET) -l- distro-build/pkg-info -o build/pkgs.rktd $(SRC_CATALOG)
	$(RACKET) -l distro-build/install-pkgs $(CONFIG_MODE_q) "$(PKGS)" $(SOURCE_USER_AUTO_q) --all-platforms
	$(RACO) setup --avoid-main $(JOB_OPTIONS)

# Although a client will build its own "collects", pack up the
# server's version to be used by each client, so that every client has
# exactly the same bytecode (which matters for SHA1-based dependency
# tracking):
origin-collects:
	$(RACKET) -l distro-build/pack-collects

# Now that we've built packages from local sources, create "built"
# versions of the packages from the installation into "build/user":
built-catalog:
	$(RACKET) -l distro-build/pack-built build/pkgs.rktd

# Run a catalog server to provide pre-built packages, as well
# as the copy of the server's "collects" tree:
built-catalog-server:
	if [ -d ".git" ]; then git update-server-info ; fi
	$(RACKET) -l distro-build/serve-catalog $(CONFIG_MODE_q) "$(SERVER_HOSTS)" $(SERVER_PORT) $(SERVE_DURING_CMD_qq)

# Demonstrate how a catalog server for binary packages works,
# which involves creating package archives in "binary" mode
# instead of "built" mode:
binary-catalog:
	$(RACKET) -l- distro-build/pack-built --mode binary build/pkgs.rktd
binary-catalog-server:
	$(RACKET) -l- distro-build/serve-catalog --mode binary $(CONFIG_MODE_q) "$(SERVER_HOSTS)" $(SERVER_PORT)

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

PROP_ARGS = SERVER=$(SERVER) SERVER_PORT=$(SERVER_PORT) SERVER_HOSTS="$(SERVER_HOSTS)" \
            PKGS="$(PKGS)" BUILD_STAMP="$(BUILD_STAMP)" \
	    RELEASE_MODE=$(RELEASE_MODE) SOURCE_MODE=$(SOURCE_MODE) \
            VERSIONLESS_MODE=$(VERSIONLESS_MODE) MAC_PKG_MODE=$(MAC_PKG_MODE) \
            PKG_SOURCE_MODE="$(PKG_SOURCE_MODE)" INSTALL_NAME="$(INSTALL_NAME)"\
            DIST_NAME="$(DIST_NAME)" DIST_BASE=$(DIST_BASE) \
            DIST_DIR=$(DIST_DIR) DIST_SUFFIX=$(DIST_SUFFIX) UPLOAD="$(UPLOAD)" \
            DIST_DESC="$(DIST_DESC)" README="$(README)" SIGN_IDENTITY="$(SIGN_IDENTITY)"\
            JOB_OPTIONS="$(JOB_OPTIONS)"

COPY_ARGS = $(PROP_ARGS) \
            SERVER_CATALOG_PATH=$(SERVER_CATALOG_PATH) SERVER_COLLECTS_PATH=$(SERVER_COLLECTS_PATH)

# Not copied, because used only immediately: DOC_SEARCH and DIST_CATALOGS_q

SET_BUNDLE_CONFIG_q = $(BUNDLE_CONFIG) "$(INSTALL_NAME)" "$(BUILD_STAMP)" "$(DOC_SEARCH)" $(DIST_CATALOGS_q)

client:
	if [ ! -d build/log ] ; then rm -rf build/user ; fi
	$(MAKE) base $(COPY_ARGS)
	$(MAKE) distro-build-from-server $(COPY_ARGS)
	$(MAKE) bundle-from-server $(COPY_ARGS)
	$(RACKET) -l distro-build/set-config $(SET_BUNDLE_CONFIG_q)
	$(MAKE) installer-from-bundle $(COPY_ARGS)

win32-client:
	IF EXIST build\user cmd /c del /f /s /q build\user
	$(MAKE) win32-base $(COPY_ARGS)
	$(MAKE) win32-distro-build-from-server $(COPY_ARGS)
	$(MAKE) win32-bundle-from-server $(COPY_ARGS)
	$(WIN32_RACKET) -l distro-build/set-config $(SET_BUNDLE_CONFIG_q)
	$(MAKE) win32-installer-from-bundle $(COPY_ARGS)

# Install the "distro-build" package from the server into
# a local build:
distro-build-from-server:
	$(RACO) pkg install $(REMOTE_USER_AUTO) distro-build-client

# Copy our local build into a "bundle/racket" build, dropping in the
# process things that should not be in an installer (such as the "src"
# directory). Then, replace the "collects" tree with the one from the
# server. Install required packages next, because they may include
# packages that are needed kto make core functionality work right
# (which as the SQLite3 library). At last, install the selected packages
# from the server, and the run a post-adjustment script.
bundle-from-server:
	rm -rf bundle
	mkdir -p bundle/racket
	$(RACKET) -l setup/unixstyle-install bundle racket bundle/racket
	$(RACKET) -l distro-build/unpack-collects http://$(SVR_PRT)/$(SERVER_COLLECTS_PATH)
	$(BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(REQUIRED_PKGS)
	$(BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(PKGS)
	$(RACKET) -l setup/unixstyle-install post-adjust "$(SOURCE_MODE)" "$(PKG_SOURCE_MODE)" racket bundle/racket

UPLOAD_q = --readme "$(README)" --upload "$(UPLOAD)" --desc "$(DIST_DESC)"
DIST_ARGS_q = $(UPLOAD_q) $(RELEASE_MODE) $(SOURCE_MODE) $(VERSIONLESS_MODE) $(MAC_PKG_MODE) \
              "$(DIST_NAME)" $(DIST_BASE) $(DIST_DIR) "$(DIST_SUFFIX)" \
              "$(SIGN_IDENTITY)"

# Create an installer from the build (with installed packages) that's
# in "bundle/racket":
installer-from-bundle:
	$(RACKET) -l- distro-build/installer $(DIST_ARGS_q)

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
	$(WIN32_RACKET) -l distro-build/unpack-collects http://$(SVR_PRT)/$(SERVER_COLLECTS_PATH)
	$(WIN32_BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(REQUIRED_PKGS)
	$(WIN32_BUNDLE_RACO) pkg install $(REMOTE_INST_AUTO) $(PKG_SOURCE_MODE) $(PKGS)

win32-installer-from-bundle:
	$(WIN32_RACKET) -l- distro-build/installer $(DIST_ARGS_q)

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
	make client $(FROM_SITE_ARGS)

# ------------------------------------------------------------
# Drive installer build across server and clients:

DRIVE_ARGS_q = $(RELEASE_MODE) $(VERSIONLESS_MODE) $(SOURCE_MODE) \
               $(CLEAN_MODE) "$(CONFIG)" "$(CONFIG_MODE)" \
               $(SERVER) $(SERVER_PORT) "$(SERVER_HOSTS)" \
               "$(PKGS)" "$(DOC_SEARCH)" "$(DIST_NAME)" $(DIST_BASE) $(DIST_DIR)
DRIVE_CMD_q = $(RACKET) -l- distro-build/drive-clients $(DRIVE_ARGS_q)

# Full server build and clients drive, based on `CONFIG':
installers:
	rm -rf build/installers
	$(MAKE) server SERVE_DURING_CMD_qq='$(DRIVE_CMD_q)'

# Server is already built; start it and drive clients:
installers-from-built:
	$(MAKE) built-catalog-server SERVE_DURING_CMD_qq='$(DRIVE_CMD_q)'

# Just the clients, assuming server is already running:
drive-clients:
	$(DRIVE_CMD)

# ------------------------------------------------------------
# Create installers, then assemble as a web site:

site:
	$(MAKE) installers
	$(MAKE) site-from-installers

DOC_CATALOGS = build/built/catalog build/native/catalog

site-from-installers:
	rm -rf build/docs
	$(RACKET) -l- distro-build/install-for-docs build/docs $(CONFIG_MODE_q) "$(PKGS)" $(DOC_CATALOGS)
	$(RACKET) -l- distro-build/assemble-site $(CONFIG_MODE_q)

# ------------------------------------------------------------
# Create a snapshot site:

snapshot-site:
	$(MAKE) site
	$(MAKE) snapshot-at-site

snapshot-at-site:
	$(RACKET) -l- distro-build/manage-snapshots $(CONFIG_MODE_q)
