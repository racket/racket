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

# In case of cross-installation, point explicitly to local content:
RUN_RACKET = $(PLAIN_RACKET) -G racket/etc -X racket/collects
WIN32_RUN_RACKET = $(WIN32_PLAIN_RACKET) -G racket/etc -X racket/collects

RUN_RACO = $(RUN_RACKET) -N raco -l- raco
WIN32_RUN_RACO = $(WIN32_RUN_RACKET) -N raco -l- raco

DEFAULT_SRC_CATALOG = http://pkgs.racket-lang.org

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

plain-in-place:
	$(MAKE) base
	$(MAKE) pkgs-catalog
	$(RUN_RACO) pkg update $(UPDATE_PKGS_ARGS)
	$(RUN_RACO) pkg install $(INSTALL_PKGS_ARGS)
	$(RUN_RACO) setup --only-foreign-libs $(ALL_PLT_SETUP_OPTIONS)
	$(RUN_RACO) setup $(ALL_PLT_SETUP_OPTIONS)

win32-in-place:
	$(MAKE) win32-base
	$(MAKE) win32-pkgs-catalog
	$(WIN32_RUN_RACO) pkg update $(UPDATE_PKGS_ARGS)
	$(WIN32_RUN_RACO) pkg install $(INSTALL_PKGS_ARGS)
	$(WIN32_RUN_RACO) setup --only-foreign-libs $(ALL_PLT_SETUP_OPTIONS)
	$(WIN32_RUN_RACO) setup $(ALL_PLT_SETUP_OPTIONS)

# Rebuild without consulting catalogs or package sources:

as-is:
	if [ "$(CPUS)" = "" ] ; \
         then $(MAKE) plain-as-is PKGS="$(PKGS)" ; \
         else $(MAKE) cpus-as-is CPUS="$(CPUS)" PKGS="$(PKGS)" ; fi

cpus-as-is:
	$(MAKE) -j $(CPUS) plain-as-is JOB_OPTIONS="-j $(CPUS)" PKGS="$(PKGS)"

plain-as-is:
	$(MAKE) base
	$(RUN_RACO) setup $(ALL_PLT_SETUP_OPTIONS)

win32-as-is:
	$(MAKE) win32-base
	$(WIN32_RUN_RACO) setup $(ALL_PLT_SETUP_OPTIONS)

# ------------------------------------------------------------
# Unix-style build (Unix and Mac OS X, only)

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
	$(MAKE) base CONFIGURE_ARGS_qq='$(CONFIGURE_ARGS_qq) $(CONFIG_PREFIX_ARGS)' $(UNIX_BASE_ARGS)
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


# For cross-compilation, build a native executable with no configure options:
native-for-cross:
	mkdir -p racket/src/build/cross
	$(MAKE) racket/src/build/cross/Makefile
	cd racket/src/build/cross; $(MAKE) reconfigure
	cd racket/src/build/cross/racket; $(MAKE)

racket/src/build/cross/Makefile: racket/src/configure racket/src/Makefile.in
	cd racket/src/build/cross; ../../configure

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
USER_CONFIG = -G build/user/config -X racket/collects -A build/user
RACKET = $(PLAIN_RACKET) $(USER_CONFIG)
RACO = $(PLAIN_RACKET) $(USER_CONFIG) -N raco -l- raco
WIN32_RACKET = $(WIN32_PLAIN_RACKET) $(USER_CONFIG)
WIN32_RACO = $(WIN32_PLAIN_RACKET) $(USER_CONFIG) -N raco -l- raco
X_AUTO_OPTIONS = --skip-installed --deps search-auto --pkgs $(JOB_OPTIONS)
USER_AUTO_OPTIONS = --scope user $(X_AUTO_OPTIONS)
LOCAL_USER_AUTO = --catalog build/local/catalog $(USER_AUTO_OPTIONS)
SOURCE_USER_AUTO_q = --catalog build/catalog-copy $(USER_AUTO_OPTIONS)
REMOTE_USER_AUTO = --catalog $(SVR_CAT) $(USER_AUTO_OPTIONS)
REMOTE_INST_AUTO = --catalog $(SVR_CAT) --scope installation $(X_AUTO_OPTIONS)
CONFIG_MODE_q = "$(CONFIG)" "$(CONFIG_MODE)"
BUNDLE_CONFIG = bundle/racket/etc/config.rktd
BUNDLE_RACO_FLAGS = -G bundle/racket/config -X bundle/racket/collects -A bundle/user -l raco
BUNDLE_RACO = $(PLAIN_RACKET) $(BUNDLE_RACO_FLAGS)
WIN32_BUNDLE_RACO = $(WIN32_PLAIN_RACKET) $(BUNDLE_RACO_FLAGS)

# ------------------------------------------------------------
# Linking all packages (development mode; not an installer build)

PKGS_CATALOG = -U -G build/config -l- pkg/dirs-catalog --link --check-metadata
PKGS_CONFIG = -U -G build/config racket/src/pkgs-config.rkt

pkgs-catalog:
	$(RUN_RACKET) $(PKGS_CATALOG) racket/share/pkgs-catalog pkgs
	$(RUN_RACKET) $(PKGS_CONFIG) "$(DEFAULT_SRC_CATALOG)" "$(SRC_CATALOG)"
	$(RUN_RACKET) racket/src/pkgs-check.rkt racket/share/pkgs-catalog

win32-pkgs-catalog:
	$(MAKE) pkgs-catalog PLAIN_RACKET="$(WIN32_PLAIN_RACKET)"

# ------------------------------------------------------------
# On a server platform (for an installer build):

# These targets require GNU `make', so that we don't have to propagate
# variables through all of the target layers.

server:
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
	$(RACO) pkg catalog-copy "$(SRC_CATALOG)" build/catalog-copy
	$(MAKE) server-cache-config
	$(RACO) pkg install --all-platforms $(SOURCE_USER_AUTO_q) $(REQUIRED_PKGS) $(DISTRO_BUILD_PKGS)
	$(MAKE) set-server-config
	$(RACKET) -l- distro-build/pkg-info -o build/pkgs.rktd build/catalog-copy
	$(RACKET) -l distro-build/install-pkgs $(CONFIG_MODE_q) "$(PKGS)" $(SOURCE_USER_AUTO_q) --all-platforms
	$(RACO) setup --avoid-main $(JOB_OPTIONS)

server-cache-config:
	$(RACO) pkg config -i --set download-cache-dir build/cache
	$(RACO) pkg config -i --set download-cache-max-files 1023
	$(RACO) pkg config -i --set download-cache-max-bytes 671088640

set-server-config:
	$(RACKET) -l distro-build/set-server-config build/user/config/config.rktd $(CONFIG_MODE_q) "" "" "$(DOC_SEARCH)" ""

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
            PKGS="$(PKGS)" PLAIN_RACKET="$(PLAIN_RACKET)" BUILD_STAMP="$(BUILD_STAMP)" \
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
	$(RACKET) -l setup/winstrip bundle/racket
	$(RACKET) -l setup/winvers-change bundle/racket
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
