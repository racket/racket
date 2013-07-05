# This makefile is meant to parse with both normal `make' and `nmake'
# (on Windows). On Windows, prefix each target with `win32-'.

# The targets here do not use dependencies (mostly), so it's a strange
# use of `make'.  Really, this makefile is an alternative to a pile of
# scripts, where each target plays the role of a script.

# The main targets are
#
#  in-place = build in "racket" with all packages in development mode
#
#  core = build in "racket" only (i.e., first step of `in-place')
#
#  server = build core, build packages listed in $(PKGS) or specified
#           via $(CONFIG), start server at port 9440
#
#  client = build core, create an installer with $(PKGS) with the help
#           of $(SERVER); result is recorded in "bundle/installer.txt"
#
#  installers = `server' plus `client' via $(CONFIG)

DEFAULT_PKGS = main-distribution plt-services

# ------------------------------------------------------------
# In-place build

PLAIN_RACKET = racket/bin/racket
WIN32_PLAIN_RACKET = racket\racket

MACOSX_CHECK = $(PLAIN_RACKET) -I racket/base -e '(case (system-type) [(macosx) (exit 0)] [else (exit 1)])'

CPUS = 

in-place:
	if [ "$(CPUS)" = "" ] ; then $(MAKE) plain-in-place ; else $(MAKE) cpus-in-place ; fi

cpus-in-place:
	$(MAKE) -j $(CPUS) plain-in-place PLT_SETUP_OPTIONS="-j $(CPUS)"

plain-in-place:
	$(MAKE) core
	if $(MACOSX_CHECK) ; then $(MAKE) native-from-git ; fi
	$(MAKE) pkg-links
	$(PLAIN_RACKET) -N raco -l- raco setup $(PLT_SETUP_OPTIONS)

win32-in-place:
	$(MAKE) win32-core
	$(MAKE) win32-pkg-links
	$(WIN32_PLAIN_RACKET) -N raco -l- raco setup $(PLT_SETUP_OPTIONS)

# ------------------------------------------------------------
# Core build

# During this step, we use a configuration file that indicates
# an empty set of link files, so that any installation-wide
# links or packages are ignored during the core build.

CONFIGURE_ARGS_qq = 

core:
	mkdir -p build/config
	echo '#hash((links-search-files . ()))' > build/config/config.rktd
	mkdir -p racket/src/build
	$(MAKE) racket/src/build/Makefile
	cd racket/src/build; $(MAKE) reconfigure
	cd racket/src/build; $(MAKE) SELF_RACKET_FLAGS="-G `cd ../../../build/config; pwd`"
	cd racket/src/build; $(MAKE) install SELF_RACKET_FLAGS="-G `cd ../../../build/config; pwd`"

win32-core:
	IF NOT EXIST build\config cmd /c mkdir -p build\config
	cmd /c echo #hash((links-search-files . ())) > build\config\config.rktd
	cmd /c racket\src\worksp\build-at racket\src\worksp ..\..\..\build\config $(PLT_SETUP_OPTIONS)

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

# Packages (separated by spaces) to include in a distribution:
PKGS = $(DEFAULT_PKGS)

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

# Set to "--release" to create release-mode installers (as opposed to
# snapshot installers):
RELEASE_MODE =

# Human-readable name (spaces allowed), installation name base, and
# Unix installation directory name for the generated installers:
DIST_NAME = Racket
DIST_BASE = racket
DIST_DIR = racket
# An extra suffix for the installer name, usually used to specify
# a variant of an OS:
DIST_SUFFIX = 
# A human-readable description (spaces allowed) of the generated
# installer, usually describing a platform:
DIST_DESC =

# Package catalog URLs (individually quoted as needed, separated by
# spaces) to install as the initial configuration in generated
# installers, where "" is replaced by the default configuration:
DIST_CATALOGS_q = ""

# Configuration of clients to run for a build farm, normally
# implemented with `#lang distro-build/config':
CONFIG = build/site.rkt

# A mode that is made available to the site-configuration module
# through the `current-mode' parameter:
CONFIG_MODE = default

# Set to "--clean" to flush client directories in a build farm
# (except as overridden in the `CONFIG' module):
CLEAN_MODE =

# A command to run after the server has started; normally set by
# the `installers' target:
SERVE_DURING_CMD_qq =

# ------------------------------------------------------------
# Helpers

# Needed for any distribution:
REQUIRED_PKGS = racket-lib

# Packages needed for building distribution:
DISTRO_BUILD_PKGS = distro-build

# To bootstrap, we use some "distro-build" libraries directly,
# instead of from an installed package:
DISTBLD = pkgs/distro-build

# Helper macros:
ADDON = build/user
RACKET = racket/bin/racket -A "$(ADDON)"
RACO = racket/bin/racket -A "$(ADDON)" -N raco -l- raco
WIN32_RACKET = racket\racket -A "$(ADDON)"
WIN32_RACO = racket\racket -A "$(ADDON)" -N raco -l- raco
X_AUTO_OPTIONS = --skip-installed --deps search-auto $(PLT_SETUP_OPTIONS)
USER_AUTO_OPTIONS = --scope user $(X_AUTO_OPTIONS)
LOCAL_USER_AUTO = --catalog build/local/catalog $(USER_AUTO_OPTIONS)
SOURCE_USER_AUTO_q = --catalog "$(SRC_CATALOG)" $(USER_AUTO_OPTIONS)
REMOTE_USER_AUTO = --catalog http://$(SERVER):9440/ $(USER_AUTO_OPTIONS)
REMOTE_INST_AUTO = --catalog http://$(SERVER):9440/ --scope installation $(X_AUTO_OPTIONS)
CONFIG_MODE_q = "$(CONFIG)" "$(CONFIG_MODE)"
BUNDLE_CONFIG = bundle/racket/etc/config.rktd

# ------------------------------------------------------------
# Linking all packages (development mode; not an installer build)

LINK_ALL = -U -G build/config racket/src/link-all.rkt ++dir pkgs ++dir build/native-pkgs
LINK_PKG_SPECS = --sticky "$(PKGS)" "$(DEFAULT_PKGS)"

pkg-links:
	$(PLAIN_RACKET) $(LINK_ALL) $(LINK_PKG_SPECS) $(PKGS)

win32-pkg-links:
	$(MAKE) pkg-links PLAIN_RACKET="$(WIN32_PLAIN_RACKET)"

# ------------------------------------------------------------
# On a server platform (for an installer build):

server:
	$(MAKE) core
	$(MAKE) stamp
	$(MAKE) server-from-core

stamp:
	if [ -d ".git" ] ; then $(MAKE) stamp-from-git ; else $(MAKE) stamp-from-date ; fi
stamp-from-git:
	echo `date +"%Y%m%d"`-`git log -1 --pretty=format:%h` > build/stamp.txt
stamp-from-date:
	date +"%Y%m%d" > build/stamp.txt

server-from-core:
	if [ "$(EEAPP)" = '' ] ; then $(MAKE) build-from-local ; else $(MAKE) build-from-catalog ; fi
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
	mkdir -p build
	if [ ! -d build/native-pkgs ]; then cd build; git clone git://github.com/plt/libs.git native-pkgs ; fi
	cd build/native-pkgs; git pull

# Create packages and a catalog for all native libraries:
native-catalog:
	$(RACKET) $(DISTBLD)/pack-native.rkt

# Create a catalog for all packages in this directory:
local-source-catalog:
	$(RACKET) $(DISTBLD)/catalog-local.rkt

# Clear out a package build in "build/user", and then install
# packages:
local-build:
	$(MAKE) fresh-user
	$(MAKE) packages-from-local

fresh-user:
	rm -rf build/user

set-config:
	$(RACKET) -l distro-build/set-config racket/etc/config.rktd $(CONFIG_MODE_q) "$(DOC_SEARCH)" ""

# Install packages from the source copies in this directory. The
# packages are installed in user scope, but we set the add-on
# directory to "build/user", so that we don't affect the actual
# current user's installation (and to a large degree we're insulated
# from it):
packages-from-local:
	$(RACO) pkg install $(LOCAL_USER_AUTO) $(REQUIRED_PKGS) $(DISTRO_BUILD_PKGS)
	$(MAKE) set-config
	$(RACKET) -l distro-build/install-pkgs $(CONFIG_MODE_q) "$(PKGS)" $(LOCAL_USER_AUTO)
	$(RACO) setup --avoid-main

# Install packages from a source catalog (as an alternative to
# `build-from-local'), where the source catalog is specified as
# `SRC_CATALOG':
build-from-catalog:
	$(MAKE) fresh-user
	$(RACO) pkg install $(SOURCE_USER_AUTO_q) $(REQUIRED_PKGS) $(DISTRO_BUILD_PKGS)
	$(MAKE) set-config
	$(RACKET) -l distro-build/install-pkgs $(CONFIG_MODE_q) "$(CONFIG_MODE)" "$(PKGS)" $(SOURCE_USER_AUTO_q)
	$(RACO) setup --avoid-main

# Although a client will build its own "collects", pack up the
# server's version to be used by each client, so that every client has
# exactly the same bytecode (which matters for SHA1-based dependency
# tracking):
origin-collects:
	$(RACKET) -l distro-build/pack-collects

# Now that we've built packages from local sources, create "built"
# versions of the packages from the installation into "build/user":
built-catalog:
	$(RACKET) -l distro-build/pack-built

# Run a catalog server to provide pre-built packages, as well
# as the copy of the server's "collects" tree:
built-catalog-server:
	if [ -d ".git" ]; then git update-server-info ; fi
	$(RACKET) -l distro-build/serve-catalog $(SERVE_DURING_CMD_qq)

# Demonstrate how a catalog server for binary packages works,
# which involves creating package archives in "binary" mode
# instead of "built" mode:
binary-catalog:
	$(RACKET) -l- distro-build/pack-built --mode binary
binary-catalog-server:
	$(RACKET) -l- distro-build/serve-catalog --mode binary

# ------------------------------------------------------------
# On each supported platform (for an installer build):
#
# The `client' and `win32-client' targets are also used by
# `distro-buid/drive-clients', which is in turn run by the
# `installers' target.
#
# For a non-Windows machine, if "build/drive" exists, then
# keep the "build/user" directory on the grounds that the
# client is the same as the server.

client:
	if [ ! -d build/drive ] ; then rm -rf build/user ; fi
	$(MAKE) core
	$(MAKE) distro-build-from-server
	$(MAKE) bundle-from-server
	$(MAKE) bundle-config
	$(MAKE) installer-from-bundle

COPY_ARGS = SERVER=$(SERVER) PKGS="$(PKGS)" RELEASE_MODE=$(RELEASE_MODE) \
            DIST_NAME="$(DIST_NAME)" DIST_BASE=$(DIST_BASE) \
            DIST_DIR=$(DIST_DIR) DIST_SUFFIX=$(DIST_SUFFIX) \
            DIST_DESC="$(DIST_DESC)" PLT_SETUP_OPTIONS="$(PLT_SETUP_OPTIONS)"

win32-client:
	IF EXIST build\user cmd /c rmdir /S /Q build\user
	$(MAKE) win32-core $(COPY_ARGS)
	$(MAKE) win32-distro-build-from-server $(COPY_ARGS)
	$(MAKE) win32-bundle-from-server $(COPY_ARGS)
	$(WIN32_RACKET) -l distro-build/set-config $(BUNDLE_CONFIG) $(CONFIG_MODE_q) "$(DOC_SEARCH)" $(DIST_CATALOGS_q)
	$(MAKE) win32-installer-from-bundle $(COPY_ARGS)

# Install the "distro-build" package from the server into
# a local build:
distro-build-from-server:
	$(RACO) pkg install $(REMOTE_USER_AUTO) distro-build

# Copy our local build into a "bundle/racket" build, dropping in the
# process things that should not be in an installer (such as the "src"
# directory). Then, replace the "collects" tree with the one from the
# server. Finally, install pre-built packages from the server:
bundle-from-server:
	rm -rf bundle
	mkdir -p bundle/racket
	$(RACKET) -l setup/unixstyle-install bundle racket bundle/racket
	$(RACKET) -l distro-build/unpack-collects http://$(SERVER):9440/
	bundle/racket/bin/raco pkg install $(REMOTE_INST_AUTO) $(PKGS) $(REQUIRED_PKGS)

bundle-config:
	$(RACKET) -l distro-build/set-config $(BUNDLE_CONFIG) $(CONFIG_MODE_q) "$(DOC_SEARCH)" $(DIST_CATALOGS_q)

UPLOAD_q = --upload http://$(SERVER):9440/ --desc "$(DIST_DESC)"
DIST_ARGS_q = $(UPLOAD_q) $(RELEASE_MODE) "$(DIST_NAME)" $(DIST_BASE) $(DIST_DIR) "$(DIST_SUFFIX)"

# Create an installer from the build (with installed packages) that's
# in "bundle/racket":
installer-from-bundle:
	$(RACKET) -l- distro-build/installer $(DIST_ARGS_q)

win32-distro-build-from-server:
	$(WIN32_RACO) pkg install $(REMOTE_USER_AUTO) distro-build

win32-bundle:
	IF EXIST bundle cmd /c rmdir /S /Q bundle
	cmd /c mkdir bundle\racket
	$(WIN32_RACKET) -l setup/unixstyle-install bundle racket bundle\racket
	$(WIN32_RACKET) -l setup/winstrip bundle\racket
	$(WIN32_RACKET) -l setup/winvers-change bundle\racket

win32-bundle-from-server:
	$(MAKE) win32-bundle $(COPY_ARGS)
	$(WIN32_RACKET) -l distro-build/unpack-collects http://$(SERVER):9440/
	bundle\racket\raco pkg install $(REMOTE_INST_AUTO) $(REQUIRED_PKGS)
	bundle\racket\raco pkg install $(REMOTE_INST_AUTO) $(PKGS)

win32-installer-from-bundle:
	$(WIN32_RACKET) -l- distro-build/installer $(DIST_ARGS_q)

# ------------------------------------------------------------
# Drive installer build across server and clients:

DRIVE_ARGS_q = $(RELEASE_MODE) $(CLEAN_MODE) "$(CONFIG)" "$(CONFIG_MODE)" \
               $(SERVER) "$(PKGS)" "$(DOC_SEARCH)" "$(DIST_NAME)" $(DIST_BASE) $(DIST_DIR)
DRIVE_CMD_q = $(RACKET) -l- distro-build/drive-clients $(DRIVE_ARGS_q)

# Full server build and clients drive, based on `CONFIG':
installers:
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

site-from-installers:
	$(RACKET) -l- distro-build/assemble-site $(CONFIG_MODE_q)

# ------------------------------------------------------------
# Create a snapshot site:

snapshot-site:
	$(MAKE) site
	$(MAKE) snapshot-at-site

snapshot-at-site:
	$(RACKET) -l- distro-build/manage-snapshots $(CONFIG_MODE_q)
