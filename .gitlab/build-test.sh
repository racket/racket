#! /bin/bash
# This script shows no error on shellcheck:
# https://github.com/koalaman/shellcheck
set -e

# ---------------------------------------------------------------------------------------------------
# Script called by jobs in .gitlab-ci.yml to build and test racket,
# possibly in a cross environment.

last_command=
current_command=
# keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# echo an error message before exiting
trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

# ---------------------------------------------------------------------------------------------------

function usage () {
    MSG=$1

    echo "${MSG}"
    echo
    echo "Usage: ./build-test.sh [--jobs <count>]"
    echo "                       [--single-thread]"
    echo "                       [--with-arch <arch>]"
    echo "                       [--with-debian <debian>]"
    echo "                       [--with-debian-mirror <debian-mirror>]"
    echo "                       [--with-configure-args <configure-args>]"
    echo "                       [--enable-cs]"
    echo "                       [--with-project-path <project-path>]"
    echo "                       [--with-chroot-path <chroot-path>]"
    echo "                       [--with-qemu-path <qemu-path>]"
    exit 1
}

DEBIAN=
DEBIAN_MIRROR=
JOBS=
RACKET_CONFIGURE_ARGS=
ARCH="$(uname -m)"
BUILD_DIR=${CI_PROJECT_DIR}
MAKE_TARGET="in-place"
CHROOT_DIR="/tmp/racket-chroot"
QEMU_PATH=

# Parse options
until
    opt=$1
    case ${opt} in
	--jobs)
	    shift
	    JOBS=$1
	    ;;
	--single-thread)
	    JOBS=1
	    ;;
	--with-arch)
	    shift
	    ARCH=$1
	    ;;
	--with-debian)
	    shift
	    DEBIAN=$1
	    ;;
	--with-debian-mirror)
	    shift
	    DEBIAN_MIRROR=$1
	    ;;
	--with-configure-args)
	    shift
	    RACKET_CONFIGURE_ARGS=$1
	    ;;
	--enable-cs)
	    MAKE_TARGET="cs"
	    ;;
	--with-project-path)
	    shift
	    BUILD_DIR=$1
	    ;;
	--with-chroot-path)
	    shift
	    CHROOT_DIR=$1
	    ;;
	--with-qemu-path)
	    shift
	    QEMU_PATH=$1
	    ;;
	?*)
	    usage "Unknown argument $1"
	    ;;
	*)
	    ;;
    esac
[ "x${opt}" = "x" ]
do
    shift
done	    

set -eu

# ---------------------------------------------------------------------------------------------------
# Set QEMU ARCH which depends on ARCH

if [ ! -e "/.chroot_is_done" ]
then
    QEMU_ARCH=

    case ${ARCH} in
	"amd64")
	    QEMU_ARCH="x86_64"
	    ;;
	"arm64")
	    QEMU_ARCH="aarch64"
	    ;;
	"armel"|"armhf"|"armv7l")
	    QEMU_ARCH="arm"
	    ;;
	"i386"|"mips"|"mipsel"|"mips64el"|"s390x"|"x86_64")
	    QEMU_ARCH=${ARCH}
	    ;;
	"ppc64el")
	    QEMU_ARCH="ppc64le"
	    ;;
	*)
	    echo "Unknown architecture ${ARCH}"
	    echo "Available archs: amd64, arm64, armel, armhf, armv7l, i386, mips, mipsel, mips64el, s390x, ppc64el"
	    echo "These are the official names for the debian ports available listed at:"
	    echo "https://www.debian.org/ports/"
	    echo "NOTE: we also accept x86_64 as an alias for amd64"
	    exit 1
	    ;;
    esac
fi
	
# ---------------------------------------------------------------------------------------------------
# Packages to install on the HOST
HOST_DEPENDENCIES="debootstrap binfmt-support sbuild rsync"

# Packages to install on the GUEST
GUEST_DEPENDENCIES="devscripts build-essential git m4 sudo python libfontconfig1-dev make gcc libpango1.0-dev libcairo2-dev openssl emacs25-nox libturbojpeg0-dev uuid-dev"

function setup_chroot {
    # Host dependencies
    echo "Installing host dependencies"
    apt-get install -y ${HOST_DEPENDENCIES}

    # Create chrooted environment
    echo "Creating chroot environment"
    mkdir "${CHROOT_DIR}"
    debootstrap --foreign --no-check-gpg --include=fakeroot,build-essential \
		--arch="${ARCH}" "${DEBIAN}" "${CHROOT_DIR}" "${DEBIAN_MIRROR}"
    cp ${QEMU_PATH}/bin/qemu-${QEMU_ARCH} "${CHROOT_DIR}"/usr/bin/qemu-${QEMU_ARCH}-static
    chroot "${CHROOT_DIR}" ./debootstrap/debootstrap --second-stage
    sbuild-createchroot --arch="${ARCH}" --foreign --setup-only \
			"${DEBIAN}" "${CHROOT_DIR}" "${DEBIAN_MIRROR}"

    # Install dependencies inside chroot
    echo "Installing guest dependencies"
    chroot "${CHROOT_DIR}" apt-get update
    chroot "${CHROOT_DIR}" apt-get --allow-unauthenticated install \
           -y ${GUEST_DEPENDENCIES}

    # Create build dir and copy travis build files to our chroot environment
    echo "Copying into chroot: ${BUILD_DIR}/ -> ${CHROOT_DIR}/${BUILD_DIR}/"
    mkdir -p "${CHROOT_DIR}"/"${BUILD_DIR}"
    rsync -av "${BUILD_DIR}"/ "${CHROOT_DIR}"/"${BUILD_DIR}"/

    # Indicate chroot environment has been set up
    touch "${CHROOT_DIR}"/.chroot_is_done

    # Call ourselves again which will cause tests to run
    echo "Recursively calling script"
    if [ ${MAKE_TARGET} = "cs" ]; then
	chroot "${CHROOT_DIR}" bash -c "cd ${BUILD_DIR} && ./.gitlab/build-test.sh --jobs ${JOBS} --with-arch ${ARCH} --with-project-path ${BUILD_DIR} --enable-cs"
    else
	chroot "${CHROOT_DIR}" bash -c "cd ${BUILD_DIR} && ./.gitlab/build-test.sh --jobs ${JOBS} --with-arch ${ARCH} --with-project-path ${BUILD_DIR}"
    fi
}

# Information about environment
echo "Environment information"
echo "======================="
echo "              Machine : $(uname -m)"
echo "                 Jobs : ${JOBS}"
echo "          Target Arch : ${ARCH}"
echo "          chroot Path : ${CHROOT_DIR}"
echo "           Build Path : ${BUILD_DIR}"
echo "          Make Target : ${MAKE_TARGET}"
echo "               Debian : ${DEBIAN}"
echo "        Debian Mirror : ${DEBIAN_MIRROR}"
echo "Racket Configure Args : ${RACKET_CONFIGURE_ARGS}"
if [ ! -e "/.chroot_is_done" ]; then
echo "            QEMU Arch : ${QEMU_ARCH}"
fi

if [ ! -e "/.chroot_is_done" ]; then
  if [ "${ARCH}" != "$(uname -m)" ]; then
    # test run, need to set up chrooted environment first
    echo "Setting up chrooted ${ARCH} environment"
    setup_chroot
  else # We are compiling and running tests natively
    apt-get install -y ${GUEST_DEPENDENCIES}
  fi      
fi

echo "Compiling"
echo "Environment: $(uname -a)"

annotate-output make CPUS=${JOBS} \
		PKGS="racket-test db-test unstable-flonum-lib net-test" \
		CONFIGURE_ARGS_qq="${RACKET_CONFIGURE_ARGS}" \
		${MAKE_TARGET}
     
echo "Running tests"
echo "Environment: $(uname -a)"

export PATH=${BUILD_DIR}/racket/bin:$PATH
command -v racket
racket -v
annotate-output raco test -l tests/racket/test
annotate-output racket -l tests/racket/contract/all
annotate-output raco test -l tests/json/json
annotate-output raco test -l tests/file/main
annotate-output raco test -l tests/net/head
annotate-output raco test -l tests/net/uri-codec
annotate-output raco test -l tests/net/url
annotate-output raco test -l tests/net/url-port
annotate-output raco test -l tests/net/encoders
annotate-output raco test -l tests/openssl/basic
annotate-output raco test -l tests/openssl/https
annotate-output raco test -l tests/match/main
annotate-output raco test -l tests/zo-path
annotate-output raco test -l tests/xml/test
annotate-output raco test -l tests/db/all-tests
annotate-output raco test -c tests/stxparse

echo "DONE!"
