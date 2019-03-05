#! /bin/bash
set -e

# ---------------------------------------------------------------------------------------------------
# Script called by jobs in .gitlab-ci.yml to build and test racket,
# possibly in a cross environment.

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
    exit 1
}

DEBIAN=
DEBIAN_MIRROR=
JOBS=
RACKET_CONFIGURE_ARGS=
ARCH="$(uname -m)"
BUILD_DIR=${CI_PROJECT_PATH}
MAKE_TARGET="in-place"

# Parse options
until
    opt=$1
    case ${opt} in
	--jobs)
	    shift
	    JOBS=$1
	    ;;
	--single-thread)
	    JOBS=$1
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

CHROOT_DIR=/tmp/racket-chroot

# ---------------------------------------------------------------------------------------------------
# Packages to install on the HOST
HOST_DEPENDENCIES="debootstrap qemu-user-static binfmt-support sbuild"

# Packages to install on the GUEST
GUEST_DEPENDENCIES="build-essential git m4 sudo python"

function setup_chroot {
    # Host dependencies
    apt-get install -y ${HOST_DEPENDENCIES}

    # Create chrooted environment
    mkdir ${CHROOT_DIR}
    debootstrap --foreign --no-check-gpg --include=fakeroot,build-essential \
		--arch=${CHROOT_ARCH} ${VERSION} ${CHROOT_DIR} ${MIRROR}
    cp /usr/bin/qemu-${ARCH}-static ${CHROOT_DIR}/usr/bin/
    chroot ${CHROOT_DIR} ./debootstrap/debootstrap --second-stage
    sbuild-createchroot --arch=${CHROOT_ARCH} --foreign --setup-only \
			${VERSION} ${CHROOT_DIR} ${MIRROR}

    # Create file with environment variables which will be used inside chrooted
    # environment
    echo "export ARCH=${ARCH}" > envvars.sh
    echo "export BUILD_DIR=${BUILD_DIR}" >> envvars.sh
    chmod a+x envvars.sh

    # Install dependencies inside chroot
    chroot ${CHROOT_DIR} apt-get update
    chroot ${CHROOT_DIR} apt-get --allow-unauthenticated install \
           -y ${GUEST_DEPENDENCIES}

    # Create build dir and copy travis build files to our chroot environment
    mkdir -p ${CHROOT_DIR}/${BUILD_DIR}
    rsync -av ${BUILD_DIR}/ ${CHROOT_DIR}/${BUILD_DIR}/

    # Indicate chroot environment has been set up
    touch ${CHROOT_DIR}/.chroot_is_done

    # Call ourselves again which will cause tests to run
    chroot ${CHROOT_DIR} bash -c "cd ${BUILD_DIR} && ./.gitlab/build-test.sh"
}

echo "Building for arch ${ARCH}"

if [ -e "/.chroot_is_done" ]; then
  # We are inside chroot
  echo "Running inside chrooted environment"
  . ./envvars.sh
else
  if [ "${ARCH}" != "$(uname -m)" ]; then
    # test run, need to set up chrooted environment first
    echo "Setting up chrooted ${ARCH} environment"
    setup_chroot
  fi
fi

echo "Compiling"
echo "Environment: $(uname -a)"

make CPUS=${JOBS} \
     PKGS="racket-test db-test unstable-flonum-lib net-test" \
     CONFIGURE_ARGS_qq="${RACKET_CONFIGURE_ARGS}" \
     ${MAKE_TARGET}
     
echo "Running tests"
echo "Environment: $(uname -a)"

which racket
racket -v
raco test -l tests/racket/test
racket -l tests/racket/contract/all
raco test -l tests/json/json
raco test -l tests/file/main
raco test -l tests/net/head
raco test -l tests/net/uri-codec
raco test -l tests/net/url
raco test -l tests/net/url-port
raco test -l tests/net/encoders
raco test -l tests/openssl/basic
raco test -l tests/openssl/https
raco test -l tests/match/main
raco test -l tests/zo-path
raco test -l tests/xml/test
raco test -l tests/db/all-tests
raco test -c tests/stxparse

echo "DONE!"
