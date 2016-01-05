The core `racket' executable has minimal library dependencies. In
contrast, libraries implemented in various packages, such as the
"draw" or "math" packages, rely on additional C-implemented libraries,
such as Cairo, GMP, etc., all of which are loaded dynamically. On Unix
variants, we expect users to install C-implemented libraries (usually
through the operating system's package manager). For Windows and Mac
OS X, we supply pre-built libraries in platform-specific packages; the
corresponding Racket packages include platform-specific dependencies
on those packages. The "x86_64-linux-natipkg" variant of Racket
is like Windows and Mac OS X, expecting packages to supply native
libraries for 64-bit Linux.

This directory contains scripts and patches to build Windows, Mac OS
X, and Linux libraries in a consistent and portable way. Naturally,
the script and patches are fragile, so we upgrade libraries
infrequently. Currently, we use the following external packages and
versions:

 pkg-config-0.28
 sed-4.2 (Windows only, to avoid non-GNU `sed')
 sqlite[-autoconf]-3080600 (Windows and Linux only)
 openssl-1.0.1g (Windows and Linux only)
 libiconv-1.14 (Windows only)
 zlib-1.2.8 (Windows and Linux only)
 libffi-3.0.13
 expat-2.1.0
 gettext-0.18.3.2
 glib-2.39.92
 libpng-1.6.10
 pixman-0.32.4
 cairo-1.12.16
 jpegsrc.v9a
 harfbuzz-0.9.27
 fontconfig-2.11.1
 freetype-2.5.3
 pango-1.36.6 (current PPC binaries are 1.36.3)
 poppler-0.24.5
 mpfr-3.1.2
 gmp-5.1.3

(Linux only:)
 xtrans-1.3.5
 inputproto-2.3.1
 xextproto-7.3.0
 kbproto-1.0.5
 xproto-7.0.26
 xcb-proto-1.11
 renderproto-0.11.1
 libpthread-stubs-0.3
 libxcb-1.11
 libXau-1.0.8
 libX11-1.6.2
 libXext-1.3.3
 libXrender-0.9.8
 freefont[-ttf]-20100919
 atk-2.12.0
 gdk-pixbuf-2.30.8
 gtk+-2.24.24

See "../mac/README.txt" for information about an additional
library on Mac OS X.

Preliminiaries
--------------

For Windows (cross-compile from Mac OS X or Linux):

The build scripts assume a MinGW cross compiler installed in
"/usr/mw32" (for 32-bit builds) and "/usr/mw64" (for 64-bit builds).
In addition, building "glib" requires "gettext" executables that run
on the build machine in your PATH.

Beware that the "libdir" configuration in
  /usr/mw{32,64}/{i686,x86_64}-w64-mingw32/lib/libstdc++.la
may be wrong, in which case you'll need to fix it by hand.

For Mac OS X (i386 and x86_64 on Intel, ppc on PowerPC):

The script assumes that "/Developer/SDKs/MacOSX10.5.sdk" (for 32-bit
builds) and "/Developer/SDKs/MacOSX10.6.sdk" (for 64-bit builds) are
available.

You can get the 10.5 SDK out of the ".dmg" for Xcode 3.2.6; mount it
(don't run it), open -R "MacOSX10.5.pkg", and right click to run.
Probably you can get the 10.6 SDK in a similar way.

If you wanted to build for 10.4, and if you're using gcc instead of
Clang, then note that you'll need gcc-4.0 --- but the Pango version
listed above relies on CoreText, which is available only with 10.5 and
later.

For Linux:

The script assumes that `gcc`, `g++`, `m4`, and `chrpath` are
installed, as well as X11 header files.

Build Steps (assuming no version changes)
-----------

 * Download .tar.{gz,bz2,xz} archives for the above packages. Stash
   them in some directory, <archive-dir>

 * Create a working directory, <build-dir>, and make it the current
   directory.

 * Run

     racket <here-dir>/build-all.rkt \
        --{win,mac,linux} \
        --m{32,64} \
        --archives <archive-dir>

    where <here-dir> is the deirectory containing this file,
    `--win' versus `--mac' selects a Windows versus Mac OS X build,
    and `--m32' versus `--m64' selects a 32-bit versus 64-bit build.

 * Run

     racket <here-dir>/install.rkt \
        --{win,mac,linux} \
        --m{32,64} \
        <native-pkgs-dir>

   where <native-pkgs-dir> contains the package "source" directories,
   such as "draw-win32-i386". The <native-pkgs-dir> is normally a
   checkout of "https://github.com/racket/libs.git".

Details
-------

The "build-all.rkt" script runs "build.rkt" for each external package
that needs to be built for the specified platform. The "build.rkt"
script encodes suitable environment and flag settings for building
native libraries.

The first build step is "pkg-config", which is built for the current
build platform, as opposed to the target platform. The resulting
`pkg-config` is installed into "<build-dir>/dest/bin", which is
included in the PATH environment variable when all other configuration
steps are run. If you used an installed `pkg-config', then you'd end
up linking to installed packages on the build machine, which would be
confusing at best.

More details for Windows:

 * GNU `sed' is built to run on the build platform, just in case the
   build platform's `sed' is BSD-style (as on Mac OS X).

 * The generated ".dll"s go to "dest/bin".

 * The "install.rkt" script finalizes the build by stripping debugging
   symbols.

 * Beware of dynamic linking to libgcc or libstdc++. The build script
   uses `-static-libgcc' and `-static-libstdc++' to statically link
   those libraries. Use "depends.exe" to check DLL dependencies.

More details for Mac OS X:

 * 32-bit binaries are built for 10.5 and up. 64-bit binaries are
   built for 10.6 and up.

 * The generated ".dylib"s go to "dest/lib".

 * The "install.rkt" script finalizes the build by stripping debugging
   symbols and adjusting cross-library references to be relative
   paths.

   During the build, ".dylib"s in "<build-dir>/dest/lib" will contain
   full paths when they depend on other ".dylibs" in the same
   directory. The "install.rkt" script uses `install_name_tool' to
   rewrite those paths to relative form using "@loader_path".

   You can use

        otool -L <name>.dylib

   to check that "<name>.dylib" does not refer to any other library
   through an absolute path (i.e., your <build-dir> path). Also, watch
   out for "/usr/opt/local" paths, which means that you have
   accidentally links to MacPorts libraries.

 * All ".dylib"s should use two-level namespaces. Use `otool -vh' and
   look for "TWOLEVEL" in the output to double check that a library
   build uses two-level namespaces.

More details for Linux:

 * The `chrpath` tool is used to set the RAPTH of each generated
   library to `$ORIGIN` so that other shared libraries will be found
   when they are installed in the same package scope.

 * A minimal set of fonts is installed in the installation's "share"
   directory under "fonts". The Fontconfig package is patched to
   redirect the default configuration location to that directory.

When Library Versions Change
----------------------------

An external package such as "libffi-3.0.13" generates a library with a
name like "libffi.6.dylib". Upgrading a library may change the version
on the generated ".dll" or ".dylib". In that case, you must update
several places:

 * Update the library version in "install.rkt".

 * Update the `ffi-lib' reference in the corresponding Racket wrapper
   libraries.

 * Update the "info.rkt" dependencies in the Racket packages that
   contain changed Racket wrappers, because a new ".dll" or ".dylib"
   version will necessitate a new Racket package version (as a suffix
   on the package name).

When Things Don't Work
----------------------

You may have to change "build-all.rkt" and "build.rkt", especially if
you're trying to upgrade external libraries. On upgrade, patches will
fail, new dependencies will be introduced, and so on. In particular,
the configuration flags and patches that we use are described within
"build.rkt".

You can run "build.rkt" directly to work on problems with an
individual external package. The "build-all.rkt" detects that an
external package <package-name> has been built though the existence of
the file

   <build-dir>/dest/stamps/<package-name>

so delete that file to make "build-all.rkt" try again for
<package-name>.

If You Have to Start Over Completely
------------------------------------

See 

 * "racket/src/mac/README.txt" in a Racket v5.x source distirbution

 * https://github.com/soegaard/racket-osx-libs

 * http://cairographics.org/end_to_end_build_for_mac_os_x/

 * Pre-built packages from www.gtk.org, specifically the "dev"
   archives.

for old build notes. The "build.rkt" script automates most everything
we learned.
