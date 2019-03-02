This is the source code distribution for minimal Racket.

If this directory is part of a clone of the Git repository for Racket,
then the clone's root directory includes a makefile to both build
minimal Racket and install packages. See "INSTALL.txt" in the clone's
root directory.

========================================================================
 License and external links
========================================================================

Racket is distributed under the GNU Lesser General Public License
(LGPL). See the file "COPYING_LESSER.txt" for more information.

Compiled executables, documentation, and up-to-date information:
   http://racket-lang.org/

Pre-compiled daily snapshots:
   http://pre.racket-lang.org/

Main development repository:
   https://github.com/racket/racket

Report bugs:
   https://github.com/racket/racket/issues


========================================================================
 Traditional Racket versus Racket-on-Chez
========================================================================

This source directory contains implementations for two different
versions of Racket: the traditional implementation that is
substantially implemented in C, and an implementation that builds on
Chez Scheme.

Traditional Racket
------------------

By default, `configure` and the Windows scripts build the traditional
implementation of Racket.

If you need more information specific to the traditional
implementation of Racket, see "racket/README.txt".

Racket on Chez Scheme
---------------------

To build Racket-on-Chez on Unix variants or Mac OS:

 * ... in addition to the traditional variant of Racket: supply
   `--enable-cs` or `--enable-csdefault` to `configure`.

   The generated Racket-on-Chez executables will have a "cs" suffix.

 * ... by itself: supply `--enable-csonly` to `configure`.
 
   The generated Racket-on-Chez executables will *not* have a "cs"
   suffix.

Chez Scheme is not included with Racket sources, but building
Racket-on-Chez requires either a "ChezScheme" build checkout within
the build directory or at at an alternate location specified by the
`--enable-scheme=...` argument to `configure`.

For now, use the fork of Chez Scheme at

    https://github.com/mflatt/ChezScheme

We hope to eventually return to the current development version from

    https://github.com/cisco/ChezScheme

To build Racket-on-Chez on Windows, see See "worksp\README.txt" for
information.

If you need more information specific to Racket-on-Chez, see
"cs/README.txt".


========================================================================
 Compiling for supported Unix variants (including Linux)
========================================================================

Quick instructions:

 From this directory (where the `configure` file is), run the following
 commands:

   mkdir build
   cd build
   ../configure
   make
   make install

 Those commands will create an in-place installation of Racket and
 store the results of C compilation in a separate "build"
 subdirectory, which is useful if you need to update your sources,
 delete the build, and start from scratch.

 You can also run the typical `./configure && make && make install` if
 you don't anticipate updating/rebuilding, but it will be harder to
 restart from scratch should you need to.

 Some build modes may require GNU `make`. For example, when building
 the traditional Racket implementation, the content of the "foreign"
 subdirectory requires GNU `make` if no installed "libffi" is
 detected. If the build fails with another variant of `make`, please
 try using GNU `make`.

Detailed instructions:

 0. If you have an old Racket installation in the target directory,
    remove it (unless you are using an "in-place" build from a
    repository as described below).

    On Unix variants other than Mac OS, to run `racket/draw` and
    `racket/gui` programs, you will not only need the packages that
    supply those libraries, you'll need Cairo, Pango, and GTk
    installed. These libraries are not distributed with Racket, and
    they are not needed for compilation, except for building
    documentation that uses `racket/draw`. More info about required
    libs is available at http://docs.racket-lang.org/draw/libs.html
    and http://docs.racket-lang.org/gui/libs.html.

 1. Select (or create) a build directory.

    It's better to run the build in a directory other than the one
    containing `configure`, especially if you're getting sources via
    git. A common way to start a git-based build is:

        cd [here]
        mkdir build
        cd build

    where "[here]" is the directory containing this "README.txt" file
    and the `configure` script. The Git repository is configured to
    support this convention by ignoring `build` in this directory.

    A separate build directory is better in case the makefile
    organization changes, or in case the makefiles lack some
    dependencies. In those cases, when using a "build" subdirectory,
    you can just delete and re-create "build" without mangling your
    source tree.

 2. From your build directory, run the script `configure` (which is in
    the same directory as this README), with optional command-line
    arguments `--prefix=TARGETDIR` or `--enable-shared` (or both).

    For example, if you want to install into "/usr/local/racket" using
    dynamic libraries, then run:

      [here]configure --prefix=/usr/local/racket --enable-shared

    Again, "[here]" is the directory path containing the `configure`
    script. If you follow the convention of running from a "build"
    subdirectory, "[here]" is just "../". If you build from the
    current directory, "[here]" is possibly unnecessary, or possibly
    just "./", depending on your shell and PATH setting.

    If the `--prefix` flag is omitted, the executables are built for
    an in-place installation (i.e., the parent of the directory
    containing this README will be used directly). Unless
    `--enable-shared` is used, the "racket" directory can be moved
    later; most system administrators would recommend that you use
    `--enable-shared`, but the Racket developers distribute
    executables built without `--enable-shared`.

    The `configure` script generates the makefiles for building Racket
    and/or GRacket. The current directory at the time `configure` is
    run will be used as working space for building the executables
    (independent of `--prefix`). This build directory does not have to
    be in the source tree, even for an in-place build. It's ok to run
    `configure` from its own directory (as in the first example
    above), but it's better to pick a separate build directory that is
    otherwise empty (as in the second example).

    The `configure` script accepts many other flags that adjust the
    build process. Run `configure --help` for more information. In
    addition, a specific compiler can be selected through environment
    variables. For example, to select the SGI compilers for Irix
    instead of gcc, run configure as

         env CC=cc CXX=CC [here]configure

    To add an include path, be sure to use CPPFLAGS="-I..." instead of
    CFLAGS="-I...". The CPPFLAGS variable controls C pre-processing,
    which includes C compilation, and the Racket build normally uses
    the C pre-processor directly for some parts of the build.

    If you re-run `configure` after running `make`, then products of
    the `make` may be incorrect due to changes in the compiler command
    line. To be safe, run `make clean' each time after running
    `configure`. To be even safer, run `configure` in a fresh build
    directory every time.

    When building for multiple platforms or configurations out of the
    same source directory, beware of cached `configure` information in
    "config.cache". Avoid this problem entirely by using a separate
    build directory (but the same source) for each platform or
    configuration.

 3. Run `make`.  [As noted above, this might need to be GNU `make`.]

    Executables and libraries are placed in subdirectories of the
    build directory. For example, the `racket3m` executable appears in
    the "racket" directory.

    You can run executables in-place before `make install`, but if you
    haven't yet built ".zo" bytecode files from Racket sources in
    "../collects", startup will be very slow.

 4. Run `make install`.

    This step copies executables and libraries into place within the
    target installation. For example, the "racket" executable is
    copied into the "bin" directory for an in-place build, or into the
    executable directory for a `--prefix` build.

    For a `--prefix` build, this step also creates a "config.rktd"
    module in an "etc" directory, so that various Racket tools and
    libraries can find the installation directories. At this stage, in
    case you are packaging an installation instead of installing
    directly, you can redirect the installation by setting the
    "DESTDIR" environment variable to an absolute path for the
    packaging area. For example, `make DESTDIR=/tmp/racket-build
    install` places the installation into "/tmp/racket-build" instead
    of the location originally specified with `--prefix`. The
    resulting installation will not work, however, until it is moved
    to the location originally specified with `--prefix`.

    Finally, the `make install` step compiles ".zo" bytecode files for
    installed Racket source, generates launcher programs like DrRacket
    (if it's already installed as a package), and builds documentation
    (again, if installed). Use `make plain-install` to install without
    compiling ".zo" files, creating launchers, or building
    documentation.

    If the installation fails because the target directory cannot be
    created, or because the target directory is not the one you want,
    then you can try repeating step 4 after running `configure` again
    with a new `--prefix` value.  That is, sometimes it is not necessary
    to repeat step 3 (so try it and find out).  On other platforms and
    configurations, it is necessary to start with a clean build
    directory when changing the `--prefix` value, because the path gets
    wired into shared objects.

    If you build frequently from the Git-based sources, beware that you
    may accumulate user- and version-specific information in your
    "add-ons" directory, which you can most easily find by evaluating
      (find-system-path 'addon-dir)
    in Racket.  In addition, if you configure with `--enabled-shared`,
    you may accumulate many unused versions of the dynamic libraries in
    your installation target.

After an "in-place" install from a source distribution, the
"racket/src" directory is no longer needed, and it can be safely
deleted. Build information is recorded in a "buildinfo" file in the
installation.

For a build without `--prefix` (or with `--enable-origtree`) and
without `--enable-shared`, you can safely move the install tree,
because all file references within the installation are relative.


========================================================================
 Compiling for Mac OS
========================================================================

First, install Xcode and command-line tools from Apple.

After installing developer tools, follow the Unix instructions above,
but note the following:

 * The Racket build creates a framework, "Racket.framework", which is
   installed into "racket/lib".  This framework is used by the `racket`
   executable that goes into "racket/bin".

 * The GRacket build creates a GUI-executable variant of the Racket
   executable. The GRacket build process also downloads (from github)
   pre-built libraries for Cairo, Pango, etc.

 * The `--enable-shared` flag for `configure` must not be used,
   because builds create and use frameworks by default. Furthermore,
   `--disable-shared` is not supported. (Unless you use
   `--enable-xonx`...)

 * To build an X11- and Gtk-based GRacket, run `configure` with the
   `--enable-xonx` flag. Frameworks are not used for such builds, so
   `--enable-shared` is allowed. The `--enable-xonx` flag also affects
   the Racket build, so that `system-type` reports 'unix. Pre-built
   libraries are not downloaded in this mode; you must have Cairo,
   Pango, and GTk installed.

 * To use `--prefix` without `--enable-xonx`, you must also supply
   `--enable-macprefix`. BEWARE! The directory structure for a
   non-xonx build does not fit a typical Unix directory structure. For
   example, frameworks are written directly to a "lib" subdirectory,
   and executables like "GRacket.app" are written directly to the
   prefix directory. (Requiring `--enable-macprefix` with `--prefix`
   for a non-xonx build helps prevent accidental installation of a
   Mac-style directory structure on top of an existing Unix-style
   directory structure.)

 * On Mac OS 10.6 and later, to build Racket in 32-bit mode, use
   `--disable-mac64`.


========================================================================
 Compiling for Windows
========================================================================

To compile with Microsoft Visual C, see the instructions in
"racket\src\worksp\README.txt".

To compile with MinGW tools, follow the Unix instructions above; do
not use `--enable-shared`, because DLLs will be generated
automatically. The result is a Windows-style build. If you are using a
variant of MinGW without "libdelayimp.a", get the implementation of
"delayimp.c" from MinGW-w64 and compile it to "libdelayimp.a".


========================================================================
 Cross-compiling
========================================================================

Cross-compilation requires at least two flags to `configure`:

 * `--host=OS`, where OS is something like `i386-gnu-linux` to
   indicate the target platform.

   The `configure` script uses OS to find suitable compilation tools,
   such as `OS-gcc` and `OS-strip`.

 * `--enable-racket=RACKET`, where RACKET is a path to a Racket
   executable that runs on the build platform; the executable must be
   the same version of Racket and the same virtual machine (i.e.,
   traditional Racket or Racket on Chez Scheme) as being built for the
   target platform.

   This flag is needed because building and installing Racket requires
   running (an existing build of) Racket.

   Use "auto" for RACKET to indicate that Racket should be built
   automatically for the current platform. In that case, `make` will
   run `configure` again (with no arguments) in a "local" subdirectory
   to create a build for the current platform.

For Racket-on-Chez, an additional flag is needed:

 * `--enable-scheme=SCHEME`, where SCHEME is a path to a Chez Scheme
   build directory. Chez Scheme must be built there already for the
   current platform, and a cross-compiled Chez Scheme will be created
   in the same directory.

Some less commonly needed `configure` flags for traditional Racket:

 * `--enable-stackup`, if the target platform`s stack grows up.

 * `--enable-bigendian`, if target platform is big-endian.

 * `--enable-cify` or `--disable-cify` if the JIT availablity on the
    target platform is different than the build platform; use
    `--enable-cify` if the JIT is not abailable on the target
    platform.


========================================================================
 Cross-compiling for Android
========================================================================

[Currently, cross-compilation for Android works only for the
 traditional Racket implementation.]

As an example of cross-compiling, to compile for Android on ARM using
the NDK, use (all on one line)

  configure --host=arm-linux-androideabi 
            --enable-sysroot="[ndk]/platforms/android-[N]/arch-arm"
            --enable-racket=auto

where [ndk] is the path to the installed NDK, [N] is a target version
of Android (such as 14), and

 [ndk]/toolchains/arm-linux-androideabi-[comp]/prebuilt/[platform]/bin

is in your PATH (so that a suitable `gcc`, `ar`, etc., are found) for
the [comp] of your choice and the [platform] used to compile.


========================================================================
 Cross-compiling for iOS
========================================================================

[Currently, cross-compilation works only for the traditional Racket
 implementation.]

To compile the Racket runtime system as a Framework for iOS, use (all
on one line)

  configure --host=[arch]-apple-darwin
            --enable-ios="[sdk]"
            --enable-racket=racket

where [arch] is one of

 - armv7, armv7s, or aarch64: to run on iOS
 - i386 or x86_64: to run on the simulator

The [sdk] argument is a path to an iOS SDK, but if it is "iPhoneOS" or
"iPhoneSimulator", then the corresponding SDK is located in the
standard place within the XCode application. For example, "iPhoneOS"
becomes the path (all on one line)

  /Applications/Xcode.app/Contents/Developer/Platforms/
    iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk


========================================================================
 Test Suite
========================================================================

Tests for the core Racket implementation are in a "racket-test-core"
package. The tests are developed in the same Git repository as the
core Racket implementation in a "pkgs" directory at the repository
root.


========================================================================
 Implementation Organization
========================================================================

Everything in this "src" directory contributes to the implementation
of the `racket` executable (and variants), while "../collects"
contains the additional Racket libraries that are included in a
minimal Racket distribution.

Sources for the traditional Racket implementation
-------------------------------------------------

 * "racket" --- traditional `racket` executable

   This implementation can build from "scratch" with a C compiler, but
   first by building a CGC variant of Racket to transform the C
   sourses to build a (normal) 3m variant.

 * "gracket" --- traditional `gracket` executable

 * "foreign" --- FFI implementation for "racket"

   This directory includes a copy of "libffi" (as needed for some
   platforms).

 * "mzcom" --- MzCOM executable (for Windows)

 * "mysink" --- `ffi/unsafe/com` helper DLL (for Windows)

See also the shared sources below, which includes rktio and the macro
expander.

Sources for the Racket-on-Chez implementation
---------------------------------------------

 * "cs" --- Racket-on-Chez `racket` executable

   Building requires both an existing Racket (possibly created from
   the "racket" sources) and an existing Chez Scheme build.

 * "thread" --- thread scheduler

 * "io" --- I/O

    This layer uses the "racketio" library to access OS facilties.

 * "regexp" --- regexp matcher

See also the shared sources below, which includes rktio, the macro
expander, and schemify.

Sources shared by both Racket implementations
---------------------------------------------

 * "expander" --- macro expander implementation

   This expander is both included in Racket executables and used to
   expand itself for inclusion in executables. It's also used to
   expand other libraries for inclusion in Racket-on-Chez executables.

   If you change the expander, run `make` in its directory to generate
   the "startup.inc" file that holds the expander's implementation for
   the traditional Racket variant. The Racket-on-Chez build (which
   needs an existing Racket to build, anyway) picks up changes to
   "expander" automatically.

 * "rktio" --- portability layer for low-level I/O

   If you change "rktio.h", then be sure to regenerate "rktio.rktl"
   and "rktio.inc" using an existing Racket implementation that has
   the "parser-tools" package installed.

 * "schemify" --- a Racket-to-Scheme compiler, used by "cs" and "cify"

        Similar to "expander", this layer is applied to itself and
        other libraries for inclusion in "cs".


 * "cify" --- a Racket-to-C compiler

   This compiler is used only when embedding the expander as C code,
   instead of Racket bytecode, which is the default for platforms
   where the traditional Racket JIT is not supported.

 * "start" --- main-executable wrapper

   Startup wrappers used by both the tradition and Racket-on-CS
   implementations. 

 * "worksp" --- Windows projects and build scripts

 * "mac" --- scripts for generating Mac OS ".app"s

 * "setup-go.rkt" --- helper script

   The "setup-go.rkt" script is a bootstrapping too that is used by
   parts of the build that need to run Racket programs in the process
   of building Racket.

More shared utilities
---------------------

 * "native-libs" --- build scripts for some native-library packages

   These script are not used when building Racket, but they are used
   to build some separately distributed packages.

 * "common" --- Racket libraries used by "thread", "io", etc.

 * "ac" --- `autoconf` helpers

 * "lt" --- `libtool` and `configure` support

 * "utils" --- miscellaneous

