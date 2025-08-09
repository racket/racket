This directory has the source code for the `racket` executable.

If this directory is part of a Racket source code distribution, then
the distribution may include additional packages. Those extra packages
will be installed and built with the `make install` step.

If this directory is a checkout of the Git repository for Racket, then
the checkout's root directory includes a makefile to both build
minimal Racket and install packages, and you should start there,
instead of here. See "build.md" in the checkout's root directory.

========================================================================
 License and external links
========================================================================

Racket is distributed under the MIT license and the Apache version 2.0
license, at your option. See "LICENSE.txt" for more information.

Compiled executables, documentation, and up-to-date information:
   http://racket-lang.org/

Pre-compiled daily snapshots:
   http://snapshot.racket-lang.org/

Main development repository:
   https://github.com/racket/racket

Report bugs:
   https://github.com/racket/racket/issues


========================================================================
 Racket CS (Chez Scheme) versus Racket BC (ByteCode / Before Chez)
========================================================================

This source directory contains implementations for two different
versions of Racket: the CS implementation that is implemented in Chez
Scheme and Racket (compiled to Chez Scheme), and the original BC
implementation that is substantially implemented in C.

Racket CS
---------

By default, `configure` (for Unix) or `winfig.bat` (for Windows)
prepares a build for the CS implementation of Racket. Chez Scheme is
included in Racket source distributions and the source repository, and
it will be compiled as part of the build.

For more information specific to Racket CS, see "cs/README.txt".

Racket BC
---------

To build Racket BC on Unix variants or Mac OS:

 * ... in addition to Racket CS: supply `--enable-cs --enable-bc` to
   `configure` or supply `/both` to `winfig.bat`.

   The generated Racket BC executables will have a "bc" suffix. A
   plain `make` will still build Racket CS; use `make bc` to build and
   `make install-bc` to install.

 * ... by itself: supply `--enable-bcdefault` to `configure` or
   `/bconly` to `winfig.bat`.
 
   The generated Racket BC executables will *not* have a "bc" suffix.

If you need more information specific to Racket BC, see
"bc/README.txt".


========================================================================
 Compiling for supported Unix variants (including Linux)
========================================================================

Quick instructions:

 From this directory (where the `configure` file is), run the
 following commands:

   # see extra commands below when building within a Git checkout

   mkdir build
   cd build
   ../configure
   make
   make install

 Those commands will create an in-place installation of Racket and
 store the results of intermediate compilation in a separate "build"
 subdirectory, which is useful if you need to update your sources,
 delete the build, and start from scratch.

 You can also run the typical `./configure && make && make install` if
 you don't anticipate updating/rebuilding, but it will be harder to
 restart from scratch should you need to.

 Some build modes may require GNU Make. See "Dependency details" below
 for more information about dependencies.

 When working from a Git checkout of the Racket repository (as opposed
 to a source code distribution), and when you do not want to use the
 makefiles in the checkout's root directory, then prefix the above
 commands with
  
   make --directory=../../ pb-fetch

 and add a command to run the installed `raco` to the end:

   raco pkg install -i racket-lib

Detailed instructions:

 0. If you have an old Racket installation in the target directory,
    remove it (unless you are using an "in-place" build as described
    below).

    When building from a Git checkout of the Racket repository (as
    opposed to a source code distribution), note that "../../build.md"
    has more information about build options in that mode. If you work
    from here, though, an extra step is needed to download or update
    sources in "ChezScheme/boot/pb" (which are already present in a
    source code distribution):

       make --directory=../../ pb-fetch

 1. Select (or create) a build directory.

    It's better to run the build in a directory other than the one
    containing `configure`, especially if you're getting sources via
    Git. A common way to start a Git-based build is:

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
    arguments like `--prefix=TARGETDIR`.

    For example, if you want to install into "/usr/local/racket", then
    run:

      [here]configure --prefix=/usr/local/racket

    Again, "[here]" is the directory path containing the `configure`
    script. If you follow the convention of running from a "build"
    subdirectory, "[here]" is just "../". If you build from the
    current directory, "[here]" is possibly unnecessary, or possibly
    just "./", depending on your shell and PATH setting.

    If the `--prefix` flag is omitted and if directories like `bindir`
    and `libdir` appear to be the default paths or the
    `--enable-origtree` flag is specified, then executables are built
    for an in-place installation (i.e., the parent of the directory
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
    variables. For example, to select the SGI compiler for Irix
    instead of gcc, run configure as

         env CC=cc [here]configure

    Use CFLAGS+=.... to add to CFLAGS instead of replacing the default,
    and `+=` also works for the variables CPPFLAGS, LDFLAGS, and LIBS.
    To add an include path, be sure to use CPPFLAGS+="-I..." instead of
    CFLAGS+="-I...". The CPPFLAGS variable controls C pre-processing,
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
    build directory. For example, the `racketcs` executable appears in
    the "cs/c" directory.

    You can run executables in-place before `make install`, but if you
    haven't yet built ".zo" bytecode files from Racket sources in
    "../collects", startup will be very slow.

    The first step of `make` is to build `bin/zuo` to run build
    scripts. If you need to select the C compiler to build `bin/zuo`
    (which is a single C file that needs only system headers), then
    supply `CC_FOR_BUILD=<compiler>` as an argument to `make`.

    For a `--prefix` build that is not a cross-compilation, this step
    also compiles ".zo" bytecode files for collections and packages,
    and it renders documentation; those prepared files are written in
    the build directory and moved into place by `make install`. For a
    build that does not use `--prefix` or is a cross compilation,
    building collections and packages is deferred to the `make
    install` step.

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
    `DESTDIR` makefile variable to an absolute path for the
    packaging area. For example, `make DESTDIR=/tmp/racket-build
    install` places the installation into "/tmp/racket-build" instead
    of the location originally specified with `--prefix`. The
    resulting installation will not work, however, until it is moved
    to the location originally specified with `--prefix`. When using
    `make` instead of `zuo`, `DESTDIR` can be set as an environment
    variable.

    Finally, the `make install` step compiles ".zo" bytecode files for
    installed Racket source, generates launcher programs like DrRacket
    (if it's already installed as a package), and builds documentation
    (again, if installed). Use `make plain-install` to install without
    compiling ".zo" files, creating launchers, or building
    documentation. Supplying `PLT_SETUP_OPTIONS` (allowed as an
    environment variable with `make`) sets flags that are passed on to
    `raco setup` by `make install`.

    For a `--prefix` build, unless `--enable-sharezo` is specified,
    "compiled" directories containing ".zo" files are moved from
    "share" to "lib" as the last step of installation. (The
    "config.rktd" file is updated so that `current-compile-file-roots`
    is initialized to find the relocated ".zo" files.) For Racket BC,
    ".zo" files are architecture-independent, and `--enable-sharezo`
    was the default installation mode through Racket version 8.0. To
    prepare additional packages (i.e., package that are not included
    with the source distribution) in installation scope without
    `--enable-sharezo`, then it's easiest to first install in-place,
    then configure and install again for a `--prefix` build; that way,
    the packages installed in-place will get carried along, and their
    "compiled" directories will be moved appropriately.

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
    in Racket. In addition, on Mac OS or if you configure with
    `--enabled-shared` for Racket BC, you may accumulate many unused
    versions of the dynamic libraries in your installation target.

 5. After an "in-place" install from a source distribution, the
    "racket/src" directory is no longer needed, and it can be safely
    deleted. Build information is recorded in a "buildinfo" file in
    the installation.

    For a build without `--prefix` (or with `--enable-origtree`) and
    without `--enable-shared`, you can safely move the install tree,
    because all file references within the installation are relative.

 6. When building from a Git checkout (as opposed to a source code
    distribution), after `make install`, the Racket installation is
    still more "minimal" than a "Minimal Racket" distribution, because
    it does not have the "racket-lib" package installed. Consider
    adding that package with

       raco pkg install -i racket-lib

Dependency details:

 Mostly, you need a C compiler and `make`.

 Some build modes may require GNU Make. For example, when building the
 Racket CS implementation, GNU Make is required when the bundled
 liblz4 is built. When building the Racket BC implementation, the
 content of the "foreign" subdirectory requires GNU Make if no
 installed libffi is detected. If the build fails with another variant
 of `make`, please try using GNU Make.

 To build Racket CS on a platform where Chez Scheme does not have a
 native-code backend, libffi must be installed. (The bundled version
 that is used by Racket BC is not currently built for Racket CS.)

 Additional recommended libraries for Unix (not Mac OS) platforms:

  * libffi (Racket BC): optional, and a version of libffi is bundled
    with Racket sources, but an installed libffi is preferred.

  * libncurses (Racket BC and CS): optional, but expeditor support in
    command-line Racket is enabled only when libncurses is present at
    compile time.

  * libiconv (Racket BC and CS): optional, but without iconv,
    `open-bytes-converter` support is limited, and UTF-8 is assumed as
    the locale's encoding.

  * libz and liblz4 (Racket CS): optional, and built from bundled
    versions if not present.

 On Unix variants other than Mac OS, to run `racket/draw` and
 `racket/gui` programs, you will not only need the packages that
 supply those libraries, you'll need Cairo, Pango, and GTk installed.
 These libraries are not distributed with Racket, and they are not
 needed for compilation, except for building documentation that uses
 `racket/draw`. More info about required libs is available at
 http://docs.racket-lang.org/draw/libs.html and
 http://docs.racket-lang.org/gui/libs.html.


========================================================================
 Compiling for Mac OS
========================================================================

First, install Xcode and command-line tools from Apple.

After installing developer tools, follow the Unix instructions above,
but note the following:

 * If you are building from a source distribution (as opposed to a Git
   repository checkout), then beware that the distribution does not
   include native libraries that are needed on Mac OS; the install
   step will fetch and install missing Racket packages that supply
   those libraries. With a Git repository checkout (as opposed to a
   source code distribution), an extra step is needed as described in
   the Unix instructions: `raco pkg install -i racket-lib`.

 * The Racket build creates a framework, "Racket.framework", which is
   installed into "racket/lib".  This framework is used by the `racket`
   executable that goes into "racket/bin" unless the `--enable-embedfw`
   flag is used.

 * The GRacket part of the build creates a GUI-executable variant of
   the Racket executable. Pre-built libraries for Cairo, Pango, etc.,
   to support Racket drawing and GUI libraries are installed as Racket
   packages.

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


========================================================================
 Compiling for Windows
========================================================================

For information on setting up a command-line build environment with
Microsoft Visual Studio, see detailed instructions in
"worksp\README.txt". With the command-line environment set up, the
build steps are essentially the same as for Unix, but with
`winfig.bat` in place of `configure` and `nmake` in place of `make`:

   mkdir build
   cd build
   ..\winfig.bat
   nmake
   nmake install

To compile with MinGW tools using MSYS2, follow the Unix instructions
above; do not use `--enable-shared`, because DLLs will be generated
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

 * `--enable-racket=RACKET`, where RACKET is either "auto" or a path
   to a Racket executable that runs on the build platform; in the
   latter case, the executable must be the same version of Racket and
   the same virtual machine (i.e., CS or BC) as being built for the
   target platform.

   This flag is needed because building and installing Racket requires
   running (an existing build of) Racket.

   Use "auto" for RACKET to indicate that Racket should be built
   automatically for the current platform. In that case, `make` will
   run `configure` again (with no arguments) in a "local" subdirectory
   to create a build for the current platform.

For Racket CS, an additional flag is required when not using
`--enable-racket=auto`:

 * `--enable-scheme=SCHEME`, where SCHEME is a Chez Scheme executable
   that runs on the build platform; for cross-compilation, it must
   match the Chez Scheme version as used in the Racket being built.
 
   Supplying `--enable-scheme=DIR` is also supported, where DIR is a
   path that has a "ChezScheme" directory where Chez Scheme is built
   for the build system (but not necessarily installed).

The `--enable-racket=RACKET` and `--enable-scheme=SCHEME` flags are
allowed for non-cross builds, too:

 * For Racket CS, supplying either selects a Racket or Chez Scheme
   implementation used to create boot files to the build platform.
   Supplying Chez Scheme is a much more direct path. These executables
   do not need to match the versions being built in non-cross mode, as
   there are bootstrapping paths that can handle differences where
   needed. If you supply both Racket and Chez Scheme, then Racket is
   used (despite being less direct).

 * For Racket BC, `--enable-racket=RACKET` selects a Racket to
   prepare C sources to cooperate with garbage collection. Its version
   needs to be close to the one being built, and potentially exactly
   the same version.

Some less commonly needed `configure` flags are for Racket BC:

 * `--enable-stackup`, if the target platform`s stack grows up.

 * `--enable-bigendian`, if target platform is big-endian.

 * `--enable-cify` or `--disable-cify` if the JIT availability on the
    target platform is different than the build platform; use
    `--enable-cify` if the JIT is not available on the target
    platform.


========================================================================
 Cross-compiling for Android
========================================================================

As an example of cross-compiling Racket for Android on ARMv7 using the
NDK, use (all on one line)

  configure --host=arm-linux-androideabi 
            --enable-sysroot="[sysroot]"
            --enable-racket=auto

If you use the NDK script "make-standalone-toolchain.sh" to generate a
toolchain directory, then include that directory's "bin" in your PATH
(so that `arm-linux-androideabi-gcc`, etc., are found), and you can
omit `--enable-sysroot` (or specify [sysroot] as the toolchain
directory's "sysroot" subdirectory).

In other NDK configurations, you may have

 [ndk]/toolchains/arm-linux-androideabi-[comp]/prebuilt/[platform]/bin

in your PATH (so that `arm-linux-androideabi-gcc`, etc., are found)
where [ndk] is the path to the installed NDK and for the [comp] of
your choice and the [platform] used to compile, and then [sysroot] is

 [ndk]/platforms/android-[N]/arch-arm

where [N] is a target version of Android (such as 14).

For 64-bit ARM, replace "arm" above with "aarch64", and replace
"androideabi" with "android".

When building BC, you may need to add `--disable-cify` for 32-bit ARM
and `--enable-cify` for 64-bit ARM instead of inheriting the build
machine's disposition.


========================================================================
 Cross-compiling for iOS
========================================================================

To compile the Racket runtime system as a Framework for iOS, use (all
on one line)

  configure --host=[arch]-apple-darwin
            --enable-ios="[sdk]"
            --enable-racket=auto

where [arch] is one of

 - armv7, armv7s, or aarch64: to run on iOS
 - x86_64 or aarch64: to run on the simulator

The [sdk] argument is a path to an iOS SDK for "iPhoneOS" or
"iPhoneSimulator". The corresponding SDK is located in the standard
place within the XCode application. For example, "iPhoneOS" becomes
the path (all on one line)

  /Applications/Xcode.app/Contents/Developer/Platforms/
    iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk

To use an existing Racket build for the build platform using
`--enable-racket`, be sure to include `--enable-scheme` for Racket CS.
For example, if you have built Racket CS for your machine at
"/path/to/racket" using the default `make` target, you can configure
the cross build using that Racket binary and a path to the Chez Scheme
build folder as follows (all on one line)

  configure --host=[arch]-apple-darwin
            --enable-ios="[sdk]"
            --enable-racket=/path/to/racket/bin/racket
            --enable-scheme=/path/to/racket/src/build/cs/c

Currently, iOS enforces W^X protection on memory pages, which is
technically a problem for Racket CS. See the note about "writes to
`space_code` memory" in "ChezScheme/c/segment.c" for the implications.
If you avoid passing newly-allocated code between threads and avoid
`#:blocking?` foreign callbacks, you might not run into any issues.

When building BC for iOS, you may need to add `--disable-cify` for
32-bit target and `--enable-cify` for 64-bit target instead of
inheriting the build machine's disposition.


========================================================================
 Compiling without run-time code generation
========================================================================

Racket programs and expressions are normally compiled to machine code
either at run time (when `eval` is used or when Racket BC JIT-compiles
bytecode) or in advance (when compiling to ".zo" files using Racket
CS). Interpreted modes are available --- but slower, of course:

 * Racket CS: configure with `--enable-pb`, which uses a bytecode
   virtual machine instead of native code. By default, core functions
   are compiled to some extent via C; use `--disable-pbchunk` to
   disable even that compilation. The configure script tries to infer
   the machine's word size, endianness, and threading support; to
   override or avoid inference, supply `--enable-mach=<machine>` with
   a <machine> that is one of `tpb64l`, `tpb64b`, `tpb32l`, `tpb32b`,
   `pb64l`, `pb64b`, `pb32l`, or `pb32b`; the presence of absence of a
   leading "t" determines whether threads are enabled, and the
   trailing letter indicates endianness.

 * Racket BC: configure with `--disable-jit`, or run Racket with the
   `-j` flag. On some supported platforms (such as AArch64), Racket BC
   lacks JIT support and always uses interpreted mode.


========================================================================
 Make versus Zuo
========================================================================

When you run `configure` or `winfig.bat`, a makefile is generated, but
most of the build work is described by ".zuo" files. The generated
makefiles ensure that `bin/zuo` or `zuo.exe` is built and then bounces
the target request to `zuo`. When working on Racket's sources, it's
often worthwhile to build and install `zuo` to make running it easier;
see "zuo/README.md".

To build `bin/zuo` via `make`, the makefile uses the default C
compiler via `$(CC) -O2`. If a different compiler is needed, supply
`CC_FOR_BUILD=<executable>` as an argument to either `make` or
`configure` to select a compiler that builds executables for the
current machine.

A file named "build.zuo" is analogous to "Makefile.in": it's in a
source directory but meant to be used from a build directory. A file
named "main.zuo" is analogous to "Makefile", where the directory
containing "main.zuo" is the build directory. The `configure` and
`winfig.bat` scripts generate a "main.zuo" in a build directory that
bounces to "build.zuo" in the source directory. A file named
"buildmain.zuo" is even more like "Makefile.in" in the sense that
"buildmain.zuo" is instantiated in a build directory as "main.zuo".


========================================================================
 Modifying Racket
========================================================================

See "cs/README.txt" and "bc/README.txt" for information about
modifying the CS and BC implementations of Racket, but one thing they
have in common is updating the Racket version number. The source for
the Racket version number is shared in "version/racket_version.h".

The version number for the "base" package needs to be updated
separately. If this directory is in a checkout of the Git repository
for Racket, then the "base" version is in "../../pkgs/base/info.rkt".

Unfortunately, there's no single source for the version number in both
Racket and "base". Those are extracted as subtrees into separate
distributions, and the point of a version in each place is to detect a
mismatch between those extracted distributions. The "version.rktl"
test in Racket's core test suite effectively checks that they're in
sync within the Racket repo.

Another thing CS and BC have in common is the macro-expander
implementation. See "expander/README.txt" for more information,
including the "Building Racket to use this expander" section.


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

Sources for the Racket CS implementation
----------------------------------------

 * "cs" --- `racket` CS executable

 * "thread" --- thread scheduler

 * "io" --- I/O

    This layer uses the "racketio" library to access OS facilities.

 * "regexp" --- regexp matcher

 * "rktboot" --- create Chez Scheme boot files using Racket

See also the shared sources below, which includes rktio, the macro
expander, and schemify.

Sources for the Racket BC implementation
----------------------------------------

 * "bc" --- `racket` BC executable

   This implementation can build from "scratch" with a C compiler, but
   first by building a CGC variant of Racket to transform the C
   sources to build a (normal) 3m variant.

 * "mzcom" --- MzCOM executable (for Windows)

 * "myssink" --- `ffi/unsafe/com` helper DLL (for Windows)

 * "cify" --- a Racket-to-C compiler

   This compiler is used only when embedding the expander as C code in
   Racket BC, instead of Racket bytecode, which is the default for
   platforms where the Racket BC JIT is not supported.

See also the shared sources below, which includes rktio and the macro
expander.

Sources shared by both Racket implementations
---------------------------------------------

 * "expander" --- macro expander implementation

   This expander is both included in Racket executables and used to
   expand itself for inclusion in executables. It's also used to
   expand other libraries for inclusion in Racket CS executables, but
   already-expanded versions are included with source in
   "cs/schemified".

   If you change the expander, run `zuo` in its directory to generate
   the "startup.inc" file that holds the expander's implementation for
   Racket BC. Also, run `zuo` in "cs" to rebuild expanded libraries
   for Racket CS. (See "Make versus Zuo" above for information on
   running `zuo`.)

 * "rktio" --- portability layer for low-level I/O

   If you change "rktio.h", then be sure to regenerate "rktio.rktl"
   and "rktio.inc" using an existing Racket implementation that has
   the "parser-tools" package installed.

 * "schemify" --- a Racket-to-Scheme compiler, used by "cs" and "cify"

   Similar to "expander", this layer is applied to itself and other
   libraries for inclusion in "cs". If you change it, be sure to run
   `make` in "cs".

 * "start" --- main-executable wrapper

   Startup wrappers used by both the Racket CS and BC implementations.

 * "worksp" --- Windows scripts, icons, etc.

 * "mac" --- scripts for generating Mac OS ".app"s

 * "setup-go.rkt" and other ".rkt" files --- helper scripts

   The "setup-go.rkt" script is a bootstrapping tool that is used by
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

========================================================================
 Configure flags
========================================================================

Run `configure --help-cs` or `configure --help-bc` for general
information on using `configure` and they way that `--enable-<x>`
implies the possibility of `--disable-<x>`. This section provides more
detail on Racket-specific options.

Racket variant
--------------

--enable-bcdefault

 Makes Racket BC the default. Racket CS is built with a "CS" or "cs"
 suffix if it is enabled with `--enable-cs`.

--enable-csdefault

 Makes Racket CS the default. Racket BC is built with a "BC" or "bc"
 suffix if it is enabled with `--enable-bc`. This is the default mode.

--enable-cs

 Builds Racket CS when it would otherwise be skipped.

--enable-bc

 Builds Racket BC when it would otherwise be skipped.

--enable-csonly

 Makes Racket CS the default and disallows combination with
 `--enable-bconly`.

--enable-bconly

 Makes Racket BC the default and disallows combination with
 `--enable-csonly`.

--enable-cgcdefault (discouraged)

 Like `--enable-bcdefault` but further specifies the CGC variant of
 Racket BC.

--enable-crossany

 Creates a Racket build, whether CS or BC, that by default compiles to
 a machine- and variant-independent format. That format is normally used
 as a stepping stone to a machine- and variant-specific format, since
 loading variant-independent bytecode can be very slow.

--enable-gracket (BC only)

 Enabled by default: Build GRacket as well as Racket. (Racket CS
 always builds GRacket.)


Installation directory structure
--------------------------------

--prefix=<dir>

 Disables in-place build mode and makes the install step put files in
 <dir>. The installation's directory structure can be further
 specified by selecting a destination for specific parts of a build
 via the options listed below, and setting any one of them disables
 in-place mode. The directories are not required to be under the
 prefix <dir>, but normally they are. Specifying these directories not
 only chooses where to install files initially, but also configures
 the installation to use the specified directory for packages and
 collections that are added later (using installation scope instead of
 user scope).

  --bindir=<dir>

   Executables go in <dir>.

  --guibindir=<dir>

   Executables for GUI programs for in <dir>, instead of the same
   directory as other executables. This separate directory makes the
   most sense for Mac OS, where GUI executables mean ".app" bundles
   (but launcher scripts for those applications are still written in
   the non-GUI executable destination).

  --sysconfdir=<dir>

   Racket's configuration (especially a "config.rktd" file) is written
   in <dir>.

  --libdir=<dir>

   Platform-specific libraries and compiled files go in <dir>. This
   includes ".zo" files, unless `--enable-sharezo` is used.

  --datarootdir=<dir>

   Adjusts the default data, doc, and man directories to be within
   <dir>.

  --datadir=<dir>

   Adjusts the default pkgs directory to be within <dir>.

  --collectsdir=<dir>

   Racket's initial library collection sources go in <dir>. If
   `--enable-sharezo` is used, then compiled forms go here, too.

  --pkgsdir=<dir>

   Sources for packages in installation scope go in <dir>. If
   `--enable-sharezo` is used, then compiled forms go here, too.

  --docdir=<dir>

   Documentation goes in <dir>.

  --mandir=<dir>

   Man pages go in <dir>.

  --appsdir=<dir>

   For Unix, ".desktop" files for in <dir>.

  --includedir=<dir>

    C header files (for Racket C ABI) go in <dir>.

--enable-origtree

 Enabled by default on Mac OS unless `--enable-xonx` is used: When
 installing to a destination specified by `--prefix`, use the same
 directory structure as an in-place build instead of a more Unix-style
 build.

  --enable-macprefix      allow --prefix with a Mac OS install

--disable-useprefix

 Causes `--prefix` to be ignored, forcing an in-place build. This
 option is intended primarily for the makefile in a Git checkout's
 root to ensure that an in-place build really is in-place.

--enable-sharezo

 In a Unix-style directory structure, causes ".zo" files for compiled
 mores to be kept in "share" instead of "lib". Using "lib" is more
 correct for Racket CS, since its ".zo" files are platform-specific.

--enable-libfw

 Install Mac OS frameworks to "/Library/Frameworks".

--enable-userfw

 Install Mac OS frameworks to "~/Library/Frameworks".

--enable-embedfw

 Statically link into executables the implementation that would
 otherwise be in a framework (shared library) on Mac OS.

--enable-libs

 Enabled by default for Unix: install static libraries containing
 Racket's kernel implementation.

Racket functionality and OS libraries
-------------------------------------

--enable-pthread

 Normally enabled automatically: Use the pthreads library to enable
 internal threads for places, futures, and similar. For Racket CS,
 disabling the use of pthread affects the inferred Chez Scheme machine
 type, picking one that does not start with "t".

--enable-iconv

 Normally enabled automatically: Use the iconv library for
 string-encoding conversion. Disabling use of iconv will limit
 conversions available from Racket's `bytes-open-converter`.

--enable-curses

 Normally enabled automatically: Use the curses or ncurses library for
 terminal control. Disabling the use of curses will hobble editing in
 Racket's `read-eval-print-loop`.

--enable-curseslib=<libs>

 Use <libs> for linking to curses library, instead of letting
 `configure` automatically find a suitable curses or ncurses library.

--enable-portable

 Link to curses support in a way that maximizes portability across OS
 variants, especially Linux versions and distributions.

--enable-libz

 Enabled by default: Use the system-installed libz, if available,
 instead of Racket's vendored copy.

--enable-liblz4

 Enabled by default: Use the system-installed liblz4, if available,
 instead of Racket's vendored copy.

--enable-libffi (BC only)

 Enabled by default: Use the system-installed libffi, if available,
 instead of Racket's vendored copy.

--enable-jit (BC only)

 Enabled by default: Support JIT compilation. (Racket CS always
 compiles to machine code on the fly.)

--enable-foreign (BC only)

 Enabled by default: Support foreign calls, which use libffi. (Racket CS
 always supports foreign calls, and it uses libffi only for pb
 variants.)

--enable-places (BC only)

 Enabled by default for pthread builds and 3m: Support places. (Racket
 CS always supports places for pthread builds.)

--enable-futures (BC only)

 Enabled by default for pthread builds and 3m: Support futures. (Racket
 CS always supports futures for pthread builds.)

--enable-float (BC only)

 Enabled by default: Support single-precision float-point values.
 (Racket CS never supports single-precision float-point values, except
 the `flsingle` procedure.)

--enable-floatinstead (BC only, strongly discouraged)

 Use single-precision floats by default.

--enable-extflonum (BC only)

 Enabled by default for i386 and x86_64 platforms: Support extflonums.
 (Racket CS never supports extflonums.)

--enable-generations (BC only)

 Enable by default: Support generational garbage collection in the 3m
 variant.

--enable-backtrace (BC only, expensive debugging mode)

 Enable backtrace dumps with the 3m variant.

--enable-sgc (BC only)

 Obsolete: Senora GC is always enabled for the CGC variant.

--enable-sgcdebug (BC only,  expensive debugging mode)

 Use Senora GC for debugging in the CGC variant.

Time--space tradeoffs
---------------------

--enable-shared

 A standard `configure` flag that has no effect for Racket CS and that
 is not recommended for Racket BC. Windows builds always use shared
 libraries, Mac OS builds use a framework unless `--enable-embedfw` is
 used, and Racket CS on other platforms never uses shared libraries.
 In all cases, executables built from Racket and distributed together
 will share a library or core executable, achieving much of the same
 effect of shared libraries.

--enable-wpo (CS only)

 Enabled by default: Use "whole-program operation" to build the
 immediate Racket layer on top of Chez Scheme. Using `--disable-wpo`
 substantially decreases the amount of memory needed to compile the
 Racket CS executable, but it may reduce performance.

--enable-compress (CS only)

 Enabled by default: Compress (via lz4) machine code when writing
 Racket ".zo" files. This flag affects only the default for writing
 ".zo" files, and all Racket CS builds support compressed and
 uncompressed content.

--enable-compressboot (CS only)

 Enabled by default on Windows: Compress the machine code for Racket's
 initial image. This machine code may be in "petite.boot",
 "scheme.boot", and "racket.boot" files, or it may be embedded in
 executables.

--enable-dynlib (BC only)

 Same as `--enable-shared`.

--enable-cify (BC only)

 Enabled by default on platforms where the JIT is not available:
 Compile Racket code in the core implementation (which includes the
 macro expander) to C instead of bytecode.

Packages and user-specific files
--------------------------------

--enable-pkgscope=<scope>

 Sets the default scope used by `raco pkg`, where <scope> can be
 `installation`, `user`, or `shared`. The default is to preserve the
 current setting, which is normally `user` but might be initialized to
 `installation` in a Git checkout build. If a <scope> is provided,
 then the build's configuration is changed to use it.

--enable-docs

 Enabled by default: Build package-provided documentation on install.
 If no documentation package is pre-installed (in a source
 distribution) or currently installed (when rebuilding in-place), then
 `--enable-docs` has no effect. Documentation for Racket itself is
 provided by the "racket-doc" package.

--enable-usersetup

 Includes user-scope packages and user-specific collections in the
 setup set of an install. Normally, the install step ignores
 user-scope packages and user-specific collections.

--enable-pkgs=<pkgs>

 At the install step, install packages listed (as space-separated) in
 <pkgs>. When building from a source distribution, the installation may
 be staged early in the `make` step instead of `make install`.

--enable-catalog=<url>

 Install any extra packages by consulting the catalog <url> instead of
 the default or currently configured (or preconfigured for a source
 distribution) catalog.

--enable-missingpkgs

 Enabled by default for a relevant platforms when building from a
 source distribution (as opposed to a Git checkout): Detect and add
 missing package dependencies at the install step. Mac OS is a
 relevant platform unless `--enable-xonx` or `--enable-nonnatipkg` is
 specified. Windows is a relevant platform. Using `--enable-natipkg`
 makes any platform relevant.

Target configuration and cross compilation
------------------------------------------

--enable-racket=<path>

 Use <path> as Racket for building, especially for cross-compilation,
 but also potentially to bootstrap parts of the build instead of
 starting from more primitive layers. Use "auto" as <path> for a cross
 build to automatically generate the host-platform Racket that is
 needed for cross compilation. See "Cross compilation" above for more
 information.

--enable-scheme=<path> (CS only)

 Use <path> as Chez Scheme for building. See "Cross compilation" above
 for more information.

--enable-mach=<mach> (CS only)

 Build for Chez Scheme machine type <mach>. Normally, <mach> is
 inferred, but supply it if inference somehow fails, For cross
 compilation, it's not enough to specify <mach>, and something like
 `--host=...` will be needed to describe the target machine (and then
 <mach> normally can be inferred); see "Cross compilation above for
 more information. Specifying a machine type that does not start with
 "t" changes whether the pthread library is used by default. If
 `--enable-pb` is also used, then `--enable-mach` can still be
 relevant for helping inference of how to build the interpreter
 kernel.

--enable-target=<mach> (CS only)

 Selects cross-compilation mode for a target described by the Chez
 Scheme machine type <mach>. Normally, `--host=...` causes <mach> to
 be inferred, but `--enable-target` provides a way to override
 inference and insist on cross compilation.

--enable-pb (CS only)

 Build for pb (portable bytecode) interpretation instead of
 native-code generation for Racket programs. A pb variant with
 suitable threading, word size, endianness will be inferred, such as
 `tpb46le`. The `--enable-pb` flag might be used in combination with
 `--enable-mach=<mach>`, in which case the `<mach>` helps infer the
 choice of pb variant and the way that the interpreter kernel itself
 is compiled. If `--enable-racket=auto` is supplied, then
 `--enable-pb` triggers cross-compilation mode. Note that for some
 host platforms (such as s390x), a pb variant is the default machine
 type.

--enable-pbchunk (CS only)

 Enable by default: When building a pb variant of Racket, compile some
 bytecode representing Racket's own implementation to machine code via
 C. The converted implementation includes the compiler and macro
 expander, so `--enable-pbchunk` can substantially speed up Racket as
 a compiler and provider of built-in operations. The generated C code
 may be large and take a long time to compile, however.

--enable-check (CS only)

 Build the Chez Scheme and Racket core in safe mode, which is
 potentially useful for tracking down bugs in Racket's implementation
 or exposing bugs in the use of unsafe operations provided by Racket.
 Normally, this mode should not be used outside of debugging Racket
 itself.

--enable-natipkg

 Adds "-natipkg" to the result of `system-library-subpath`. The intent
 is primarily to effect platform-specific package dependencies,
 perhaps causing the "racket-x86_64-linux-natipkg-3" dependency of the
 "racket-lib" package to be activated, for example.

--enable-nonatipkg

 Adds "-nonatipkg" to the result of `system-library-subpath`. Like
 `--enable-natipkg`, the intent is to affect package dependencies,
 perhaps causing the "racket-x86_64-macosx-3" dependency of the
 "racket-lib" package to *not* be activated, for example.

--enable-xonx

 Affects Mac OS builds only: Creates a build that is Unix-style in its
 default installation layout and package dependencies. For an
 `--enable-xonx` build, the `system-library-subpath` result will use
 "darwin" instead of "macosx".

--enable-ios=<path>

 Cross-compile for iOS, and use <path> as the SDK path along the same
 lines as `--enable-sdk`.

--enable-mac64

  Enabled by default: allow 64-bit Mac OS build. Historically,
  `--disable-mac64` was used to build for 32-bit Mac OS on 64-bit Mac
  OS.

--enable-stackup (BC only)

 Assume "up" if the stack direction cannot be inferred.

--enable-bigendian (BC only)

 Assume "big" if endianness cannot be inferred.

--enable-ffipoll (BC only)

 Defer FFI calls to the point where an embedding program explicitly
 polls for them.

C compiler and linker configuration
-----------------------------------

--enable-sdk=<path>

 Use <path> as a Mac OS 10.4 SDK directory, also setting 10.4 as the
 minimal Mac OS version.

--enable-sdk5=<path>

 Like `--enable-sdk`, but for Mac OS 10.5.

--enable-sdk6=<path>

 Like `--enable-sdk`, but for Mac OS 10.6.

--enable-sdk9=<path>

 Like `--enable-sdk`, but for Mac OS 10.9.

--enable-sdk11=<path>

 Like `--enable-sdk`, but for Mac OS 11.0.

--enable-sysroot=<path>

 Use <path> as a sysroot directory (e.g., for Android).

--enable-postlink=<cmd>

 Apply <cmd> to executable after linking.

--enable-strip

 Usually enabled by default: Strip debugging information at the
 install step.

--enable-ubsan

 Compile with `-fsanitize=undefined`.

--enable-asan (CS only)

 Compile with `-fsanitize=address`.

--enable-gprof (BC only)

 Compile for profiling with gprof (gcc only).

--enable-gcov (BC only)

 Compile to gather gcov statistics (gcc3 only).

--enable-noopt (BC only)

 Drop `-O` C flags (useful for low-level debugging).

--enable-jitframe (BC only)

 On x86_64, use a frame pointer for internal calls.

--enable-werror (BC only)

 Compile with warnings as errors.

--enable-lt=<prog> (BC only)

 Use <prog> instead of `libtool`. Use `--disable-lt` to always use a
 vendored `libtool`.
