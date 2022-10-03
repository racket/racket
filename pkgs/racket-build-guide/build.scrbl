#lang scribble/manual
@(require "common.rkt")

@title[#:tag "build"]{Building Racket from Source}

In a checkout of the Racket @hyperlink[git-repo]{Git repository}, you
could try just running

@commandline{make}

but we recommend that you at least consider the information in
@secref["src"] and @secref["modes"].

@; ------------------------------------------------------------
@section[#:tag "src"]{Git Repository versus Source Distribution}

Instead of building from the @hyperlink[git-repo]{Git repository},
consider getting source for the current Racket release from

@centerline{@url{http://download.racket-lang.org/}}

or get a source snapshot (updated daily) from

@centerline{@url{http://snapshot.racket-lang.org/}}

The @onscreen{Source + built packages} options from those sites will
build and install especially quickly, because platform-independent
bytecode and documentation are pre-built.

In contrast to the Git repository, release and snapshot source
distributions will work in the

@commandline{configure --prefix=... && make && make install}

way that you probably expect.

@; ------------------------------------------------------------
@section[#:tag "modes"]{Git Repository Build Modes}

The rest of this chapter assumes that you're sticking with the
@hyperlink[git-repo]{source repository}. In that case, you still have
several options:

@itemlist[

 @item{@bold{In-place build} --- This mode is the default. It creates
   a build in the @filepath{racket} subdirectory and installs packages
   that you specify (or the @filepath{main-distribution} plus
   @filepath{main-distribution-test} package by default). Any package
   implementations that reside in the @filepath{pkgs} subdirectory are
   linked in-place. This is the most natural mode for developing
   Racket itself or staying on the bleeding edge. See
   @secref["quick-in-place"] for more instructions.}

 @item{@bold{Unix-style install} --- This mode installs to a given
   destination directory (on platforms other than Windows), leaving no
   reference to the source directory. This is the most natural mode
   for installing once from the source repository. See
   @secref["quick-unix-style"] for more instructions.}

 @item{@bold{Minimal} --- This mode is like a source distribution, and
   it is described in the @filepath{src} subdirectory of
   @filepath{racket} (i.e., ignore the repository's root directory and
   @filepath{pkgs} subdirectory). Build a minimal Racket using the
   usual @exec{configure && make && make install} steps (or similar
   for Windows), and then you can install packages from the catalog
   server with @exec{raco pkg}.}

 @item{@bold{Installers} --- This mode creates Racket distribution
   installers for a variety of platforms by farming out work to
   machines that run those platforms. This is the way that Racket
   snapshots and releases are created, and you can create your own.
   See @secref["distribute"] for more instructions.}

 @item{@bold{In-place Racket BC build} --- This mode builds the old
   Racket implementation (where ``BC'' means ``bytecode'' or ``before
   Chez Scheme''). Final executables with names that end in
   @litchar{bc} or @litchar{BC} are the Racket BC variants. See
   @secref["build-cs"] for more information.}

]

@; ------------------------------------------------------------
@section[#:tag "quick-in-place"]{Quick Instructions: In-Place Build}

On Unix (including Linux) and Mac OS, @exec{make} (or @exec{make in-place})
creates a build in the @filepath{racket} directory.

On Windows with Microsoft Visual Studio (any version between 2008/9.0
and 2022/17.0), @exec{nmake} creates a build in the @filepath{racket}
directory. If your command-prompt environment is not already
configured for Visual Studio to run programs like @exec{nmake.exe} and
@exec{cl.exe}, you run @filepath{racket/src/worksp/msvcprep.bat}
(PowerShell: @filepath{racket/src/worksp/msvcprep.ps1}} and provide an
argument that selects a build mode: @exec{x86} (32-bit Intel/AMD
mode), @exec{x64} or @exec{x86_amd64} (64-bit Intel/AMD mode), or
@exec{x64_arm64} (64-bit Arm mode). Any use of @exec{make} described
in this build guide should also work with @exec{nmake}, except as
noted.

In all cases, an in-place build includes (via links) a few packages
that are in the @filepath{pkgs} directory. To get new versions of
those packages, as well as the Racket core, then use @exec{git pull}.
Afterward, or to get new versions of any other package, use @exec{make
in-place} again, which includes a @exec{raco pkg update} step.

See @secref["more"] for more information.


@; ------------------------------------------------------------
@section[#:tag "quick-unix-style"]{Quick Instructions: Unix-Style Install}

On Unix (including Linux), @exec{make unix-style PREFIX=@nonterm{dir}}
builds and installs into @filepath{@nonterm{dir}} with binaries in
@filepath{@nonterm{dir}/bin}, packages in
@filepath{@nonterm{dir}/share/racket/pkgs}, documentation in
@filepath{@nonterm{dir}/share/racket/doc}, etc.

On Mac OS, @exec{make unix-style PREFIX=@nonterm{dir}} builds and
installs into @filepath{@nonterm{dir}} with binaries in
@filepath{@nonterm{dir}/bin}, packages in
@filepath{@nonterm{dir}/share/pkgs}, documentation in
@filepath{@nonterm{dir}/doc}, etc.

On Windows, Unix-style install is not supported.

A Unix-style install leaves no reference to the source directory.

To split the build and install steps of a Unix-style installation,
supply @exec{DESTDIR=@nonterm{dest-dir}} with @exec{make unix-style
PREFIX=@nonterm{dir}}, which assembles the installation in
@filepath{@nonterm{dest-dir}}. Then, copy or move the content of
@filepath{@nonterm{dest-dir}} to the target root
@filepath{@nonterm{dir}}; take care to preserve file timestamps.

See @secref["more"] for more information.

@; ------------------------------------------------------------
@section[#:tag "more"]{More Instructions: Building Racket}

The @filepath{racket} directory contains minimal Racket, which is just
enough to run @exec{raco pkg} to install everything else. A first step
of @exec{make in-place} or @exec{make unix-style} is to build minimal
Racket, and you can read @filepath{racket/src/README.txt} for more
information, including information about dependencies. (The very first
step of a build is to compile Zuo, which is a tiny variant of Racket
that @seclink["zuo"]{drives the rest of the build system}.)

If you would like to provide arguments to @exec{configure} for the
minimal Racket build, then you can supply them with by adding
@exec{CONFIGURE_ARGS="@nonterm{options}"} to @exec{make in-place}
or @exec{make unix-style}.

The @filepath{pkgs} directory contains packages that are tied to the
Racket core implementation and are therefore kept in the same Git
repository. A @exec{make in-place} links to the package in-place,
while @exec{make unix-style} copies packages out of @filepath{pkgs} to
install them.

To install a subset of the packages that would otherwise be installed,
supply a @exec{PKGS} value to @exec{make}. For example,

@commandline{make PKGS="gui-lib readline-lib"}

installs only the @filepath{gui-lib} and @filepath{readline-lib} packages
and their dependencies. The default value of @exec{PKGS} is
@tt{"main-distribution main-distribution-test"}. If you run @tt{make}
a second time, all previously installed packages remain installed and
are updated, while new packages are added. To uninstall previously
selected package, use @exec{raco pkg remove}.

To build anything other than the latest sources in the repository
(e.g., when building from the @tt{v6.2.1} tag), you need a catalog
that's compatible with those sources. Release tags starting with
@tt{v7.1} include a default catalog that corresponds to the release.
For earlier versions, the release distribution is configured to use a
catalog specific to that release, so you can extract the catalog's URL
from there.

Using @exec{make} (or @exec{make in-place}) sets the installation's
name to @tt{development}, unless the installation has been previously
configured (i.e., unless the @filepath{racket/etc/config.rktd} file
exists). The installation name affects, for example, the directory
where user-specific documentation is installed. Using @exec{make} also
sets the default package scope to @exec{installation}, which means
that packages are installed by default into the installation's space
instead of user-specific space. The name and/or default-scope
configuration can be changed through @exec{raco pkg config}.

When @exec{make -j @nonterm{n}} is used to specify parallelism, the
build system may be able to propagate that choice to intermediate
steps and the @exec{raco setup} part. A more portable alternative is
to supply the @exec{JOBS} variable:

@commandline{make JOBS=@nonterm{n}}

Setting @exec{JOBS} works with other targets, including @exec{make
unix-style}. For backward compatibility, @exec{CPUS} is recognized as
an alias for @exec{JOBS}.

Use @exec{make as-is} to perform the same build actions as @exec{make
in-place}, but without consulting any package catalogs or package
sources to install or update packages. In other words, use @exec{make
as-is} to rebuild after local changes that could include changes to
minimal Racket. (If you change only packages, then @exec{raco setup}
should suffice.)

If you need even more control over the build, carry on to
@secref["even-more"] further below.


@; ------------------------------------------------------------
@section[#:tag "build-cs"]{More Instructions: Building Racket CS and Racket BC}

The default build of Racket, also known as Racket CS, uses and
incorporates Chez Scheme. Chez Scheme sources are included in the
Racket repository.

Building Racket CS requires either an existing Racket or pb (portable
bytecode) boot files for Chez Scheme. By default, pb boot files are
downloaded from a separate Git repository by @exec{make}. If you have
Racket v7.1 or later, then you can choose instead to bootstrap using
that Racket implementation with

@commandline{make cs BOOTFILE_RACKET=racket}

The @exec{make bc} target (or @exec{make bc-as-is} for a rebuild) builds an older
variant of Racket, called Racket BC, which does not use Chez Scheme.
By default, the executables for the Racket BC variant all have a
@litchar{bc} or @litchar{BC} suffix, and they coexist with a Racket CS
build by keeping compiled files in a @filepath{bc} subdirectory of the
@filepath{compiled} directory. You can remove the @litchar{bc} suffix
and the subdirectory in @filepath{compiled} by providing
@exec{RACKETBC_SUFFIX=""} to @exec{make bc}.

Along similar lines, you can add a @litchar{cs} suffix to the Racket
CS executables and cause them to use a machine-specific subdirectory
of @filepath{compiled} by providing @exec{RACKETCS_SUFFIX="cs"} to
@exec{make} or @exec{make cs}.

Use @exec{make both} to build both Racket BC and Racket CS, where
packages are updated and documentation is built only once (using
Racket CS).

@; ------------------------------------------------------------
@section[#:tag "even-more"]{Even More Instructions: Building Racket Pieces}

Instead of just using @exec{make in-place} or @exec{make unix-style}, you can
take more control over the build by understanding how the pieces fit
together. You can also read @filepath{Makefile}, which defines and
describes many variables that can be supplied via @exec{make}.

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection{Building Minimal Racket}

Instead of using the top-level makefile, you can go into
@filepath{racket/src} and follow the @filepath{README.txt} there,
which gives you more configuration options.

If you don't want any special configuration and you just want the base
build, you can use @exec{make base} with the
top-level makefile.

Minimal Racket does not require additional native libraries to run,
but under Windows, encoding-conversion, extflonum, and SSL
functionality is hobbled until native libraries from the
@filepath{racket-win32-i386} or @filepath{racket-win32-x86_64} package
are installed.

On all platforms, from the top-level makefile, the
@exec{PLT_SETUP_OPTIONS} makefile variable is passed on to the
@exec{raco setup} that is used to build minimal-Racket libraries. See
the documentation for @exec{raco setup} for information on the
options. (The @exec{JOB_OPTIONS} makefile variable is also passed on,
but it is meant to be set by some makefile targets when @exec{CPUS} is
non-empty.)

For cross compilation, add configuration options to
@exec{CONFIGURE_ARGS="@nonterm{options}"} as described in the
@filepath{README.txt} of @filepath{racket/src}, but also add a
@exec{RACKET=...} argument for the top-level makefile instead of
using  @exec{--enable-racket=...} for @exec{configure}.

Specify @exec{SETUP_MACHINE_FLAGS=@nonterm{options}} to set Racket
flags that control the target machine of compiled bytecode for
@exec{raco setup} and @exec{raco pkg install}. For example
@exec{SETUP_MACHINE_FLAGS=-M} causes the generated bytecode to be
machine-independent, which is mainly useful when the generated
installation will be used as a template for other platforms or for
cross-compilation.

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection{Installing Packages}

After you've built and installed minimal Racket, you could install
packages via the package-catalog server, completely ignoring the
content of @filepath{pkgs}.

If you want to install packages manually out of the @filepath{pkgs}
directory, the @exec{local-catalog} target creates a catalog as
@filepath{racket/local/catalog} that merges the currently configured
catalog's content with pointers to the packages in @filepath{pkgs}. A
Unix-style build works that way: it builds and installs minimal
Racket, and then it installs packages out of a catalog that is created
by @exec{make local-catalog}.

To add a package catalog that is used after the content of
@filepath{pkgs} but before the default package catalogs, specify the
catalog's URL as the @exec{SRC_CATALOG} makefile variable:

@commandline{make .... SRC_CATALOG=@nonterm{url}}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@subsection{Linking Packages for In-Place Development Mode}

With an in-place build, you can edit packages within @filepath{pkgs} directly
or update those packages with @exec{git pull} plus @exec{raco setup}, since the
packages are installed with the equivalent of @exec{raco pkg install -i
--static-link @nonterm{path}}.

Instead of actually using @exec{raco pkg install --static-link ...}, the
@exec{pkgs-catalog} makefile target creates a catalog that points to the
packages in @filepath{pkgs}, and the catalog indicates that the packages are to
be installed as links. The @exec{pkgs-catalog} target further configures
the new catalog as the first one to check when installing
packages. The configuration adjustment is made only if no
configuration file @filepath{racket/etc/config.rktd} exists already.

All other packages (as specified by @exec{PKGS}) are installed via the
configured package catalog. They are installed in installation scope, but
the content of @filepath{racket/share/pkgs} is not meant to be edited. To
reinstall a package in a mode suitable for editing and manipulation
with Git tools, use

@commandline{raco pkg update --clone extra-pkgs/@nonterm{pkg-name}}

The @filepath{extra-pkgs} directory name is a convention that is supported by a
@filepath{.gitignore} entry in the repository root.
