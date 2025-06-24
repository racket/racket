# Building, Distributing, and Contributing to Racket

The main Racket source code repository is

[https://github.com/racket/racket](https://github.com/racket/racket)

This guide explains how to build those sources, how to create Racket
distributions like the ones at
[https://download.racket-lang.org](https://download.racket-lang.org),
and how to contribute to Racket development.

If you’re reading this document in Markdown form, you may find the
[online HTML
version](https://docs.racket-lang.org/racket-build-guide/index.html)
more readable. There’s no guarantee that the online version is still
available or matches the Racket sources you’re using, however.

> [1 Building Racket from Source](#1-building-racket-from-source)  
>> [1.1 Git Repository versus Source Distribution](#11-git-repository-versus-source-distribution)  
>> [1.2 Git Repository Build Modes](#12-git-repository-build-modes)  
>> [1.3 Quick Instructions: In-Place Build](#13-quick-instructions-in-place-build)  
>> [1.4 Quick Instructions: Unix-Style Install](#14-quick-instructions-unix-style-install)  
>> [1.5 More Instructions: Building Racket](#15-more-instructions-building-racket)  
>> [1.6 More Instructions: Building Racket CS and Racket BC](#16-more-instructions-building-racket-cs-and-racket-bc)  
>> [1.7 Even More Instructions: Building Racket Pieces](#17-even-more-instructions-building-racket-pieces)  
>>> [1.7.1 Building Minimal Racket](#171-building-minimal-racket)  
>>> [1.7.2 Installing Packages](#172-installing-packages)  
>>> [1.7.3 Linking Packages for In-Place Development Mode](#173-linking-packages-for-in-place-development-mode)  
  
> [2 Contributing to Racket Development](#2-contributing-to-racket-development)  
>> [2.1 Main-Repository Contributions](#21-main-repository-contributions)  
>> [2.2 Distribution-Package Contributions](#22-distribution-package-contributions)  
>> [2.3 General Contribution Guidelines](#23-general-contribution-guidelines)  
>> [2.4 More Resources](#24-more-resources)  
  
> [3 Zuo and the Racket Build System](#3-zuo-and-the-racket-build-system)  
  
> [4 Bootstrapping Racket](#4-bootstrapping-racket)  
  
> [5 Distributing Racket Variants](#5-distributing-racket-variants)  
>> [5.1 Running Build Farms](#51-running-build-farms)  
>> [5.2 Generating Installer Web Sites](#52-generating-installer-web-sites)  
>> [5.3 Managing Snapshot Web Sites](#53-managing-snapshot-web-sites)  
>> [5.4 Separate Server and Clients](#54-separate-server-and-clients)  
>> [5.5 Creating a Client from an Installer Web Site](#55-creating-a-client-from-an-installer-web-site)  
>> [5.6 Cleaning Up Docker Containers](#56-cleaning-up-docker-containers)  

## 1. Building Racket from Source

In a checkout of the Racket [Git
repository](https://github.com/racket/racket), you could try just
running

  `make`

but we recommend that you at least consider the information in [Git
Repository versus Source
Distribution](#11-git-repository-versus-source-distribution) and [Git
Repository Build Modes](#12-git-repository-build-modes).

### 1.1. Git Repository versus Source Distribution

Instead of building from the [Git
repository](https://github.com/racket/racket), consider getting source
for the current Racket release from

[http://download.racket-lang.org/](http://download.racket-lang.org/)

or get a source snapshot (updated daily) from

[http://snapshot.racket-lang.org/](http://snapshot.racket-lang.org/)

The Source + built packages options from those sites will build and
install especially quickly, because platform-independent bytecode and
documentation are pre-built.

In contrast to the Git repository, release and snapshot source
distributions will work in the

  `configure --prefix=... && make && make install`

way that you probably expect.

### 1.2. Git Repository Build Modes

The rest of this chapter assumes that you’re sticking with the [source
repository](https://github.com/racket/racket). In that case, you still
have several options, depending on your goal, but you almost certainly
want the first one:

* **In-place build** — ​_This mode is the default, and it is almost
  certainly the mode you want._​ In this mode, “build” and “install” are
  the same, because the build is self-contained for in-place use. It
  creates a build in the `"racket"` subdirectory and installs (local to
  that subdirectory) packages that you specify (or the
  `"main-distribution"` plus `"main-distribution-test"` packages by
  default). Building and installing packages implies that documentation
  provided by those packages is built and locally installed, too. Any
  package implementations that reside in the `"pkgs"` subdirectory are
  linked in-place. This is the most natural mode for developing Racket
  itself or staying on the bleeding edge. See [Quick Instructions:
  In-Place Build](#13-quick-instructions-in-place-build) for more
  instructions.

* **Unix-style install** — ​_This mode is not the one you want for
  contributing to Racket, but it can be a sensible choice for installing
  Racket._​ This mode builds and installs to a given destination
  directory (on platforms other than Windows), leaving no reference to
  the source directory. This is the most natural mode for installing
  once from the source repository. See [Quick Instructions: Unix-Style
  Install](#14-quick-instructions-unix-style-install) for more
  instructions.

* **Minimal** — ​_This mode is a building block for miscellaneous tasks,
  and probably not the mode you want._​ This mode is like a source
  distribution, and it is described in the `"src"` subdirectory of
  `"racket"` (i.e., ignore the repository’s root directory and `"pkgs"`
  subdirectory). Build an in-place minimal Racket using `make base`.
  Alternatively, use `make pb-fetch` to download bootstrapping support,
  and then in `"racket/src"` use the usual `configure && make && make
  install` steps (or similar for Windows). After installation, you can
  install packages from the catalog server with `raco pkg`; if you do
  not use `make base`, you should install at least the `"racket-lib"`
  package. See [Building Minimal Racket](#171-building-minimal-racket)
  for more information.

* **Installers** — ​_This mode is for creating new distributions of
  Racket, not for developing or installing Racket locally._​ This mode
  creates Racket distribution installers for a variety of platforms by
  farming out work to machines that run those platforms. This is the way
  that Racket snapshots and releases are created, and you can create
  your own. See [Distributing Racket
  Variants](#5-distributing-racket-variants) for more instructions.

* **In-place Racket BC build** — ​_This mode is for software
  archeologists or developers with a particular need to access a
  historical Racket implementation in a contemporary context._​ This
  mode builds the old Racket implementation (where “BC” means “bytecode”
  or “before Chez Scheme”). Final executables with names that end in
  `bc` or `BC` are the Racket BC variants. See [More Instructions:
  Building Racket CS and Racket
  BC](#16-more-instructions-building-racket-cs-and-racket-bc) for more
  information.

### 1.3. Quick Instructions: In-Place Build

On Unix (including Linux) and Mac OS, `make` (or `make in-place`)
creates a build in the `"racket"` directory, and the build is an
“installation” in the sense that you can run it directly.

On Windows with Microsoft Visual Studio (any version between 2008/9.0
and 2022/17.0), `nmake` creates a build in the `"racket"` directory. If
your command-prompt environment is not already configured for Visual
Studio to run programs like `nmake.exe` and `cl.exe`, you can run
`"racket/src/worksp/msvcprep.bat"` \(PowerShell:
`"racket/src/worksp/msvcprep.ps1"`) and provide an argument that selects
a build mode: `x86` (32-bit Intel/AMD mode), `x64` or `x86_amd64`
(64-bit Intel/AMD mode), or `x64_arm64` (64-bit Arm mode). Any use of
`make` described in this build guide should also work with `nmake`,
except as noted.

In all cases, an in-place build includes (via links) a few packages that
are in the `"pkgs"` directory. To get new versions of those packages, as
well as the Racket core, then use `git pull`. Afterward, or to get new
versions of any other package, use `make in-place` again, which includes
a `raco pkg update` step.

See [More Instructions: Building
Racket](#15-more-instructions-building-racket) for more information. If
your goal is to contribute to Racket development, skip to [Contributing
to Racket Development](#2-contributing-to-racket-development), first.

### 1.4. Quick Instructions: Unix-Style Install

On Unix (including Linux), `make unix-style PREFIX=<dir>` builds and
installs into `"<dir>"` with binaries in `"<dir>/bin"`, packages in
`"<dir>/share/racket/pkgs"`, documentation in
`"<dir>/share/racket/doc"`, etc.

On Mac OS, `make unix-style PREFIX=<dir>` builds and installs into
`"<dir>"` with binaries in `"<dir>/bin"`, packages in
`"<dir>/share/pkgs"`, documentation in `"<dir>/doc"`, etc.

On Windows, Unix-style install is not supported.

A Unix-style install leaves no reference to the source directory.

To split the build and install steps of a Unix-style installation,
supply `DESTDIR=<dest-dir>` with `make unix-style PREFIX=<dir>`, which
assembles the installation in `"<dest-dir>"`. Then, copy or move the
content of `"<dest-dir>"` to the target root `"<dir>"`; take care to
preserve file timestamps.

See [More Instructions: Building
Racket](#15-more-instructions-building-racket) for more information.

### 1.5. More Instructions: Building Racket

The `"racket"` directory contains minimal Racket, which is just enough
to run `raco pkg` to install everything else. A first step of `make
in-place` or `make unix-style` is to build minimal Racket, and you can
read `"racket/src/README.txt"` for more information, including
information about dependencies. (The very first step of a build is to
compile Zuo, which is a tiny variant of Racket that [drives the rest of
the build system](#3-zuo-and-the-racket-build-system).)

If you would like to provide arguments to `configure` for the minimal
Racket build, then you can supply them with by adding
`CONFIGURE_ARGS="<options>"` to `make in-place` or `make unix-style`.

The `"pkgs"` directory contains packages that are tied to the Racket
core implementation and are therefore kept in the same Git repository. A
`make in-place` links to the package in-place, while `make unix-style`
copies packages out of `"pkgs"` to install them.

To install a subset of the packages that would otherwise be installed,
supply a `PKGS` value to `make`. For example,

  `make PKGS="gui-lib readline-lib"`

installs only the `"gui-lib"` and `"readline-lib"` packages and their
dependencies. The default value of `PKGS` is `"main-distribution
main-distribution-test"`. If you run `make` a second time, all
previously installed packages remain installed and are updated, while
new packages are added. To uninstall previously selected package, use
`raco pkg remove`.

To build anything other than the latest sources in the repository
\(e.g., when building from the `v6.2.1` tag), you need a catalog that’s
compatible with those sources. Release tags starting with `v7.1` include
a default catalog that corresponds to the release. For earlier versions,
the release distribution is configured to use a catalog specific to that
release, so you can extract the catalog’s URL from there.

Using `make` (or `make in-place`) sets the installation’s name to
`development`, unless the installation has been previously configured
(i.e., unless the `"racket/etc/config.rktd"` file exists). The
installation name affects, for example, the directory where
user-specific documentation is installed. Using `make` also sets the
default package scope to `installation`, which means that packages are
installed by default into the installation’s space instead of
user-specific space. The name and/or default-scope configuration can be
changed through `raco pkg config`.

When `make -j <n>` is used to specify parallelism, the build system may
be able to propagate that choice to intermediate steps and the `raco
setup` part. A more portable alternative is to supply the `JOBS`
variable:

  `make JOBS=<n>`

Setting `JOBS` works with other targets, including `make unix-style`.
For backward compatibility, `CPUS` is recognized as an alias for `JOBS`.

Use `make as-is` to perform the same build actions as `make in-place`,
but without consulting any package catalogs or package sources to
install or update packages. In other words, use `make as-is` to rebuild
after local changes that could include changes to minimal Racket. (If
you change only packages, then `raco setup` should suffice.)

If you need even more control over the build, carry on to [Even More
Instructions: Building Racket
Pieces](#17-even-more-instructions-building-racket-pieces) further
below.  If your goal is to contribute to Racket development, skip to
[Contributing to Racket
Development](#2-contributing-to-racket-development), first.

### 1.6. More Instructions: Building Racket CS and Racket BC

The default build of Racket, also known as Racket CS, uses and
incorporates Chez Scheme. Chez Scheme sources are included in the Racket
repository.

Building Racket CS requires either an existing Racket or pb (portable
bytecode) boot files for Chez Scheme. By default, pb boot files are
downloaded from a separate Git repository by `make`. If you have Racket
v7.1 or later, then you can choose instead to bootstrap using that
Racket implementation with

  `make cs BOOTFILE_RACKET=racket`

The `make bc` target (or `make bc-as-is` for a rebuild) builds an older
variant of Racket, called Racket BC, which does not use Chez Scheme. By
default, the executables for the Racket BC variant all have a `bc` or
`BC` suffix, and they coexist with a Racket CS build by keeping compiled
files in a `"bc"` subdirectory of the `"compiled"` directory. You can
remove the `bc` suffix and the subdirectory in `"compiled"` by providing
`RACKETBC_SUFFIX=""` to `make bc`.

Along similar lines, you can add a `cs` suffix to the Racket CS
executables and cause them to use a machine-specific subdirectory of
`"compiled"` by providing `RACKETCS_SUFFIX="cs"` to `make` or `make cs`.

Use `make both` to build both Racket BC and Racket CS, where packages
are updated and documentation is built only once (using Racket CS).

### 1.7. Even More Instructions: Building Racket Pieces

Instead of just using `make in-place` or `make unix-style`, you can take
more control over the build by understanding how the pieces fit
together. You can also read `"Makefile"`, which defines and describes
many variables that can be supplied via `make`.

If you are just trying to get a build in place so you can to contribute
to Racket development, then you’ve probably read too far in this
section. Try jumping to [Contributing to Racket
Development](#2-contributing-to-racket-development).

#### 1.7.1. Building Minimal Racket

Instead of using the top-level makefile, you can go into `"racket/src"`
and follow the `"README.txt"` there, which gives you more configuration
options.

If you don’t want any special configuration and you just want the base
build, you can use `make base` with the top-level makefile.

Minimal Racket does not require additional native libraries to run, but
under Windows, encoding-conversion, extflonum, and SSL functionality is
hobbled until native libraries from the `"racket-win32-i386"` or
`"racket-win32-x86_64"` package are installed.

On all platforms, from the top-level makefile, the `PLT_SETUP_OPTIONS`
makefile variable is passed on to the `raco setup` that is used to build
minimal-Racket libraries. See the documentation for `raco setup` for
information on the options. (The `JOB_OPTIONS` makefile variable is also
passed on, but it is meant to be set by some makefile targets when
`CPUS` is non-empty.)

For cross compilation, add configuration options to
`CONFIGURE_ARGS="<options>"` as described in the `"README.txt"` of
`"racket/src"`, but also add a `RACKET=...` argument for the top-level
makefile instead of using  `--enable-racket=...` for `configure`.

Specify `SETUP_MACHINE_FLAGS=<options>` to set Racket flags that control
the target machine of compiled bytecode for `raco setup` and `raco pkg
install`. For example `SETUP_MACHINE_FLAGS=-M` causes the generated
bytecode to be machine-independent, which is mainly useful when the
generated installation will be used as a template for other platforms or
for cross-compilation.

#### 1.7.2. Installing Packages

After you’ve built and installed minimal Racket, you could install
packages via the package-catalog server, completely ignoring the content
of `"pkgs"`.

If you want to install packages manually out of the `"pkgs"` directory,
the `local-catalog` target creates a catalog as `"racket/local/catalog"`
that merges the currently configured catalog’s content with pointers to
the packages in `"pkgs"`. A Unix-style build works that way: it builds
and installs minimal Racket, and then it installs packages out of a
catalog that is created by `make local-catalog`.

To add a package catalog that is used after the content of `"pkgs"` but
before the default package catalogs, specify the catalog’s URL as the
`SRC_CATALOG` makefile variable:

  `make .... SRC_CATALOG=<url>`

#### 1.7.3. Linking Packages for In-Place Development Mode

With an in-place build, you can edit packages within `"pkgs"` directly
or update those packages with `git pull` plus `raco setup`, since the
packages are installed with the equivalent of `raco pkg install -i
--static-link <path>`.

Instead of actually using `raco pkg install --static-link ...`, the
`pkgs-catalog` makefile target creates a catalog that points to the
packages in `"pkgs"`, and the catalog indicates that the packages are to
be installed as links. The `pkgs-catalog` target further configures the
new catalog as the first one to check when installing packages. The
configuration adjustment is made only if no configuration file
`"racket/etc/config.rktd"` exists already.

All other packages (as specified by `PKGS`) are installed via the
configured package catalog. They are installed in installation scope,
but the content of `"racket/share/pkgs"` is not meant to be edited. To
reinstall a package in a mode suitable for editing and manipulation with
Git tools, use

  `raco pkg update --clone extra-pkgs/<pkg-name>`

The `"extra-pkgs"` directory name is a convention that is supported by a
`".gitignore"` entry in the repository root.

## 2. Contributing to Racket Development

The Racket developers are happy to receive bug reports and improvements
to the implementation and documentation through GitHub issues and pull
requests:

* Issues (bug reports):
  [https://github.com/racket/racket/issues](https://github.com/racket/racket/issues)

* Pull requests (improvements):
  [https://github.com/racket/racket/pulls](https://github.com/racket/racket/pulls)

The Racket distribution includes scores of packages that have their own
separate repositories, which somewhat complicates the process of sending
pull requests. The mechanism is the same, but see [Distribution-Package
Contributions](#22-distribution-package-contributions) for more
guidance.

By making a contribution, you are agreeing that your contribution is
licensed under the LGPLv3, Apache 2.0, and MIT licenses. Those licenses
are available in the [Racket Git
repository](https://github.com/racket/racket) in the files
`"LICENSE.txt"`, `"LICENSE-APACHE.txt"`, and `"LICENSE-MIT.txt"`.

### 2.1. Main-Repository Contributions

The [main Racket Git repository](https://github.com/racket/racket)
contains the implementation of everything that is in the Minimal Racket
distribution. That includes the runtime system, core libraries, and
`raco pkg` so that other packages can be installed.

The main Racket repository also has the source to the Racket Reference,
Racket Guide, and other core-ish documentation, including the source to
the document that you are reading. Those document sources are in the
repository’s `"pkgs"` directory.

Finally, the main repository includes a few other packages that are
especially tightly bound to the runtime-system implementation, such as
the `"compiler-lib"` package or the `"racket-test"` package. Those
package sources are also in the repository’s `"pkgs"` directory.

To develop improvements to any of those parts of Racket, following the
usual GitHub-based workflow:

* Fork the Racket repository.

* Create an in-place build as described in [Building Racket from
  Source](#1-building-racket-from-source).

* Make your changes and rebuild with `make` or `make as-is` or `raco
  setup`, where `raco setup` is the best choice when modifying Racket
  libraries that are in `"collects"` or a package. If your changes
  involve modifying things that are part of the `racket` executable,
  then a simple `make` may not suffice; see “Modifying Racket” in
  `"racket/src/README.txt"` for more information.

* Commit changes to your fork and [submit a pull
  request](https://help.github.com/en/articles/creating-a-pull-request).

See the [General Contribution
Guidelines](#23-general-contribution-guidelines).

### 2.2. Distribution-Package Contributions

If you find yourself changing a file that is in a `"share/pkgs"`
subdirectory (either installed as part of a Racket release or as a
product of an in-place build), then that file is probably not part of
the main Racket Git repository. It almost certainly has its own Git
repository somewhere else, possibly within
[https://github.com/racket](https://github.com/racket), but possibly in
another user’s space. The name of the directory in `"share/pkgs"` is
almost certainly the package name.

To start working on a package <_pkg-name_>, in the directory you’d like
to hold the package’s source, use

  `raco pkg update --clone <pkg-name>`

> For Racket version 8.14 and earlier as a release or snapshot, before
> using `--clone`, you first need to adjust the package installation to
> use the source specified by the main package catalog:
>   `raco pkg update --no-setup --catalog https://pkgs.racket-lang.org
> <pkg-name>`

That command will clone the package’s source Git repository into
`"<pkg-name>"` within the current directory and checkout the appropriate
commit. Then, it will replace the current installation of the package in
your Racket build to point at that directory, and then it will rebuild
(essentially by using `raco setup`) with the new location of the package
installation. Now you can edit in `"<pkg-name>"`, and your changes will
be live.

Some information that might improve your experience:

* You can add `--no-setup` to the `raco pkg update` command to skip the
  `raco setup` step, which makes sense if you want to make changes and
  then run `raco setup` yourself.

* The argument after `--clone` is a directory, and by default, the
  package name is inferred from the directory. Within an in-place build
  of the main Racket repository, for example, the conventional use

    `raco pkg update --clone extra-pkgs/<pkg-name>`

  creates `"extra-pkgs/<pkg-name>"` as a clone of the Git repository for
  <_pkg-name_> (and `".gitignore"` for the Racket repository excludes
  `"extra-pkgs"`).

* To use a clone directory name that is different than the package name,
  you can supply the package name explicitly after the `--clone`
  directory name:

    `raco pkg update --clone <repo-name> <pkg-name>`

* If you’re done and want to go back to the normal installation for
  <_pkg-name_>, use

    `raco pkg update --unclone <pkg-name>`

* See Developing Packages with Git for more information about how
  packages are meant to work as Git repositories.

Note that none of this is necessary if you’re modifying a package in the
main Racket repository’s `"pkgs"` directory. Those are automatically
linked in place for an in-place build of Racket.

### 2.3. General Contribution Guidelines

When you make a pull request, the Racket developers will help you get
the improvement in shape to merge to the Racket repository. You can make
that process faster by keeping a few guidelines in mind:

* Try to follow the style guide.

* When you fix a bug or create a new feature, include a test case for
  it.

  Note that core Racket tests are in
  `"pkgs/racket-test-core/tests/racket"`, and tests for other libraries
  are also sometimes in a separate `"-test"` package.

* Include new or updated documentation as appropriate.

  To locate a documentation (Scribble) source file, visit the current
  documentation in a browser, and click at the page heading. A box will
  appear with a URL to a documentation source. Note that while it is
  likely that the documentation source will not be the file that you
  want to edit exactly, it should give you a rough idea for where it is.
  Particularly, the Racket reference is in
  `"pkgs/racket-doc/scribblings/reference"`, and the Racket guide is in
  `"pkgs/racket-doc/scribblings/guide"`.

  When adding to a library or extending an existing binding’s behavior,
  be sure to include a `history` note in the documentation to record the
  change.

* Build with your changes.

  Don’t break the Racket build. That means at least checking that `raco
  setup` runs and completes without errors. If you added or modified
  documentation, visually inspect the newly rendered documentation to
  make sure it reads as intended.

  A common mistake is to just run a modified library or its tests, but
  where a change creates a new package dependency that will only be
  detected by a full `raco setup`. _Really:_ run `raco setup`.

* For changes to the C code, ensure your code follows the C99 standard.

  On Unix systems, extensions that are part of the `_DEFAULT_SOURCE`
  pre-processor flag are also allowed. See the
  [glibc](https://www.gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html#index-_005fDEFAULT_005fSOURCE)
  manual for more details.

### 2.4. More Resources

For additional pointers on how to contribute to Racket, see

[https://github.com/racket/racket/wiki/Ways-to-contribute-to-Racket](https://github.com/racket/racket/wiki/Ways-to-contribute-to-Racket)

## 3. Zuo and the Racket Build System

Racket builds with many options, and the build needs to work in a
variety of environments. That variability is difficult to manage through
a traditional makefile. The Racket build is mostly driven instead with
Zuo, which is a tiny, Racket-like scripting language with facilities
inspired by `make` and [Shake](https://shakebuild.com/). When you build
Racket with `make`, the makefile target ensures that `zuo` is built, and
then it bounces the build request to a `"main.zuo"` script.

Racket makefiles build `zuo` using the `CC_FOR_BUILD` makefile variable
plus `CFLAGS_FOR_BUILD`. The `CC_FOR_BUILD` variable defaults to using
the `CC` makefile variable plus `-O2`, while `CC` normally defaults to
`cc`. If you need to specify a C compiler or options for building Zuo,
supply `CC=<compiler>`, `CC_FOR_BUILD=<compiler>`, and/or
`CFLAGS_FOR_BUILD=<flags>` to `make`.

In you have `zuo` installed, you can generally substitute `zuo .` in
place of `make` when building Racket components. You can even use just
`zuo` in place of `make` if you’re not providing additional target or
variable arguments to `make`, but otherwise `.` is needed after `zuo` to
select the `main.zuo` script in the current directory. In most cases, it
doesn’t matter whether you use `make` or `zuo .`, but if you move deep
enough into the Racket build tree, there are only Zuo scripts. To
install Zuo, you can use the usual `configure && make && make install`
in `"racket/src/zuo"`.

Even when you run `zuo` directly, configuration information is
frequently read from `"Makefile"` or `"Mf-config"`. The latter name is
used when the makefile exists only for recording a configuration and
does not provide targets. When you run a `configure` script,
configuration choices are recorded in a generated `"Makefile"` or
`"Mf-config"`.

By convention, a source file `"build.zuo"` is analogous to
`"Makefile.in"`: it is meant to be instantiated in a build directory as
`"main.zuo"`. Instead of copying and updating, as typically happens to
convert `"Makefile.in"` to `"Makefile"`, a `"main.zuo"` is typically
instantiated as a small module, possibly by copying a `"buildmain.zuo"`
file to `"main.zuo"`. That `"main.zuo"` reaches `"build.zuo"` using a
source directory that is recorded in an accompanying `"Makefile"` or
`"Mf-config"`.

## 4. Bootstrapping Racket

Although Racket is implemented in Racket, you do not normally need an
existing Racket installation to build Racket. Distribution archives
include the needed bootstrapping artifacts in a portable form. The
Racket Git repository similarly includes some of those artifacts checked
in directly, and some are in a separate repository that is downloaded by
`make`. Specifically:

* `"racket/src/cs/schemified"` includes macro-expanded, schemified
  versions of layers that are implemented in Racket for Racket CS, and
  these are checked into the Git repository;

* `"racket/src/bc/srcstartup.inc"` is the macro-expanded expander (as
  implemented in Racket) for Racket BC, and it is checked into the Git
  repository; and

* `"racket/src/ChezScheme/boot/pb"` contains Chez Scheme pb (portable
  bytecode) boot files, normally downloaded from a separate Git
  repository in a branch that has a single commit \(i.e., no history of
  old versions within the branch\).

If you modify certain pieces of Racket, you will need an existing build
of Racket to bootstrap. That includes the Chez Scheme implementation (at
least for some kinds of modifications), the Racket macro expander, and
in the case of Racket CS, the "thread", "io", "regexp", and "schemify"
layers.

For more information about modifying Chez Scheme, see
`"racket/src/cs/README.txt"`. As explained there, you can create new
boot files in `"racket/src/ChezScheme/boot/pb"` or platform-specific
boot files using even a relatively old version of Chez Scheme or Racket.

For information about modifying the macro expander for Racket CS and/or
BC, see `"racket/src/expander/README.txt"`. Building the expander may
require a relatively new version of Racket, perhaps even the very latest
version before the change.

Finally, for information about modifying the other layers for Racket CS,
see `"racket/src/cs/README.txt"`. Rebuilding these layers requires a
relatively new version of Racket, too.

## 5. Distributing Racket Variants

This chapter is about distributing variants of Racket, as opposed to
distributing applications that are built with Racket. See `raco
distribute`: Sharing Stand-Alone Executables for information about
distributing applications.

**Important:** To build installers that can be distributed to other
users, do not use `make in-place` or `make unix-style`, but instead
start from a clean repository.

Use one non-Windows machine as a server, where packages will be
pre-built. Then, as described below, create platform-specific installers
on some number of client machines, each of which contacts the server
machine to obtain pre-built packages. The server can act as a client,
naturally, to create an installer for the server’s platform. Instead of
using separate client machines, Docker containers can be used to
cross-compile for multiple platforms, all on a single “server” machine.

The distribution-build process is a collaboration between the Racket Git
repository’s top-level makefile and the `"distro-build"` package.

### 5.1. Running Build Farms

The `installers` target of the makefile will do everything to generate
installers: build a server on the current machine, run clients on hosts
specified via `CONFIG`, and start/stop Docker containers or VirtualBox
virtual machines that act as client machines.

If the server is already built, the `installers-from-built` target will
drive the client builds without re-building the server.

See the documentation of the `"distro-build"` package for a description
of the site-configuration module and requirements on client hosts. The
`distro-build/main-distribution` library provides site-configuration
helpers for creating a distribution like the one at the main Racket
download site and using Docker containers (all on one host machine) to
cross-compile for supported platforms.

If `"my-site-config.rkt"` is a configuration module, then

  `make installers CONFIG=my-site-config.rkt`

drives the build farm, and the resulting installers are in
`"build/installers"`, with a hash table mapping descriptions to
installer filenames in `"build/installer/table.rktd"`. A log file for
each client is written to `"build/log"`.

If you have the `"distro-build-server"` package installed in some Racket
build (not the one for building installers), you can use

  `make describe-clients CONFIG=my-site-config.rkt`

to see, without building anything, the effect of the configuration in
`"my-site-config.rkt"` and the planned build steps. See also the
`#:fake-installers?` site-configuration option.

The default `CONFIG` path is `"build/site.rkt"`, so you could put your
configuration file there and omit the `CONFIG` argument to `make`. A
default configuration file is created there automatically. Supply
`CONFIG_MODE=...` to pass a configuration mode on to your
site-configuration module (accessible via the `current-mode` parameter).
Supply `CLEAN_MODE=--clean` to make the default `#:clean?` configuration
for a client to `#t` instead of `#f`, supply `RELEASE_MODE=--release` to
make the default `#:release?` configuration `#t`, supply
`SOURCE_MODE=--source` to make the default `#:source?` configuration
`#t`, and supply `VERSIONLESS_MODE=--version` to make the default
`#:versionless?` configuration `#t`.

A configuration file can specify the packages to include, host address
of the server, distribution name, installer directory, and documentation
search URL, but defaults can be provided as `make` arguments via `PKGS`,
`SERVER` plus `SERVER_PORT` plus `SERVER_HOSTS`, `DIST_NAME`,
`DIST_BASE`, and `DIST_DIR`, `DOC_SEARCH`, respectively. The site
configuration’s top-level options for packages and documentation search
URL are used to configure the set of packages that are available to
client machines to include in installers.

For each installer written to `"build/installers"`, the installer’s name
is

`"<dist-base>-<version>-<platform>-<dist-suffix>.<ext>"`

where <_dist-base_> defaults to `"racket"` (but can be set via
`DIST_BASE`), <_platform_> is from `(system-library-subpath #f)` but
normalizing the Windows results to `"i386-win32"` and `"x86_63-win32"`,
-<_dist-suffix_> is omitted unless a `#:dist-suffix` string is specified
for the client in the site configuration, and <_ext_> is
platform-specific: `".sh"` for Unix (including Linux), `".dmg"` or
`".pkg"` for Mac OS, and `".exe"` for Windows.

The server supports CS clients on different platforms and BC clients by
creating built packages in machine-independent form (which is then
recompiled to the client’s native format, still much faster than
compiling from source).

### 5.2. Generating Installer Web Sites

The `site` target of the makefile uses the `installers` target to
generate a set of installers, and then it combines the installers,
packages, a package catalog, and log files into a directory that is
suitable for access via a web server.

Supply the same `CONFIG=...` and `CONFIG_MODE=...` arguments for `site`
as for `installers`. The configuration file should have a
`#:dist-base-url` entry for the URL where installers and packages will
be made available; the `installers` target uses `#:dist-base-url` to
embed suitable configuration into the installers. Specifically,
installers are configured to access pre-built packages and documentation
from the site indicated by `#:dist-base-url`.

Note that `#:dist-base-url` should almost always end with `"/"`, since
other URLs will be constructed as relative to `#:dist-base-url`.

The site is generated as `"build/site"` by default. A `#:site-dest`
entry in the configuration file can select an alternate destination.

Use the `site-from-installers` makefile target to perform the part of
`site` that happens after `installers` (i.e., to generate a `site` from
an already-generated set of installers).

### 5.3. Managing Snapshot Web Sites

The `snapshot-site` makefile target uses `site` (so supply the same
`CONFIG=...` and `CONFIG_MODE=...` arguments), and then treats the
resulting site as a snapshot with additional snapshot-management tasks.

For snapshot management, the destination of the files generated for
`site` (as specified by `#:site-dest`) should be within a directory of
snapshots. The configuration file can use `(current-stamp)` to get a
string that represents the current build, and then use the string both
for `#:dist-base-url` and `#:site-dest`. Normally, the stamp string is a
combination of the date and Git commit hash.

Snapshot management includes creating an `"index.html"` file in the
snapshots directory (essentially a copy of the snapshot’s own
`"index.html"`) and pruning snapshot subdirectories to keep the number
of snapshots at the amount specified by `#:max-snapshots`
configuration-file entry (with a default value of `5`).

Use the `snapshot-at-site` makefile target to perform the part of
`snapshot-site` that happens after `site` (i.e., to manage snapshots
around an already-generated site).

### 5.4. Separate Server and Clients

Instead of using the `installers` makefile target and a site
configuration file, you can run server and client processes manually.

Roughly, the steps are as follows

* On the server machine:

    `make server PKGS="<pkgs>"`

  See step 2 in the detailed steps below for more information on
  variables other than `PKGS` that you can provide with `make`.

* On each client machine:

    `make client SERVER=<address> PKGS="<pkgs>"`

  See 4 in the detailed steps below for more information on variables
  other than `SERVER` and `PKGS` that you can provide with `make`.

In more detail, the steps are as follows:

* Build `racket` on a server.

  The `base` target of the makefile will do that, if you haven’t done it
  already. (The server only works on non-Windows platforms, currently.)

* On the server, build packages and start a catalog server.

  The `server-from-base` target of the makefile will do that.

  Alternatively, use the `server` target, which combines `base` and
  `server-from-base` (i.e., steps 1 and 2).

  The `SERVER_PORT` variable of the makefile choose the port on which
  the server listens to clients. The default is port `9440`.

  The `SERVER_HOSTS` variable of the makefile determines the interfaces
  at which the server listens. The default is `localhost` which listens
  only on the loopback device (for security). Supply the empty string to
  listen on all interfaces. Supply multiple addresses by separating them
  with a comma.

  The `PKGS` variable of the makefile determines which packages are
  built for potential inclusion in a distribution.

  The `DOC_SEARCH` variable of the makefile determine a URL that is
  embedded in rendered documentation for cases where a remote search is
  needed (because other documentation is not installed).

  The `SRC_CATALOG` variable determines the catalog that is used to get
  package sources and native-library packages. The default is
  `http://pkgs.racket-lang.org`.

  The `SERVER_PKG_INSTALL_OPTIONS` variable determines extra flags that
  are passed to `raco pkg install` when installing on the server (to
  create package builds that are sent to clients). For example,
  `SERVER_PKG_INSTALL_OPTIONS=--source` could be useful to ensure that
  the server always builds from sources.

  The `PACK_BUILT_OPTIONS` variable can be set to `--mode <mode>` to set
  the package mode for built packages. The default `infer` mode infers
  uses the package’s `distribution-preference` `"info.rkt"` field, if
  any, infers `binary` if the package has any native libraries and no
  Racket sources, and infers `built` otherwise.

  The server provides README files from the `"build/readmes"` directory.
  If `"README.txt"` does not exist when the sever is started, a default
  file is created (and clients download `"README.txt"` by default).

  If you stop the server and want to restart it, use the
  `built-package-server` makefile target instead of starting over with
  the `server` target.

* On each client (one for each platform to bundle), build `racket`.

  This is the same as step 1, but on each client. If the client and
  server are the same, there’s nothing more to do for step 3.

* On each client, create an installer.

  The `client` target of the makefile will do that.

  Provide `SERVER` as the hostname of the server machine, but a
  `localhost`-based tunnel back to the server is more secure and avoids
  the need to specify `SERVER_HOSTS` when starting the server in step 2.
  Also, provide `SERVER_PORT` if an alternate port was specified in step
  2.

  Provide the same `PKGS` (or a subset) as in step 2 if you want a
  different set than the ones listed in the makefile. Similarly,
  `DOC_SEARCH` normally should be the same as in step 2, but for a
  client, it affects future documentation builds in the installation.

  Alternatively, use the `client` target, which combines `base` and
  `client-from-base` (i.e., steps 3 and 4).

  On Windows, you need NSIS installed, either in the usual location or
  with `makensis` in your command-line path.

  To create a release installer, provide `RELEASE_MODE` as `--release`
  to `make`. A release installer has slightly different defaults that
  are suitable for infrequently updated release installations, as
  opposed to frequently updated snapshot installations.

  To create a source archive, provide `SOURCE_MODE` as `--source` to
  `make`.

  To create an archive that omits the version number and also omit and
  version number in installer paths, provide `VERSIONLESS_MODE` as
  `--versionless` to `make`.

  To change the human-readable name of the distribution as embedded in
  the installer, provide `DIST_NAME` to `make`. The default distribution
  name is `Racket`. Whatever name you pick, the Racket version number is
  automatically added for various contexts.

  To change the base name of the installer file, provide `DIST_BASE` to
  `make`. The default is `racket`.

  To change the directory name for installation on Unix (including
  Linux), provide `DIST_DIR` to `make`. The default is  `racket`.

  To add an extra piece to the installer’s name, such as an identifier
  for a variant of Linux, provide `DIST_SUFFIX` to `make`. The default
  is `"", which` omits the prefix and its preceding hyphen.

  To set the description string for the installer, provide `DIST_DESC`
  to `make`. The description string is recorded alongside the installer.

  To set the initial package catalogs URLs for an installation, provide
  `DIST_CATALOGS_q` to `make`. Separate multiple URLs with a space, and
  use an empty string in place of a URL to indicate that the default
  catalogs should be used. The `_q` in the variable name indicates that
  its value can include double quotes (but not single quotes)—which are
  needed to specify an empty string, for example.

  To select a `"README"` file for the client, provide `README` to
  `make`. The `README` value is used as a file name to download from the
  server.

  To create a `".tgz"` archive instead of an installer (or any
  platform), set `TGZ_MODE` to `--tgz`.

  For a Mac OS installer, set `SIGN_IDENTITY` as the name to which the
  signing certificate is associated. Set `MAC_PKG_MODE` to `--mac-pkg`
  to create a `".pkg"` installer instead of a `".dmg"` image.

  For a Windows installer, set `OSSLSIGNCODE_ARGS_BASE64` as a Base64
  encoding of an S-expression for a list of argument strings for
  `osslsigncode`. The `-n`, `-t`, `-in`, and `-out` arguments are
  provided to `osslsigncode` automatically, so supply the others.

  The `SERVER_CATALOG_PATH` and `SERVER_COLLECTS_PATH` makefile
  variables specify paths at `SERVER` plus `SERVER_PORT` to access the
  package catalog and pre-built `"collects"` tree needed for a client,
  but those paths should be empty for a server started with `make
  server`, and they are used mainly by `make client-from-site`
  \(described below\).

  The `UPLOAD` makefile variable specifies a URL to use as an upload
  destination for the created installed, where the installer’s name is
  added to the end of the URL, or leave as empty for no upload.

On each client, step 4 produces a `"bundle/installer.txt"` file that
contains the path to the generated installer on one line, followed by
the description on a second line. The installer is also uploaded to the
server, which leaves the installer in a `"build/installers"` directory
and records a mapping from the installer’s description to its filename
in `"build/installers/table.rktd"`.

If you provide `JOB_OPTIONS=<options>` for either a client or server
build, the options are used both for `raco setup` and `raco pkg
install`. Normally, `JOB_OPTIONS` is used to control parallelism.

### 5.5. Creating a Client from an Installer Web Site

If you (or someone else) previously created an installer site with `make
site`, then `make client-from-site` in a clean repository creates an
installer for the current platform drawing packages from the site.

At a minimum, provide `SERVER`, `SERVER_PORT` (usually 80 or 443),
`SERVER_URL_SCHEME` (if `https` instead of `http`) and `SITE_PATH` (if
not empty, include a trailing `/`) makefile variables to access a site
at

`http://$(SERVER):$(SERVER_PORT)/$(SITE_PATH)`

The `client-from-site` makefile target chains to `make client` while
passing suitable values for `DIST_CATALOGS_q`, `DOC_SEARCH`,
`SERVER_CATALOG_PATH`, and `SERVER_COLLECTS_PATH`. Supply any other
suitable variables, such as `DIST_NAME` or `RELEASE_MODE`, the same as
for `make client`.

### 5.6. Cleaning Up Docker Containers

When building with Docker containers as clients, the containers will
remain after the completion of client builds, and they will be used for
incremental updates as long as they exist. The containers can be removed
at any time between builds. Use

  `make clean-clients`

to clean up Docker instances and shared data that is created by client
builds.
