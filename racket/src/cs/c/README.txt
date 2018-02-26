This directory contains a `configure` script and a makefile (template)
for building the variant of Racket that runs on Chez Scheme. The
result of the build is a `racketcs` executable that embeds both Chez
Scheme and the Racket startup code to behave the same as the
traditional `racket` executable.

If you have a checkout of the main Racket repo, you can just use `make
cs` in the top-level directory of the repo to build Racket-on-Chez.
See "INSTALL.txt" in the top-level directory for more information.

If you want to know more about how Racket-on-Chez is put together, see
"../README.txt".

========================================================================
 Requirements
========================================================================

Building Racket-on-Chez requires both an existing Racket build and
Chez Scheme build:

 * By default, the build uses Racket as built and installed in-place
   in the same way as described in "../../README", so that the Racket
   executable is "../../../bin/racket".

   You can select a different Racket excutable by supplying
   `--enable-racket=...` to `configure`.

 * By default, the build uses Chez Scheme as built in a "ChezScheme"
   sibling directory of the build directory. The Racket-on-Chez build
   needs a Chez Scheme build directory, and not an end-user Chez
   Scheme installation, because it needs "kernel.o" as created in a
   Chez Scheme build; it may also need makefiles or other scripts in
   the Chez Scheme build directory.

   See "../README.txt" for information on the required Chez Scheme
   version.

   You can select a different Chez Scheme build path by supplying
   `--enable-scheme=...` to `configure`.

========================================================================
 Compiling for supported Unix variants (including Linux and Mac OS)
========================================================================

From two directories up, run the following commands:

   mkdir build
   cd build
   ../cs/c/configure
   make
   make install

Those commands will create an in-place installation of Racket and
store the results of various compilation steps in a separate "build"
subdirectory, which is useful if you need to update your sources,
delete the build, and start from scratch.

You can also run the typical `./configure && make && make install` if
you don't anticipate updating/rebuilding, but it will be harder to
restart from scratch should you need to.

========================================================================
 Compiling for Windows
========================================================================

Compilation for Windows on Windows requires building the traditional
Racket implementation. Then, from the directory "..\..\worksp", run

 ..\..\racket csbuild.rkt

Many intermediate files will be put in "../../build", including a Chez
Scheme checkout if it's not already present (in whcih case `git` must
be available).
