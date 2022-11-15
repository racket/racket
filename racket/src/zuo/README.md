Zuo: A Tiny Racket for Scripting
================================

You should use Racket to write scripts. But what if you need something
much smaller than Racket for some reason — or what if you're trying
to script a build of Racket itself? Zuo is a tiny Racket with
primitives for dealing with files and running processes, and it comes
with a `make`-like embedded DSL.

Zuo is a Racket variant in the sense that program files start with
`#lang`, and the module path after `#lang` determines the parsing and
expansion of the file content. That's how the `make`-like DSL is
defined, and even the base Zuo language is defined by layers of
`#lang`s. One of the early layers implements macros.


Some Example Scripts
--------------------

See [`local/hello.zuo`](local/hello.zuo),
[`local/tree.zuo`](local/tree.zuo),
[`local/image.zuo`](local/image.zuo), and
[`build.zuo`](build.zuo).


Building and Running Zuo
------------------------

Compile `zuo.c` with a C compiler. No additional files are needed,
other than system and C library headers. No compiler flags should be
needed, although flags like `-o zuo` or `-O2` are a good idea.

You can also use `configure`, `make`, and `make install`, where `make`
targets mostly invoke a Zuo script after compiling `zuo.c`. If you
don't use `configure` but compile to `zuo` in the current directory,
then `./zuo build.zuo` and `./zuo build.zuo install` (omit the `./` on Windows)
will do the same thing as `make` and `make install` with a default
configuration.

The Zuo executable runs only modules. If you run Zuo with no
command-line arguments, then it loads `main.zuo`. Use the `-c`
flag to provide module text as an argument. Otherwise, the first
argument to Zuo is a file to run or a directory containing a
`main.zuo` to run, and additional arguments are delivered to that Zuo
program via the `runtime-env` procedure. Running the command
`./zuo build install`, for example, runs the `build/main.zuo` program
with the argument `install`. Whatever initial script is run, if it has
a `main` submodule, that submodule is also run.


Library Modules and Startup Performance
---------------------------------------

Except for the built-in `zuo/kernel` language module, Zuo finds
languages and modules through a collection of libraries. By default,
Zuo looks for a directory `lib` relative to the executable as the root
of the library-collection tree. You can supply an alternate collection
path with the `-X` command-line flag.

You can also create an instance of Zuo with a set of libraries
embedded as a heap image. Embedding a heap image has two advantages:

 * No extra directory of library modules is necessary.

 * Zuo can start especially quickly, competitive with the fastest
   command-line programs.

The `local/image.zuo` script generates a `.c` file that is a copy of
`zuo.c` plus embedded modules. By default, the `zuo` module and its
dependencies are included, but you can specify others with `++lib`. In
addition, the default collection-root path is disabled in the
generated copy, unless you supply `--keep-collects` to
`local/image.zuo`.

When you use `configure` and `make` or `./zuo build.zuo`, the default
build target creates a `to-run/zuo` that embeds the `zuo` library, as
well as a `to-install/zuo` that has the right internal path to find
other libraries after `make install` or `./zuo build.zuo install`.

You can use heap images without embedding. The `dump-heap-and-exit`
Zuo kernel permitive creates a heap image, and a `-B` or `--boot`
command-line flag for Zuo uses the given boot image on startup. You
can also embed an image created with `dump-image-and-exit` by using
`local/image.zuo` with the `--image` flag.

A boot image is machine-independent, whether in a stand-alone file or
embedded in `.c` source.


Cross Compiling
---------------

If you use `./configure --host=...` to cross compile, then you will
also need to add something like `CC_FOR_BUILD=cc` as a `./configure`
argument to specify the compiler for a `zuo` to use on the build
machine. If necessary, you can also specify `CFLAGS_FOR_BUILD`,
`LDFLAGS_FOR_BUILD`, and/or `LIBS_FOR_BUILD`.


Embedding Zuo in Another Application
------------------------------------

Zuo can be embedded in a larger application, with or without an
embedded boot image. To support embedding, compile `zuo.c` or the
output of `local/image.zuo` with the `ZUO_EMBEDDED` preprocessor macro
defined (to anything); the `zuo.h` header will be used in that case,
and `zuo.h` should also be used by the embedding application.
Documentation for the embedding API is provided as comments within
`zuo.h`.


More Information
----------------

Install the `zuo-doc` directory as a package in Racket to render the
documentation there.
