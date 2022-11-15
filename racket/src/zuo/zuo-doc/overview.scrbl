#lang scribble/manual
@(require (for-label zuo-doc/fake-zuo))

@title{Zuo Overview}

Zuo is a Racket variant in the sense that program files start with
@hash-lang[], and the module path after @hash-lang[] determines the
parsing and expansion of the file content. Zuo, however, has a
completely separate implementation. So, even though its programs start
with @hash-lang[], Zuo programs are not meant to be run via Racket.

While @racket[@#,hash-lang[] @#,racketmodname[zuo/base]] accesses a
base language, the primary intended use of Zuo is with
@racket[@#,hash-lang[] @#,racketmodname[zuo]], which includes the
@racketmodname[zuo/build] library for using @seclink["zuo-build"]{Zuo
as a @exec{make} replacement}.

The name ``Zuo'' is derived from the Chinese word for ``make.''


@section[#:tag "running"]{Building and Running Zuo}

Compile @filepath{zuo.c} from the Zuo sources with a C compiler. No
additional are files needed for compilation, other than system and
C-library headers. No compiler flags should be needed, although flags
like @exec{-o zuo} or @exec{-O2} are a good idea.

You can also use @exec{configure}, @exec{make}, and @exec{make
install}, where @exec{make} targets mostly invoke a Zuo script after
compiling @filepath{zuo.c}. If you don't use @exec{configure} but
compile to @exec{zuo} in the current directory, then @exec{./zuo
build.zuo} and @exec{./zuo build.zuo install} (omit the @exec{./} on Windows)
will do the same thing as @exec{make} and @exec{make install} with
a default configuration.

The Zuo executable runs only modules:

@itemlist[

 @item{If you run Zuo with no command-line arguments, then it loads
       @filepath{main.zuo} in the current directory.}

 @item{As long as the @Flag{c} is not used and the first argument is
       not the empty string, the first argument to Zuo is used as a
       file to run or a directory containing a @filepath{main.zuo} to
       run.

       Note that starting Zuo with the argument @filepath{.} is
       equivalent to the argument @filepath{./main.zuo}, so @exec{zuo
       .} is a convenient replacement for @exec{make} while still
       passing arguments.}

 @item{If the @Flag{c} flag is provided to Zuo, the first argument is
       treated as the text of a module to run, instead of the name of
       a file or directory.}

 @item{If the first argument to Zuo is the empty string (which would
       be invalid as a file path), the module to run is read from
       standard input.}

]

Additional Zuo arguments are delivered to that program via the
@racket[runtime-env] procedure. When the initial script module has a
@racketidfont{main} submodule (see @racket[module+]), that submodule
is run.

@history[#:changed "1.1" @elem{Added the @Flag{c} flag.}]

@section{Library Modules and Startup Performance}

Except for the built-in @racketmodname[zuo/kernel] language module,
Zuo finds languages and modules through a collection of libraries. By
default, Zuo looks for a directory @filepath{lib} relative to the
executable as the root of the library-collection tree. You can supply
an alternate collection path with the @Flag{X} command-line flag.

You can also create an instance of Zuo with a set of libraries
embedded as an image. Embedding an image has two advantages:

@itemlist[

 @item{No extra directory of library modules is necessary, as long as
       all relevant libraries are embedded.}

 @item{Zuo can start especially quickly, competitive with the fastest
       command-line programs.}

]

The @filepath{local/image.zuo} script included with the Zuo sources
generates a @filepath{.c} file that is a copy of @filepath{zuo.c} plus
embedded modules. By default, the @racketmodname[zuo] module and its
dependencies are included, but you can specify others with
@DPFlag{lib}. In addition, the default collection-root path is
disabled in the generated copy, unless you supply
@DFlag{keep-collects} when running @filepath{image.zuo}.

When you use @exec{configure} and @exec{make} @exec{./zuo build.zuo} to
build Zuo, the default build target creates a @filepath{to-run/zuo}
that embeds the @racketmodname[zuo] library, as well as a
@filepath{to-install/zuo} that has the right internal path to find
other libraries after @exec{make install} or @exec{./zuo build.zuo
install}.

You can use images without embedding. The @racket[dump-image-and-exit]
Zuo kernel permitive creates an image containing all loaded modules,
and a @Flag{B} or @DFlag{boot} command-line flag for Zuo uses the
given boot image on startup. You can also embed an image created with
@racket[dump-image-and-exit] by using @filepath{local/image.zuo} with
the @DFlag{image} flag.

A boot image is machine-independent, whether in a stand-alone file or
embedded in @filepath{.c} source.


@section{Embedding Zuo in Another Application}

Zuo can be embedded in a larger application, with or without an
embedded boot image. To support embedding, compile @filepath{zuo.c} or
the output of @filepath{local/image.zuo} with the @tt{ZUO_EMBEDDED}
preprocessor macro defined (to anything); the @filepath{zuo.h} header
will be used in that case, and @filepath{zuo.h} should also be used by
the embedding application. Documentation for the embedding API is
provided as comments within @filepath{zuo.h}.


@section{Zuo Datatypes}

Zuo's kernel supports the following kinds of data:

@itemlist[

 @item{booleans;}

 @item{integers as 64-bit two's complement with modular arithmetic}

 @item{strings as byte strings (optionally prefixed with @litchar{#}
       and with @litchar{\n}, @litchar{\r}, @litchar{\t},
       @litchar{\"}, @litchar{\\}, and octal escapes);}

 @item{symbols, both interned (never garbage collected) and
       uninterned;}

 @item{lists;}

 @item{@deftech{hash tables}, which are symbol-keyed persistent maps
       (and don't actually employ hashing internally);}

 @item{procedures, including first-class continuations reified as
        procedures;}

 @item{@deftech{variables}, which are named, set-once, single-valued
       containers;}

 @item{@deftech{opaque objects} that pair a key and a value, where the
       value can be accessed only by supplying the key (which is
       typically kept private using lexical scope); and}

 @item{@deftech{handles}, which represent system resources like files
       or processes.}

]

Notable omissions include floating-point numbers, characters, Unicode
strings, and vectors. Paths are represented using byte strings (with
an implied UTF-8 encoding for Windows wide-character paths).

See @secref["reader"] for information on reading literal values as
S-expression.


@section{Zuo Implementation and Macros}

The @filepath{zuo.c} source implements @racketmodname[zuo/kernel],
which is a syntactically tiny language plus around 100 primitive
procedures. Since Zuo is intended for scripting, it's heavy on
filesystem, I/O, and process primitives, and almost half of the
primitives are for those tasks (while another 1/3 of the primitives are
just for numbers, strings, and @tech{hash tables}).

Zuo data structures are immutable except for @tech{variable} values,
and even a variable is set-once; attempting to get a value of the
variable before it has been set is an error. (Variables are used to
implement @racket[letrec], for example.) Zuo is not purely functional,
because it includes imperative I/O and errors, but it actively
discourages in-process state by confining imperative actions to
external interactions. Along those lines, an error in Zuo always
terminates the program; there is no exception system (and therefore no
way within Zuo to detect early use of an unset variable).

The @racketmodname[zuo] language is built on top of
@racketmodname[zuo/kernel], but not directly. There's an internal
``looper'' language that just adds simple variants of @racket[letrec],
@racket[cond], and @racket[let*], because working without those is
especially tedious. Then there's an internal ``stitcher'' language
that is the only use of the ``looper'' language; it adds its own
@racket[lambda] (with implicit @racket[begin]) @racket[let] (with
multiple clauses), @racket[let*], @racket[letrec] (with multiple
binding clauses), @racket[and], @racket[or], @racket[when],
@racket[unless], and a kind of @racket[define] and @racket[include].

Two macro implementations are built with the ``stitcher'' layer. One
is based on the same set-of-scopes model as Racket, and that macro
system is used for and provided by @racketmodname[zuo/hygienic]. The
other is non-hygienic and uses a less expressive model of scope, which
a programmer might notice if, say, writing macro-generating macros;
that macro system is used for and provided by @racketmodname[zuo],
because it's a lot faster and adequate for most scripting purposes.
The two macro system implementations are mostly the same source, which
is parameterized over the representation of scope and binding, and
implemented through a combination of @racketmodname[zuo/datum] and the
``stitcher'' layer's @racket[include].

Naturally, you can mix and match @racketmodname[zuo] and
@racketmodname[zuo/hygienic] modules in a program, but you can't use
macros from one language within the other language. More generally,
Zuo defines a @hash-lang[] protocol that lets you build arbitrary new
languages (from the character/byte level), as long as they ultimately
can be expressed in @racketmodname[zuo/kernel].


@section[#:tag "module-protocol"]{Zuo Module Protocol}

At Zuo's core, a module is represented as a @tech{hash table}. There
are no constraints on the keys of the hash table, and different layers
on top of the core module protocol can assign meanings to keys. For
example, the @racketmodname[zuo] and @racketmodname[zuo/hygienic]
layers use a common key for accessing @racket[provide]d bindings, but
different keys for propagating binding information for macro
expansions.

The core module system assigns a meaning to one key,
@racket['read-and-eval], which is supplied by a module that implements
a @hash-lang[] language. The value of @racket['read-and-eval] is a
procedure of three arguments:

@itemlist[

 @item{a string for the text of a module using the language,}

 @item{a position within the string that starts a module body after
       @hash-lang[] and the language name, and}

 @item{a module path that will be mapped to the result of evaluating
       the module (i.e., the path to the text's source).}

]

The procedure must return a hash table representing the evaluated
module. A @racket['read-and-eval] procedure might use
@racket[string-read] to read input, it might use
@racket[kernel-eval] to evaluate read or generated terms, and it might
use @racket[module->hash] to access other modules in the process of
parsing a new module---but a @racket['read-and-eval] procedure is
under no obligation to use any of those.

A call @racket[(module->hash _M)] primitive checks whether the module
@racket[_M] is already loaded and returns its hash table if so. The
@racket[zuo/kernel] module is always preloaded, but other modules may
be preloaded in an image that was created by
@racket[dump-image-and-exit]. If a module @racket[_M] is not already
loaded, @racket[module->hash] reads the beginning of @racket[_M]'s
source to parse the @hash-lang[] specification and get the path of the
language module @racket[_L]; a recursive call @racket[(module->hash
_L)] gets @racket[_L], and @racket[_L]'s @racket['read-and-eval]
procedure is applied to the source of @racket[_M] to get @racket[_M]'s
representation as a hash. That representation is both recorded for
future use and returned from the original @racket[(module->hash _M)]
call.

The Zuo startup sequence assigns a meaning to a second key in a
module's hash table: @racket['submodules]. The value of
@racket['submodules] should be a hash table that maps keys to thunks,
each representing a submodule. When Zuo runs an initial script, it
looks for a @racket['main] submodule and runs it (i.e., calls the
thunk) if present.

The @racketmodname[zuo], @racketmodname[zuo/base], and
@racketmodname[zuo/hygienic] languages do not specify how their
provided-variable information is represented in a module hash table,
but they do specify that @racket['dynamic-require] is mapped to the
@racket[dynamic-require] function, and then @racket[dynamic-require]
can be used to access provided values.

@history[#:changed "1.2" @elem{Added the @racket['dynamic-require] key
        for @racketmodname[zuo] and related languages.}]

@section[#:tag "paths"]{Path Handling}

Working with paths is a central issue in many scripting tasks, and
it's certainly a key problem for a build system. Zuo embeds some
specific choices about how to work with paths:

@itemlist[

 @item{Zuo relies on syntactic normalization of paths. For example,
       starting with @filepath{a/b} and building @filepath{../c} from
       there produces the path @filepath{a/c}, even if @filepath{a/b}
       on the filesystem is a symbolic link to to the relative path
       @filepath{x/y/z}---in which case the filesystem would resolve
       @filepath{a/b/../c} the same as @filepath{a/x/y/z/../c}, which
       is @filepath{a/x/y/c} and not @filepath{a/c}.

       In short, mixing directory symbolic links with Zuo's path
       functions can be different than what the filesystem would do,
       so take care to avoid cases that would not work. Symbolic links
       to files will not create problems, so consider just never using
       directory links.}

 @item{There is no way to change the working directory of the Zuo
       process. Having a fixed current directory means that relative
       paths work in many more situations than they would otherwise.
       Relative paths are communicated to system facilities still in
       relative form, leaving it up to the operating system to resolve
       the path relative to the current working directory.

       When starting a subprocess, you can pick the working directory
       for the subprocess. In that case, you must take care to adjust
       relative paths communicated to the process, and
       @racket[find-relative-path] can help.}

 @item{Zuo uses and propagates relative paths as much as possible.
       This convention is partly enabled by the fact that the working
       directory cannot change within the Zuo process. It also helps
       avoid trouble from a mismatch between syntactic and
       filesystem-based path resolution, as might be created with
       symbolic directory links; for example, even if you used
       symbolic links or one of multiple filesystem mounts to access a
       Zuo working tree, staying within that tree avoids complications
       with the path that reaches the tree.

       The way that you start a Zuo script affects the script's
       operation in terms of absolute or relative paths. If you start
       a Zuo script with a relative path, such as @exec{zuo
       scripts/go.zuo}, the @racket[quote-module-path] form will
       report a relative path for the enclosing script. If you start
       it with an absolute path, such as @exec{zuo
       /home/racket/scripts/go.zuo}, then @racket[quote-module-path]
       reports an absolute path. Similarly, with
       @racketmodname[zuo/build], when you use a relative path to
       refer to a dependency, information about the dependency
       can be recorded in relative form, but referring to a dependency
       with an absolute path means that information is recorded with
       an absolute path (even if that could be made relative to the
       dependent target's path).}

 @item{The @racket[build/build] library encourages an explicit
       distinction between ``source'' and ``build'' directories,
       neither of which necessarily corresponds to the current working
       directory. This distinction, along with the fact that the
       working directory doesn't change, helps to create composable
       build scripts. See @secref["build-targets"] for more
       information.}

]
