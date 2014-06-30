#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "overview"]{Overview}

The Racket run-time system is responsible for the implementation
primitive datatypes such as numbers and strings, the evaluation and/or
JIT compilation of Racket bytecode, the macro expansion and
compilation of Racket from source to bytecode, the allocation and
reclamation of memory used during evaluation, and the scheduling
concurrent threads and parallel tasks.

Much of the language provided by @racketmodname[racket/base] is
implemented in a more primitive dialect of Racket that is provided by
the run-time system. Future versions of Racket are likely to move
macro expansion, compilation, and many ``primitive'' functions into
such Racket-implemented libraries, instead of having them built into
the run-time system.

@section{``Scheme'' versus ``Racket''}

The old name for Racket was ``PLT Scheme,'' and the core compiler and
run-time system used to be called ``MzScheme.'' The old names are
entrenched in Racket internals, to the point that most C bindings
defined in this manual start with @cpp{scheme_}. In principle, they
all should be renamed to start @cpp{racket_}.

@; ----------------------------------------------------------------------

@section{Building Racket from Source}

The normal Racket distribution includes @filepath{.rkt} sources for
collection-based libraries. After modifying library files, run
@exec{raco setup} (see @secref[#:doc '(lib
"scribblings/raco/raco.scrbl") "setup"]) to rebuild installed
libraries.

The normal Racket distribution does not include the C sources for
Racket's run-time system. To build Racket from scratch, download a
source distribution from @url{http://download.racket-lang.org};
detailed build instructions are in the @filepath{README} file in the
top-level @filepath{src} directory. You can also get the latest
sources from the @tt{git} repository at
@url{https://github.com/plt/racket}, but beware that the repository is
one step away from a normal source distribution, and it provides build
modes that are more suitable for developing Racket itself; see
@filepath{INSTALL.txt} in the @tt{git} repository for more
information.

@; ----------------------------------------------------------------------

@section[#:tag "CGC versus 3m"]{CGC versus 3m}

Before mixing any C code with Racket, first decide whether to use the
@bold{3m} variant of Racket, the @bold{CGC} variant of Racket, or
both:

@itemize[

@item{@bold{@as-index{3m}} : the main variant of Racket, which uses
  @defterm{precise} garbage collection and requires explicit
  registration of pointer roots and allocation shapes. The precise
  garbage collector may move its objects in memory during a
  collection.}

@item{@bold{@as-index{CGC}} : the original variant of Racket, where
  memory management depends on a @defterm{conservative} garbage
  collector. The conservative garbage collector can automatically find
  references to managed values from C local variables and (on some
  platforms) static variables, and it does not move allocated
  objects.}

]

At the C level, working with CGC can be much easier than working with
3m, but overall system performance is typically better with 3m.

@; ----------------------------------------------------------------------

@section[#:tag "embedding-and-extending"]{Embedding and Extending Racket}

The Racket run-time system can be embedded into a larger program; see
@secref["embedding"] for more information. As an alternative to
embedding, the @exec{racket} executable can also be run in a
subprocess, and that choice may be better for many purposes. On
Windows, @seclink["top" #:doc '(lib "mzcom/mzcom.scrbl") #:indirect?
#t]{MzCom} provides another option.

The Racket run-time system @seclink["Writing Racket Extensions"]{can
be extended} with new C-implemented functions. Historically, writing
an extension could provide performance benefits relative to writing
pure Racket code, but Racket performance has improved to the point
that performance benefits of writing C code (if any) are usually too
small to justify the maintenance effort. For calling functions that
are provided by a C-implemented library, meanwhile, using with
@seclink["top" #:doc '(lib
"scribblings/foreign/foreign.scrbl")]{foreign-function interface}
within Racket is a better choice than writing an extension of Racket
to call the library.

@; ----------------------------------------------------------------------

@section[#:tag "places"]{Racket and Places}

Each Racket @|tech-place| corresponds to a separate OS-implemented
thread. Each place has its own memory manager. Pointers to GC-managed
memory cannot be communicated from one place to another, because such
pointers in one place are invisible to the memory manager of another
place.

When @|tech-place| support is enabled, static variables at the C level
generally cannot hold pointers to GC-managed memory, since the static
variable may be used from multiple places.  For some OSes, a static
variable can be made thread-local, in which case it has a different
address in each OS thread, and each different address can be
registered with the GC for a given place.

In an @seclink["embedding"]{embedding application}, the OS thread that
originally calls @cpp{scheme_basic_env} is the OS thread of the
original place. When @cpp{scheme_basic_env} is called a second time to
reset the interpreter, it can be called in an OS thread that is
different from the original call to
@cpp{scheme_basic_env}. Thereafter, the new thread is the OS thread
for the original place.

@; ----------------------------------------------------------------------

@section{Racket and Threads}

Racket implements threads for Racket programs without aid from the
operating system, so that Racket threads are cooperative from the
perspective of C code. On Unix, stand-alone Racket uses a single
OS-implemented thread. On Windows and Mac OS X, stand-alone
Racket uses a few private OS-implemented threads for background
tasks, but these OS-implemented threads are never exposed by the
Racket API.

Racket can co-exist with additional OS-implemented threads, but the
additional OS threads must not call any @cpp{scheme_} function.  Only
the OS thread representing a particular @|tech-place| can call
@cpp{scheme_} functions. (This restriction is stronger than saying all
calls for a given place must be serialized across threads. Racket
relies on properties of specific threads to avoid stack overflow and
garbage collection.) In an @seclink["embedding"]{embedding
application}, for the original place, only the OS thread used to call
@cpp{scheme_basic_env} can call @cpp{scheme_} functions. For any other
place, only the OS thread that is created by Racket for the place can
be used to call @cpp{scheme_} functions.

See @secref["threads"] for more information about threads, including
the possible effects of Racket's thread implementation on extension
and embedding C code.

@; ----------------------------------------------------------------------

@section[#:tag "im:unicode"]{Racket, Unicode, Characters, and Strings}

A character in Racket is a Unicode code point. In C, a character
value has type @cppi{mzchar}, which is an alias for @cpp{unsigned} ---
which is, in turn, 4 bytes for a properly compiled Racket. Thus, a
@cpp{mzchar*} string is effectively a UCS-4 string.

Only a few Racket functions use @cpp{mzchar*}. Instead, most
functions accept @cpp{char*} strings. When such byte strings are to be
used as a character strings, they are interpreted as UTF-8
encodings. A plain ASCII string is always acceptable in such cases,
since the UTF-8 encoding of an ASCII string is itself.

See also @secref["im:strings"] and @secref["im:encodings"].

@; ----------------------------------------------------------------------

@section[#:tag "im:intsize"]{Integers}

Racket expects to be compiled in a mode where @cppi{short} is a
16-bit integer, @cppi{int} is a 32-bit integer, and @cppi{intptr_t} has
the same number of bits as @cpp{void*}. The @cppi{long} type can match
either @cpp{int} or @cpp{intptr_t}, depending on the platform.
The @cppi{mzlonglong} type has
64 bits for compilers that support a 64-bit integer type, otherwise it
is the same as @cpp{intptr_t}; thus, @cpp{mzlonglong} tends to match
@cpp{long long}. The @cppi{umzlonglong} type is the unsigned version
of @cpp{mzlonglong}.
