#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "runtime"]{Environment and Runtime Information}

@defproc[(getenv [name string?]) (or/c string? #f)]{

Gets the value of an operating system environment variable. The
@racket[name] argument cannot contain a null character; if an
environment variable named by @racket[name] exists, its value is
returned (as a string); otherwise, @racket[#f] is returned.}

@defproc[(putenv [name string?] [value string?]) boolean?]{

Sets the value of an operating system environment variable. The
@racket[name] and @racket[value] arguments are strings that cannot
contain a null character; the environment variable named by
@racket[name] is set to @racket[value]. The return value is
@racket[#t] if the assignment succeeds, @racket[#f] otherwise.}

@defproc[(system-type [mode (or/c 'os 'word 'gc 'link 'so-suffix 'machine)
                            'os])
         (or/c symbol? string? bytes? exact-positive-integer?)]{

Returns information about the operating system, build mode, or machine
for a running Racket.

In @indexed-racket['os] mode,
 the possible symbol results are:

@itemize[
@item{@indexed-racket['unix]}
@item{@indexed-racket['windows]}
@item{@indexed-racket['macosx]}
]

In @indexed-racket['word] mode, the result is either @racket[32] or
@racket[64] to indicate whether Racket is running as a 32-bit program
or 64-bit program.

In @indexed-racket['gc] mode,
the possible symbol results are:

@itemize[
@item{@indexed-racket['cgc]}
@item{@indexed-racket['3m]}
]

In @indexed-racket['link] mode, the possible symbol results are:

@itemize[
@item{@indexed-racket['static] (Unix)}
@item{@indexed-racket['shared] (Unix)}
@item{@indexed-racket['dll] (Windows)}
@item{@indexed-racket['framework] (Mac OS X)}
]

Future ports of Racket may expand the list of @racket['os],
@racket['gc], and @racket['link] results.

In @indexed-racket['so-suffix] mode, then the result is a byte string
that represents the file extension used for shared objects on the
current platform. The byte string starts with a period, so it is
suitable as a second argument to @racket[path-replace-suffix].

In @indexed-racket['machine] mode, then the result is a string, which
contains further details about the current machine in a
platform-specific format.}


@defproc[(system-language+country) string?]{

Returns a string to identify the current user's language and
country.

On Unix and Mac OS X, the string is five characters: two lowercase
ASCII letters for the language, an underscore, and two uppercase ASCII
letters for the country. On Windows, the string can be arbitrarily
long, but the language and country are in English (all ASCII letters
or spaces) separated by an underscore.

On Unix, the result is determined by checking the
@indexed-envvar{LC_ALL}, @indexed-envvar{LC_TYPE}, and
@indexed-envvar{LANG} environment variables, in that order (and the
result is used if the environment variable's value starts with two
lowercase ASCII letters, an underscore, and two uppercase ASCII
letters, followed by either nothing or a period). On Windows and
Mac OS X, the result is determined by system calls.}


@defproc[(system-library-subpath [mode (or/c 'cgc '3m #f)
                                       (system-type 'gc)])
         path?]{

Returns a relative directory path. This string can be used to build
paths to system-specific files. For example, when Racket is running
on Solaris on a Sparc architecture, the subpath starts
@racket["sparc-solaris"], while the subpath for Windows on an i386
architecture starts @racket["win32\\i386"].

The optional @racket[mode] argument specifies the relevant
garbage-collection variant, which one of the possible results of
@racket[(system-type 'gc)]: @racket['cgc] or @racket['3m]. It can also
be @racket[#f], in which case the result is independent of the
garbage-collection variant.}


@defproc[(version) (and/c string? immutable?)]{

Returns an string indicating the currently executing version of
Racket.}


@defproc[(banner) (and/c string? immutable?)]{

Returns an immutable string for Racket's start-up banner text (or the
banner text for an embedding program, such as GRacket). The banner string
ends with a newline.}


@defparam[current-command-line-arguments argv (vectorof (and/c string? immutable?))]{

A @tech{parameter} that is initialized with command-line arguments when
Racket starts (not including any command-line arguments that were
treated as flags for the system).}


@defparam[current-thread-initial-stack-size size exact-positive-integer?]{

A @tech{parameter} that provides a hint about how much space to reserve for a
newly created thread's local variables. The actual space used by a
computation is affected by @tech{JIT} compilation, but it is
otherwise platform-independent.}


@defproc[(vector-set-performance-stats! [results (and/c vector?
                                                        (not/c immutable?))]
                                        [thd (or/c thread? #f) #f])
         void?]{

Sets elements in @racket[results] to report current performance
statistics. If @racket[thd] is not @racket[#f], a particular set of
thread-specific statistics are reported, otherwise a different set of
global statics are reported.

For global statistics, up to @math{11} elements are set in the vector,
starting from the beginning. If @racket[results] has @math{n} elements
where @math{n < 11}, then the @math{n} elements are set to the first
@math{n} performance-statistics values. The reported statistics values
are as follows, in the order that they are set within
@racket[results]:

 @itemize[

  @item{@racket[0]: The same value as returned by
  @racket[current-process-milliseconds].}

  @item{@racket[1]: The same value as returned
  by @racket[current-milliseconds].}

  @item{@racket[2]: The same value as returned
  by @racket[current-gc-milliseconds].}

  @item{@racket[3]: The number of garbage collections performed since
  start-up.}

  @item{@racket[4]: The number of thread context switches performed since
  start-up.}

  @item{@racket[5]: The number of internal stack overflows handled since
  start-up.}

  @item{@racket[6]: The number of threads currently scheduled for
  execution (i.e., threads that are running, not suspended, and not
  unscheduled due to a synchronization).}

  @item{@racket[7]: The number of syntax objects read from compiled code
  since start-up.}

  @item{@racket[8]: The number of hash-table searches performed. When
  this counter reaches the maximum value of a @tech{fixnum}, it
  overflows to the most negative @tech{fixnum}.}

  @item{@racket[9]: The number of additional hash slots searched to
  complete hash searches (using double hashing).  When this counter
  reaches the maximum value of a @tech{fixnum}, it overflows to the
  most negative @tech{fixnum}.}

  @item{@racket[10]: The number of bytes allocated for machine code
  that is not reported by @racket[current-memory-use].}

 ]

For thread-specific statistics, up to @math{4} elements are set in the
vector:

 @itemize[

  @item{@racket[0]: @racket[#t] if the thread is running, @racket[#f]
  otherwise (same result as @racket[thread-running?]).}

  @item{@racket[1]: @racket[#t] if the thread has terminated,
  @racket[#f] otherwise (same result as @racket[thread-dead?]).}

  @item{@racket[2]: @racket[#t] if the thread is currently blocked on a
  synchronizable event (or sleeping for some number of milliseconds),
  @racket[#f] otherwise.}

  @item{@racket[3]: The number of bytes currently in use for the
  thread's continuation.}

 ]
}
