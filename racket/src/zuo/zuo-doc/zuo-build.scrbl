#lang scribble/manual
@(require scribble/bnf
          (for-label zuo-doc/fake-zuo
                     racket/contract/base)
          "defzuomodule.rkt")

@(define shake-url "https://shakebuild.com/")

@title[#:tag "zuo-build"]{Zuo as a @exec{make} Replacement}

@defzuomodule[zuo/build]

The @racketmodname[zuo/build] library is modeled on @exec{make} and
@hyperlink[shake-url]{Shake} for tracking dependencies
and build steps. The library has two layers:

@itemlist[

 @item{The core @tech{target} datatype and build engine, as reflected
       by functions like @racket[target] and @racket[build].}

 @item{A makefile-like, declarative form for dependencies as
       implemented by the @racket[make-targets] function.}

]

A @tech{target} represents either an input to a build (such as a
source file) or a generated output, and a target can depend on any
number of other targets. A target's output is represented by a
string that is normally an SHA-256 hash: more precisely, it is
represented by a value satisfying the predicate @racket[sha256?].
The @racket[build] procedure
records hashes and dependencies in a database located alongside
non-input targets, so it can avoid rebuilding targets when nothing has
changed since the last build. Unlike @exec{make}, timestamps are used
only as a shortcut to avoiding computing the SHA-256 of a file (i.e., if
the timestamp has not changed, the SHA-256 result is assumed to be
unchanged).

``Recursive make'' is encouraged in the sense that a target's build
rule can call @racket[build] to start a nested build, or it can call
@racket[build/dep] to build or register a dependency that is
discovered in the process of building.

Here's an example of a Zuo script to build @filepath{demo} by
compiling and linking @filepath{main.c} and @filepath{helper.c}:

@racketblock[
@#,hash-lang[] zuo

(provide-targets targets-at)

(define (targets-at at-dir vars)
  (define demo (at-dir (.exe "demo")))

  (define main.c (at-source "main.c"))
  (define main.o (at-dir (.c->.o "main.c")))

  (define helper.c (at-source "helper.c"))
  (define helper.o (at-dir (.c->.o "helper.c")))

  (make-targets
   `([:target ,demo (,main.o ,helper.o)
              ,(lambda (dest token)
                 (c-link dest (list main.o helper.o) vars))]
     [:target ,main.o (,main.c)
              ,(lambda (dest token)
                 (c-compile dest main.c vars))]
     [:target ,helper.o (,helper.c)
              ,(lambda (dest token)
                 (c-compile dest helper.c vars))]
     [:target clean ()
              ,(lambda (token)
                 (for-each rm* (list main.o helper.o demo)))])))
]

Although the @racket[make-targets] function takes a makefile-like
description of targets and dependencies, this script is still much
more verbose than a Unix-specific makefile that performs the same
task. Zuo is designed to support the kind of syntactic abstraction
that could make this script compact, but the current implementation is
aimed at build tasks that are larger and more complex. In those cases,
it's not just a matter of dispatching to external tools like a C
compiler, and most Zuo code ends up in helper functions and libraries
outside the @racket[make-targets] form.

@section[#:tag "make-target"]{Creating Targets}

Construct a @deftech{target} with either @racket[input-file-target]
(given a file name), @racket[input-data-target] (given a value whose
@racket[~s] form is hashed), or @racket[target] (given a filename for a
real target or a symbol for a @tech{phony} target).

Only a target created with @racket[target] can have dependencies, but
they are not specified when @racket[target] is called, because
computing dependencies for a target may involve work that can be
skipped if the target isn't needed. Instead, @racket[target] takes a
@racket[_get-rule] procedure that will be called if the dependencies
are needed. The @racket[_get-rule] procedure returns up to three
results in a @racket[rule] record: a list of dependencies; the hash of
an already-built version of the target, if one exists, where
@racket[file-sha256] is used by default; and a @racket[_rebuild]
procedure that is called if the returned hash, the hash of
dependencies (rebuilt if needed), and recorded results from a previous
build together determine that a rebuild is needed.

When a target's @racket[_rebuild] function is called, it optionally
returns a hash for the result of the build if the target's
@racket[rule] had one, otherwise @racket[file-sha256] is used to get a
result hash. Either way, it's possible that the result hash is the
same the one returned by @racket[_get-rule]; that is, maybe a
dependency of the target changed, but the change turned out not to
affect the built result. In that case, rebuilding for other targets
that depend on this one can be short-circuited.

Finally, in the process of building a target, a @racket[_rebuild]
procedure may discover additional dependencies. A discovered
dependency sent to @racket[build/dep] is recorded as a dependency of
the target in addition to the ones that were reported by
@racket[_get-deps]. Any changes in these additional targets trigger a
rebuild of the target in the future. Meanwhile, the build system
assumes that if none of the dependencies change, then the set of
additional dependencies discovered by @racket[_rebuild] would be the
same; that assumption allows the build system to skip
@racket[_rebuild] and its discoveries if none of the dependencies have
changed.

A @deftech{phony} target is like a regular target, but one that always
needs to be rebuilt. A typical use of a phony target is to give a name
to a set of ``top-level'' targets or to implement an action along the
lines of @exec{make install}. Create a phony target with
@racket[target] and a symbol name.

A target can declare multiple outputs by specifying additional outputs
in a @racket['co-outputs] option. The target's @racket[_rebuild]
procedure will be called if any of the additional outputs are missing
or not consistent with the result of an earlier build.

In many cases, a plain path string can be used as a target as a
shorthand for applying @racket[input-file-target] to the path string.

@section[#:tag "build-targets"]{Building Targets}

There is no global list of targets that @racket[build] draws from.
Instead, @racket[build] starts with a given target, and it learns
about other targets a @racket[_get-dep] procedures return them and as
@racket[_rebuild] procedures expose them via @racket[build/dep]. If
@racket[build] discovers multiple non-input targets with the same
filename, then it reports an error.

The @racket[build/command-line] function is a convenience to implement
get @exec{make}-like command-line handling for building targets. The
@racket[build/command-line] procedure takes a list of targets, and it
calls @racket[build] on one or more of them based on command-line
arguments (with help from @racket[find-target]).

All relative paths are considered relative to the start-time current
directory. This convention works well for running a Zuo script that's
in a source directory while the current directory is the build
directory, as long as the script references source files with
@racket[at-source] to make them relative to the script. For
multi-directory builds, a good convention is for each directory to
have a script that exports a @racketidfont{targets-at} procedure,
where @racketidfont{targets-at} takes an @racket[_at-dir] procedure
(supplied as just @racket[build-path] by default) to apply to each
target path when building a list of targets, and a hash table of
variables (analogous to variables that a makefile might provide to
another makefile via @exec{make} arguments).

As a further convenience following the @racketidfont{targets-at}
model, the @racket[provide-targets] form takes an identifier for such a
@racketidfont{targets-at} procedure, and it both exports
@racketidfont{targets-at} and creates a @racket[main] @tech{submodule}
that calls @racket[build/command-line*] on with the
@racketidfont{targets-at} procedure.

As a naming convention, consider using @filepath{main.zuo} in a
directory where build results are intended to be written, but use
@filepath{build.zuo} in a source directory that is intended to be
(potentially) separate from the build directory. In other words, use
@filepath{main.zuo} as a replacement for @filepath{Makefile} and
@filepath{build.zuo} as a replacement for @filepath{Makefile.in} in a
@exec{configure}-style build. You may even have a @exec{configure}
script that generates a @filepath{main.zuo} script in a build
directory so that @exec{zuo .} is a replacement for @exec{make}.
The generated @filepath{main.zuo} could import the source directory's
@filepath{build.zuo} and calls @racket[build/command-line*] on with
the imported @racketidfont{targets-at} procedure plus
@racket[at-source]:

@racketblock[
@#,hash-lang[] @#,racketmodname[zuo]
(require @#,elem[@racketvalfont{"}@nonterm{srcdir}@racketvalfont{/build.zuo"}])
(build/command-line* targets-at at-source)
]

However, correctly encoding @nonterm{srcdir} can be tricky when
working from something like a shell configure script or batch file to
generate @filepath{main.zuo}. You may find it easier to write the path
to a separate file using a shell-variable assignment syntax, and then
have the generated @filepath{main.zuo} read from that file. The
@racket[bounce-to-targets] form implements that pattern. For example,
if @filepath{Mf-config} is written in the same directory with a
@litchar{srcdir=} line to specify the source directory (where no
escapes are needed for the path after @litchar{=}), then a
@filepath{mzin.zuo} of them form

@racketblock[
@#,hash-lang[] @#,racketmodname[zuo]
(bounce-to-targets "Mf-config" 'srcdir "build.zuo")
]

reads @filepath{Mf-config} to find and dispatch to
@filepath{build.zuo} in the same way as the earlier example module.


@section{Recording Results}

Build results are stored in a @filepath{_zuo.db} file in the same
directory as a target (by default). Cached SHA-256 results with associated
file timestamps are stored in a @filepath{_zuo_tc.db} in the same
directory (i.e., the cached value for dependency is kept with the
target, which is in a writable build space, while an input-file target
might be in a read-only source space). A target's options can specify
an alternative directory to use for @filepath{_zuo.db} and
@filepath{_zuo_tc.db}. Timestamp recording in @filepath{_zuo_tc.db}
is disabled if the @envvar{SOURCE_DATE_EPOCH} environment variable is set.

In the unfortunate case that a @filepath{_zuo.db} or
@filepath{_zuo_tc.db} file gets mangled, then it may trigger an error
that halts the build system, but the @filepath{_zuo.db} or
@filepath{_zuo_tc.db} file will be deleted in reaction to the error.
Another attempt at the build should recover, while perhaps rebuilding
more than it would have otherwise, since the result of previous builds
might have been lost.

Specify a location for the @filepath{_zuo.db} and
@filepath{_zuo_tc.db} files associated with a target via the
@racket['db-dir] target option. The @racket[make-targets] function
recognizes as @racket[:db-dir] clause to set the option for all of the
targets that it creates.

@section{Parallelism}

A build runs in a @tech{threading context}, so a target's
@racket[_get-deps] or @racket[_rebuild] procedure can use
@racket[thread-process-wait] can be used to wait on a process. Doing
so can enable parallelism among targets, depending on the
@racket['jobs] option provided to @racket[build] or
@racket[build/command-line], a @DFlag{jobs} command-line argument
parsed by @racket[build/command-line], a jobserver configuration as
provided by GNU make and communicated through the @envvar{MAKEFLAGS}
environment variable, or the @envvar{ZUO_JOBS} environment variable.

When calling @racket[build] for a nested build from a target's
@racket[_get-deps] or @racket[_rebuild] procedures, supply the
@tech{build token} that is passed to @racket[_get-deps] to the
@racket[build] call. That way, parallelism configured for the
enclosing build will be extended to the nested build.


@section{Build API}


@defproc[(target? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is @tech{target}, @racket[#f]
otherwise.}


@defproc[(target-name [t target?]) (or/c symbol? path-string?)]{

Returns the name of a target, which is a path for most targets, but a
symbol for an input-data target or a @tech{phony} target.}


@defproc[(target-path [t target?]) path-string?]{

The same as @racket[target-name] for a target whose name is a path,
and an error for other targets.}

@defproc[(target-shell [t target?]) string?]{

Composes @racket[target-path] with @racket[string->shell]. Use this
when getting a target name to include in a shell command.}


@defproc[(input-file-target [path path-string?]) target?]{

Creates a @tech{target} that represents an input file. An input-file
target has no build procedure, and it's state is summarized as a hash
via @racket[file-sha256].}


@defproc[(input-data-target [name symbol?] [content any/c]) target?]{

Similar to @racket[input-file-target] for a would-be file that
contains @racket[(~s content)].

The result of @racket[(symbol->string name)] must be distinct among
all the input-data dependencies of a particular target, but it does
not need to be globally unique.}


@defproc*[([(target [name path-string?]
                    [get-deps (path-string? token? . -> . rule?)]
                    [options hash? (hash)])
            target?]
           [(target [name symbol?]
                    [get-deps (token? . -> . phony-rule?)]
                    [options hash? (hash)])
            target?])]{

Creates a @tech{target} that can have dependencies. If @racket[name]
is a path string, then it represents a file build target whose results
are recorded to avoid rebuilding. If @racket[name] is a symbol, then
it represents a @tech{phony} target that is always rebuilt.

In the case of a file target, @racket[get-deps] receives @racket[name]
back, because that's often more convenient for constructing a target
when applying an @racket[_at-dir] function to create @racket[name].

The @deftech{build token} argument to @racket[get-deps] represents the
target build in progress. It's useful with @racket[file-sha256] to take
advantage of caching, with @racket[build/dep] to report
discovered targets, and with @racket[build/no-dep] or @racket[build].

The following keys are recognized in @racket[options]:

@itemlist[

@item{@racket['co-outputs] mapped to a list of path strings: paths
      that are also generated by the target in addition to
      @racket[name] when @racket[name] is a path string; the target's
      build function will be called if the combination of
      @racket[name] and these files is out-of-date.}

@item{@racket['precious?] mapped to any value: if non-@racket[#f] for
      a non-phony target, @racket[name] is not deleted if the
      @racket[get-deps] function or its result's @racket[_rebuild]
      function fails.}

@item{@racket['command?] mapped to any value: if non-@racket[#f], when
      @racket[build/command-line] runs the target as the first one
      named on the command line, all arguments from the command line
      after the target name are provided @racket[_get-deps] as
      additional arguments. When building a target directly instead
      of through @racket[build/command-line], use
      @racket[command-target->target] to supply arguments.}

@item{@racket['noisy?] mapped to any value: if non-@racket[#f], then a
      message prints via @racket[alert] whenever when the target is
      found to be already up to date.}

@item{@racket['quiet?] mapped to any value: if non-@racket[#f], then
      even when @racket[build] runs the target directly or as the
      dependency of a @tech{phony} target, it does not print a message
      via @racket[alert] when the target is up to date, unless the
      target is also noisy. When a phony target is quiet, it builds
      its dependencies as quiet.}

@item{@racket['eager?] mapped to any value: if non-@racket[#f], then
      the target's build step is not run in a separate thread, which
      has the effect of ordering the build step before others that do
      run in a separate thread.}

@item{@racket['db-dir] mapped to a path or @racket[#f]: if
      non-@racket[#f], build information for the target is stored in
      @filepath{_zuo.db} and @filepath{_zuo_tc.db} files in the
      specified directory, instead of the directory of @racket[path].}

]}

@deftogether[(
@defproc[(rule [dependencies (listof (or/c target? path-string?))]
               [rebuild (or/c (-> (or/c sha256? any/c)) #f) #f]
               [sha256 (or/c sha256? #f) #f])
         rule?]
@defproc[(rule? [v any/c]) boolean?]
)]{

The @racket[rule] procedure combines the three results expected from a
procedure passed to @racket[target]. See @secref["make-target"].

A path string can be reported as a dependency in
@racket[dependencies], in which case it is coerced to a target using
@racket[input-file-target]. If @racket[sha256] is @racket[#f],
@racket[file-sha256] is used to compute the target's current hash, and
@racket[rebuild] is not expected to return a hash. If @racket[sha256] is
not @racket[#f], then if @racket[rebuild] is called, it must return a
new hash.}


@deftogether[(
@defproc[(phony-rule [dependencies (listof (or/c target? path-string?))]
                     [rebuild (-> any/c)])
         phony-rule?]
@defproc[(phony-rule? [v any/c]) boolean?]
)]{

The @racket[phony-rule] procedure combines the two results expected
from a procedure passed to @racket[target] to create a @tech{phony}
target. Compared to the non-phony protocol, the result SHA-256 is
omitted.}

@defproc[(token? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a token representing a target
build, @racket[#f] otherwise.}


@defproc[(build [target (or/c target? path-string? (listof (or/c target? path-string?)))]
                [token (or/c #f token?) #f]
                [options hash? (hash)])
         void?]{

Builds @racket[target] as a fresh build process, independent of any
that might already be running (in the sense described below). A list
of targets as @racket[target] is coerced to a phony target that
depends on the given list.

If @racket[target] is a path, then it is coerced to target via
@racket[input-file-target], but the only effect will be to compute the
file's SHA-256 or error if the file does not exist.

The @racket[options] argument supplies build options, and the
following keys are recognized:

@itemlist[

@item{@racket['jobs] mapped to a positive integer: controls the
      maximum build steps that are allowed to proceed concurrently,
      and this concurrency turns into parallelism when a task uses a
      process and @racket[thread-process-wait]; if @racket['jobs] is
      not mapped, a jobserver is used if found via
      @racket[maybe-jobserver-client]; otherwise, the default is the
      value of the @envvar{ZUO_JOBS} environment variable if it is
      set, @racket[1] if not}

@item{@racket['log?] mapped to any value: enables logging of rebuild
      reasons via @racket[alert] when the value is not @racket[#f];
      logging also can be enabled by setting the
      @envvar{ZUO_BUILD_LOG} environment variable}

]

If @racket[token] is not @racket[#f], it must be a @tech{build token}
that was passed to a target's @racket[_get-deps] to represent a build
in progress (but paused to run this one). The new build process uses
parallelism available within the in-progress build for the new build
process.

Whether or not @racket[token] is @racket[#f], the new build is
independent of other builds in the sense that target results for
others build are not reused for this one. That is, other builds and
this one might check the states of some of the same files, but any
triggered actions are separate, and @tech{phony} targets are similarly
triggered independently. Use @racket[build/dep] or
@racket[build/no-dep], instead, to recursively trigger targets within
the same build.

@history[#:changed "1.1" @elem{Use @racket[maybe-jobserver-client] if
                               @racket['jobs] is not set in
                               @racket[options].}]}


@defproc[(build/dep [target (or target? path-string?)] [token token?]) void?]{

Like @racket[build], but continues a build in progress as represented
by a @racket[token] that was passed to a target's @racket[_get-deps]
or @racket[_rebuild] procedure. Targets reachable through
@racket[target] may have been built or have be in progress already,
for example. After @racket[target] is built, it is registered as a
dependency of the target that received @racket[token] (if the target
is not @tech{phony}).}


@defproc[(build/no-dep [target (or target? path-string?)] [token token?]) void?]{

Like @racket[build/dep] to continue a build in progress, but does not
register a dependency. Using @racket[build/no-dep] has an effect
similar to @hyperlink[shake-url]{Shake}'s ``order only'' dependencies.}


@defproc[(build/command-line [targets (listof target?)] [options hash? (hash)]) void?]{

Parses command-line arguments to build one or more targets in
@racket[targets], where the first one is built by default. The
@racket[options] argument is passed along to @racket[build], but may
be adjusted via command-line flags such as @DFlag{jobs}.

If @racket[options] has a mapping for @racket['args], the value is
used as the command-line arguments to parse instead of
@racket[(hash-ref (system-env) 'args)]. If @racket[options] has a
mapping for @racket['usage], the value is used as the usage options
string.}


@defproc[(build/command-line* [targets-at (procedure? hash? . -> . (listof target?))]
                              [at-dir (path-string? ... . -> . path-string?) build-path]
                              [options hash? (hash)])
         void?]{

Adds a layer of target-variable parsing to
@racket[build/command-line]. Command-line arguments of the form
@nonterm{name}@litchar{=}@nonterm{value} are parsed as variable
assignments, where @nonterm{name} is formed by @litchar{a}-@litchar{z},
@litchar{A}-@litchar{Z}, @litchar{_}, and @litchar{0}-@litchar{9}, but
not starting @litchar{0}-@litchar{9}. These variables can appear
anywhere in the command line and are removed from the argument list
sent on to @racket[build/command-line], but no argument after a
@racket{--} argument is parsed as a variable assignment.

The @racket[targets-at] procedure is applied to @racket[at-dir] and a
hash table of variables, where each variable name is converted to a
symbol and the value is left exact as after @litchar{=}.}

@defproc[(find-target [name string?]
                      [targets (listof target?)]
                      [fail-k procedure? (lambda () (error ....))])
         (or/c target? #f)]{

Finds the first target in @racket[targets] that is a match for
@racket[name], returning @racket[#f] is not match is found. A
@racket[name] matches when it is the same as n entire symbol or path
target name or when it matches a suffix that is preceded by
@litchar{/} or @litchar{\\}. If no match is found, @racket[fail-k]
is called in tail position.}

@defproc[(make-at-dir [path path-string?]) (path-string?  ... . -> . path-string?)]{

Creates a function that is similar to on created by @racket[at-source],
but relative to @racket[path].}

@deftogether[(
@defproc[(command-target? [v any/c]) boolean?]
@defproc[(command-target->target [target command-target?]
                                 [args list?])
         target?]
)]{

The @racket[command-target?] predicate recognizes a target with the
@racket['target?] option, and @racket[command-target->target] converts
such a target to one where @racket[args] are the argument when the
target is built.}

@deftogether[(
@defproc[(file-sha256 [file path-string?] [token (or/c token? #f)]) sha256?]
@defproc[(sha256? [v any/c]) booelan?]
@defthing[sha256-length integer? #:value 64]
)]{

The @racket[file-sha256] procedure returns the SHA-256 hash of the
content of @racket[file] as a 64-character hexadecimal string (thus,
@racket[sha256-length]), or it returns @racket[no-sha256] if
@racket[file] does not exist.

The @racket[sha256?] predicate recognizes @racket[no-sha256] and
strings for which @racket[string-length] returns either
@racket[sha256-length] or a multiple of @racket[sha256-length]. The
later case is used for multi-file targets, which concatenate the
constituent SHA-256 strings.

See also @racket[string-sha256].}

@defthing[no-sha256 sha256? ""]{

The empty string represents a non-existent target or one that needs to
be rebuilt.}

@defform[(provide-targets targets-at-id)]{

Provides @racket[targets-at-id] as @racketidfont{targets-at}, and
creates a @racketidfont{main} submodule that runs
@racket[(build/command-line* targets-at-id build-path)]. A script
using @racket[provide-targets] thus works as a makefile-like script or
as an input to a larger build.}

@defform[(bounce-to-targets config-file-expr key-symbol-expr script-file-expr)]{

Chains to targets from (the path produced by)
@racket[script-file-expr] relative to the directory recorded in (the
file whose path is produced by) @racket[config-file-expr] using the
key (produced by) @racket[key-symbol-expr], supplying the enclosing
script's directory as the target directory.

The path produced by @racket[config-file-expr] is interpreted relative
to the enclosing module. If the path in that file for
@racket[key-symbol-expr] is relative, it is treated relative to the
@racket[config-file-expr] path.

See @secref["build-targets"] for an explanation of how
@racket[bounce-to-targets] is useful. The expansion of
@racket[bounce-to-targets] is roughly as follows:

@racketblock[
  (define config (config-file->hash (at-source config-file-expr)))
  (define at-config-dir (make-at-dir (or (car (split-path config-file)) ".")))
  (define script-file (at-config-dir (hash-ref config key-symbol-expr)
                                     script-file-expr))
  (build/command-line* (dynamic-require script-file 'targets-at)
                       at-source)
]}

@defproc[(make-targets [specs list?]) list?]{

Converts a @exec{make}-like specification into a list of targets for use
with @racket[build]. In this @exec{make}-like specification, extra
dependencies can be listed separately from a build rule, and
dependencies can be written in terms of paths instead of @tech{target}
objects.

Although it might seem natural for this @exec{make}-like specification
to be provided as a syntactic form, typical makefiles use patterns and
variables to generate sets of rules. In Zuo, @racket[map] and similar
are available for generating sets of rules. So, @racket[make-targets]
takes an S-expression representation of the declaration as
@racket[specs], and plain old quasiquote and splicing can be used to
construct @racket[specs].

The @racket[specs] argument is a list of @defterm{lines}, where each
line has one of the following shapes:

@racketblock[
  `[:target ,_path (,_dep-path-or-target ...) ,_build-proc ,_option ...]
  `[:depend ,_path (,_dep-path-or-target ...)]
  `[:target (,_path ...) (,_dep-path-or-target ...) ,_build-proc ,_option ...]
  `[:depend (,_path ...) (,_dep-path-or-target ...)]
  `[:db-dir ,_path]
]

A @racket[':target] line defines a build rule that is implemented by
@racket[_build-proc], while a @racket[':depend] line adds extra
dependencies for a @racket[_path] that also has a @racket[':target]
line. A @racket[':depend] line with multiple @racket[_path]s is the
same as a sequence of @racket[':depend] lines with the same
@racket[_dep-path-or-target] list, but a @racket[':target] line with multiple
@racket[_path]s creates a single target that builds all of the
@racket[_path]s.

In @racket[':target] and @racket[':depend] lines, a @racket[_path] is
normally a path string, but it can be a symbol for a @tech{phony}
target. When a @racket[':target] has multiple @racket[_path]s, they
must all be path strings.

A @racket[_build-proc] accepts a path (if not phony) and a @tech{build
token}, just like a @racket[_get-deps] procedure for @racket[target],
but @racket[_build-proc] should build the target like the
@racket[_rebuild] procedure for @racket[rule] (or @racket[phony-rule]).
When a @racket[':target] line has multiple @racket[_path]s, only the
first one is passed to the @racket[_build-proc].

A @racket[_dep-path-or-target] is normally a path string. If it is the
same path as the @racket[_path] of a @racket[':target] line, then a
dependency is established on that target. If
@racket[_dep-path-or-target] is any other path string, it is coerced
to an input-file target. A @racket[_dep-path-or-target] can also be a
target that is created outside the @racket[make-targets] call.

An @racket[_option] can be @racket[':precious], @racket[':command],
@racket[':noisy], @racket[':quiet], or @racket[':eager] to set the
corresponding option (see @racket[target]) in a target.}

A @racket[':db-dir] line (appearing at most once) specifies where
build information should be recorded for all targets. Otherwise, the
build result for each target is stored in the target's directory.
