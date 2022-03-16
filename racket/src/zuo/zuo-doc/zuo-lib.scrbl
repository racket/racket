#lang scribble/manual
@(require scribble/bnf
          (for-label zuo-doc/fake-zuo
                     racket/contract/base)
          "real-racket.rkt")

@(define-syntax-rule (defzuomodule zuo/x)
   (begin
     @defmodule[zuo/x #:no-declare #:packages ()]
     @declare-exporting[zuo zuo/x #:packages () #:use-sources (zuo-doc/fake-zuo)]
     @para{The @racketmodname[zuo/x] module is reprovided by @racketmodname[zuo].}))

@title[#:tag "zuo-lib" #:style '(toc)]{Zuo Libraries}

The @racketmodname[zuo] language includes libraries added to
@racketmodname[zuo/base] to support scripting and build tasks.

@local-table-of-contents[]

@; ------------------------------------------------------------

@section[#:tag "zuo-cmdline"]{Command-Line Parsing}

@defzuomodule[zuo/cmdline]

@defform[#:literals(:program :usage :args-in :init :multi :once-each :once-any :args)
         (command-line flag-clause ... args-clause)
         #:grammar ([flag-clause (code:line :program expr)
                                 (code:line :usage expr)
                                 (code:line :args-in expr)
                                 (code:line :init expr)
                                 (code:line :multi flag-spec ...)
                                 (code:line :once-each flag-spec ...)
                                 (code:line :once-any flag-spec ...)]
                    [flag-spec (accum-id flags id ... help-spec
                                         accum-body ...+)
                               (flags id ... help-spec
                                      proc-body ...+)]
                    [flags string
                           (string ...)]
                    [help-spec string
                               (string-expr ...)]
                    [args-clause code:blank
                                 (code:line :args args-formals
                                                  proc-body ...)])]{

Analogous to @realracket*[command-line] from
@racketmodname[racket/cmdline].

One small difference is that @racket[:args-in] is used to specify a
list of incoming arguments instead of @racket[#:argv] for an incoming
vector of arguments. The default @racket[:args-in] uses
@racket[(hash-ref (runtime-env) 'args '())]. Another difference is the
addition of @racket[:usage], which supplies a usage-options string
as an alternative to the one inferred from an @racket[:args] clause.

A more significant difference is that @racketmodname[zuo] does not
have mutable data structures, so an explicit accumulator must be
threaded through flag parsing. An @racket[:init] clause provides the
initial accumulator value defaulting to @racket[(hash)]. Each
@racket[flag-spec] either starts with an @racket[accum-id], which is
bound to the value accumulated so far, or its body produces a function
to receive the accumulated value; either way, the result of the body
or procedure is a new accumulated value. Finally, the body of an
@racket[args-clause] must produce a function to receive the
accumulated value.}

@; ------------------------------------------------------------

@section[#:tag "zuo-build"]{Building with Dependencies}

@defzuomodule[zuo/build]

The @racketmodname[zuo/build] library for tracking dependencies and
build steps is modeled on @exec{make} and
@hyperlink["https://shakebuild.com/"]{Shake}. A @tech{target}
represents either an input to a build (such as a source file) or a
generated output, and a target can depend on any number of other
targets. A built target is represented by 40-character string that is
normally a SHA-1 hash; the @racket[build] procedure records hashes and
dependencies in a database located alongside non-input targets, so it
can avoid rebuilding targets when nothing has changed since the last
build. Unlike @tt{make}, timestamps are used only as a shortcut to
avoiding computing the SHA-1 of a file (i.e., if the timestamp has not
changes, the SHA-1 result is assumed to be unchanged).

@subsection[#:tag "make-target"]{Creating Targets}

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
@racket[file-sha1] is used by default; and a @racket[_rebuild]
procedure that is called if the returned hash, the hash of
dependencies (rebuilt if needed), and recorded results from a previous
build together determine that a rebuild is needed.

When a target's @racket[_rebuild] function is called, it optionally
returns a hash for the result of the build if the target's
@racket[rule] had one, otherwise @racket[file-sha1] is used to get a
result hash. Either way, it's possible that the result hash is the
same the one returned by @racket[_get-rule]; that is, maybe a
dependency of the target changed, but the change turned out not to
affect the built result. In that case, rebuilding for other targets
that depend on this one can be short-circuited.

Finally, in the process of building a target, a @racket[_rebuild]
procedure may discover additional dependencies. A discovered
dependency sent to @racket[build/recur] is recorded as a dependency of
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

In some cases, a plain path string can be used as a target as a
shorthand for applying @racket[input-file-target] to the path string.

@subsection{Building Targets}

There is no global list of targets that @racket[build] draws from.
Instead, @racket[build] starts with a given target, and it learns
about other targets a @racket[_get-dep] procedures return them and as
@racket[_rebuild] procedures expose them via @racket[build/recur]. If
@racket[build] discovers multiple targets with the same filename,
then @racket[build] reports an error, unless the targets were all
created by @racket[input-file-target].

The @racket[build/command-line] function is a convenience to implement
get @tt{make}-like command-line handling for building targets. The
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
another makefile via @tt{make} arguments).

As a further convenience following the @racketidfont{targets-at}
model, the @racket[provide-targets] form takes an expression for
producing such a @racketidfont{targets-at} procedure, and it both
exports @racketidfont{targets-at} and creates a @racket[main]
@tech{submodule} that calls @racket[build/command-line*] on with the
@racketidfont{targets-at} procedure.

@subsection{Recording Results}

Build results are stored in a @filepath{_zuo.db} file in the same
directory as a target by default. Cached SHA-1 results with associated
file timestamps are stored in a @filepath{_zuo_tc.db} in the same
directory (i.e., the cached value for dependency is kept with the
target, which is in a writable build space, while an input-file target
might be in a read-only source space). A target's options can specify
an alternative directory to use for @filepath{_zuo.db} and
@filepath{_zuo_tc.db}.

In the unfortunate case that a @filepath{_zuo.db} or
@filepath{_zuo_tc.db} file gets mangled, then it may trigger an error
that halts the build system, but the @filepath{_zuo.db} or
@filepath{_zuo_tc.db} file will be deleted in reaction to the error.
Another attempt at the build should recover, while perhaps rebuilding
more than it would have otherwise, since the result of previous builds
might have been lost.

@subsection{Parallelism}

A build runs in a @tech{threading context}, so a target's
@racket[_get-deps] or @racket[_rebuild] procedure can use
@racket[thread-process-wait] can be used to wait on a process. Doing
so can enable parallelism among targets, depending on the
@racket['jobs] option provided to @racket[build] or
@racket[build/command-line].


@subsection{Build API}


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
via @racket[file-sha1].}


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
target build in progress. It's useful with @racket[file-sha1] to take
advantage of caching and with @racket[build/recur] to report
discovered targets.

The following keys are recognized in @racket[options]:

@itemlist[

@item{@racket['precious?] mapped to any value: if non-@racket[#f] for
      a non-phony target, @racket[name] is not deleted if the
      @racket[get-deps] function or its result's @racket[_build]
      function fails.}

@item{@racket['command?] mapped to any value: if non-@racket[#f], when
      @racket[build/command-line] runs the target as the first one
      named on the command line, all arguments from the command line
      after the target name are provided @racket[_get-deps] as
      additional arguments.}

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
               [rebuild (or/c (-> (or/c sha1? any/c)) #f) #f]
               [sha1 (or/c sha1? #f) #f])
         rule?]
@defproc[(rule? [v any/c]) boolean?]
)]{

The @racket[rule] procedure combines the three results expected from a
procedure passed to @racket[target]. See @secref["make-target"].

A path string can be reported as a dependency in
@racket[dependencies], in which case it is coerced to a target using
@racket[input-file-target]. If @racket[sha1] is @racket[#f],
@racket[file-sha1] is used to compute the target's current hash, and
@racket[rebuild] is not expected to return a hash. If @racket[sha1] is
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
target. Compared to the non-phonu protocol, the result SHA-1 is
omitted.}

@defproc[(token? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a token representing a target
build, @racket[#f] otherwise.}


@defproc[(build [target (or/c target? path-string? (listof (or/c target? path-string?)))]
                [options hash? (hash)])
         void?]{

Builds @racket[target] as a fresh build process, independent of any
that might already be running. A list of targets as @racket[target] is
coerced to a phony target that depends on the given list.

If @racket[target] is a path, then it is coerced to target via
@racket[input-file-target], but the only effect will be to compute the
file's SHA-1 or error if the file does not exist.

The @racket[options] argument supplies build options, and the
following keys are recognized:

@itemlist[

@item{@racket['jobs] mapped to a positive integer: controls the
      maximum build steps that are allowed to proceed concurrently;
      this concurrency turns into parallelism when a task uses a
      process and @racket[thread-process-wait]}

@item{@racket['log?] mapped to any value: enables logging of rebuild
      reasons via @racket[alert] when the value is not @racket[#f];
      logging also can be enabled by setting the
      @envvar{ZUO_BUILD_LOG} environment variable}

]}

@defproc[(build/recur [target (or target? path-string?)] [token token?]) void?]{

Like @racket[build], but continues a build in progress as represented
by a @racket[token] that was passed to a target's @racket[_get-deps]
or @racket[_rebuild] procedure. After @racket[target] is built, it is
registered as a dependency of the target that received
@racket[token].}


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
assignments, where @racket{name} is formed by @litchar{a}-@litchar{z},
@litchar{A}-@litchar{Z}, @litchar{_}, and @litchar{0}-@litchar{9}, but
not starting @litchar{0}-@litchar{9}. These variables can appear
anywhere in the command line and are removed from the argument list
sent on to @racket[build/command-line], but no argument after a
@racket{--} argument is parsed as a variable assignment.

The @racket[targets-at] procedure is applied to @racket[at-dir] and a
hash table of variables, where each variable name is converted to a
symbol and the value is left exact as after @litchar{=}.}

@defproc[(find-target [name string?] [targets (listof target?)]) (or/c target? #f)]{

Finds the first target in @racket[targets] that is a match for
@racket[name], returning @racket[#f] is not match is found. A
@racket[name] matches when it is the same as n entire symbol or path
target name or when it matches a suffix that is preceded by
@litchar{/} or @litchar{\\}.}

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
@defproc[(file-sha1 [file path-string?] [token (or/c token? #f)]) sha1?]
@defproc[(sha1? [v any/c]) booelan?]
)]{

The @racket[file-sha1] procedure returns the SHA-1 hash of the content
of @racket[file], or it returns @racket[no-sha1] if @racket[file] does
not exist.

The @racket[sha1?] predicate recognizes values that are either a
40-character string or @racket[no-sha1].}

@defthing[no-sha1 sha1? ""]{

The empty string represents a non-existent target or one that needs to
be rebuilt.}

@defform[(provide-targets targets-at-expr)]{

Binds a generated identifier to the result of
@racket[targets-at-expr], provides it as @racketidfont{targets-at},
and creates a @racketidfont{main} submodule that runs
@racket[(build/command-line* @#,racketidfont{targets-at} build-path)].
A script using @racket[provide-targets] thus works as a makefile-like
script or as an input to a larger build.}

@defproc[(make-targets [specs list?]) list?]{

Converts a @tt{make}-like specification into a list of targets for use
with @racket[build]. In this @tt{make}-like specification, extra
dependencies can be listed separately from a build rule, and
dependencies can be written in terms of paths instead of @tech{target}
objects.

The @racket[specs] argument is a list of @defterm{lines}, where each
line has one of the following shapes:

@racketblock[
  `[:target ,_path (,_dep-path-or-target ...) ,_build-proc ,_option ...]
  `[:depend ,_path (,_dep-path-or-target ...)]
  `[:depend ,(_path ...) (,_dep-path-or-target ...)]
  `[:db-dir ,path]
]

A @racket[:target] line defines a build rule that is implemented by
@racket[_build-proc], while a @racket[:depend] line adds extra
dependencies for a @racket[_path] that also has a @racket[:target]
line. A @racket[:depend] line with multiple @racket[_path]s is the
same as a sequence of @racket[:depend] lines with the same
@racket[_dep-path-or-target] list. A @racket[:db-dir] line (appearing
at most once) specifies where build information should be recorded for
all targets; otherwise the build result for each target is stored in the
target's directory.

In @racket[:target] and @racket[:depend] lines, a @racket[_path] is
normally a path string, but it can be a symbol for a @tech{phony}
target.

A @racket[_build-proc] accepts a path (if not phony) and a @tech{build
token}, just like a @racket[_get-deps] procedure for @racket[target],
but @racket[_build-proc] should build the target like the
@racket[_build] procedure for @racket[rule] (or @racket[phony-rule]).

A @racket[_dep-path-or-target] is normally a path string. If it is the
same path as the @racket[_path] of a @racket[:target] line, then a
dependency is established on that target. If
@racket[_dep-path-or-target] is any other path string, it is coerced
to an input-file target. A @racket[_dep-path-or-target] can also be a
target that is created outside the @racket[make-targets] call.

An @racket[_option] can be @racket[:precious], @racket[:command],
@racket[:noisy], @racket[:quiet], or @racket[:eager] to set the
corresponding option (see @racket[target]) in a target.}

@; ------------------------------------------------------------

@section[#:tag "zuo-glob"]{Glob Matching}

@defzuomodule[zuo/glob]

@defproc[(glob->matcher [glob string?]) procedure?]{

Creates a procedure that takes a string and reports @racket[#t] if the
string matches the pattern @racket[glob], @racket[#f] otherwise.

The pattern language of @racket[glob] is based on shell globbing:

@itemlist[

 @item{@litchar{?} matches any character;}

 @item{@litchar{*} matches any squence of characters; and}

 @item{@litchar{[}@italic{range}@litchar{]} matches any character in
       @italic{range}, and @litchar{[^}@italic{range}@litchar{]}
       matches any character not in @italic{range}.}

]

In a @italic{range}, most characters stand for themselves as elements
of the range, including @litchar{*} and @litchar{?}, including
@litchar{]} when it appears first, and including @litchar{-} when it
appears first or last. When @litchar{-} appears between two characters
in @italic{range}, then the second character's value must be at least
as large as the first, and all character in from the first (inclusive)
to the second (inclusive) are included in the range.

A leading @litchar{.} or a @litchar{/} in an input string are not
treated specially. That is, @racket["*"] matches @racket[".apple"] and
@racket["a/pple"] as well as @racket["apple"]. Use @racket[split-path]
and a secondary check for a leading @litchar{.} to imitate shell-like
path-sensitive globbing.}


@defproc[(glob-match? [glob string?] [str string?]) boolean?]{

Equivalent to @racket[((glob->matcher glob) str)].}

@; ------------------------------------------------------------

@section[#:tag "zuo-thread"]{Cooperative Threads}

@defzuomodule[zuo/thread]

To use cooperative threads, create a @deftech{threading context} with
@racket[call-in-main-thread], and perform a thread operations---such
as creating a new thread with @racket[thread] or a channel with
@racket[channel]---during the body of the thunk provided to
@racket[call-in-main-thread]. Threads can block either on channels or
on a process handle as from @racket[process]. Only one thread runs at
a time, where a thread switch happens only when a thread terminates or
uses a (potentially) blocking operation.

@defproc[(call-in-main-thread [thunk procedure]) any/c]{

Creates a new @tech{threading context}, calling @racket[thunk] in the
main thread of the context, and returning the value of @racket[thunk]
after all threads in the context have either completed or are blocked
on a channel. An error is reported if no thread can run and the main
thread is blocked on a channel.}

@deftogether[(
@defproc[(thread [thunk procedure?]) thread?]
@defproc[(thread? [v any/c]) boolean?]
@defproc[(channel) channel?]
@defproc[(channel? [v any/c]) boolean?]
@defproc[(channel-put [ch channel?] [v any/c]) channel?]
@defproc[(channel-get [ch channel?]) any/c]
)]{

Analogous to @realracket*[thread thread? make-channel channel? channel-put
channel-get] from @racketmodname[racket], but channels are
asynchronous (with an unbounded queue) instead of synchronous.

Except for @racket[thread?] and @racket[channel?], these procedures
can be used only in a @tech{threading context}. A channel can be used
only in the threading context where it was created.

Beware that attempting to use these operations outside of a threading
context will @emph{not} necessarily trigger an error, and may instead
deliver an opaque threading request to the enclosing continuation
prompt.}

@defproc[(thread-process-wait [process handle?] ...) handle?]{

Like @racket[process-wait], but can only be used in a @tech{threading
context}, and counts as a blocking operation that can allow other
threads to run.}

@; ------------------------------------------------------------

@section[#:tag "zuo-shell"]{Shell Commands}

@defzuomodule[zuo/shell]

@defproc[(shell [command path-string?] [options hash? (hash)]) hash?]{

Like @racket[process], but runs @racket[command] as a shell command
(via @exec{/bin/sh} on Unix or @exec{cmd.exe} on Windows).}

@defproc[(shell/wait [command path-string?]
                     [options hash? (hash)]
                     [what string? "shell command"])
         void?]{

Like @racket[shell], but first @racket[displayln]s the command string,
uses @racket[thread-process-wait] (or @racket[process-wait] if
@racket[options] has a true value for @racket['no-thread?]) to wait on
the shell process, and reports an error if the process has a
non-@racket[0] exit code. The @racket[what] string is use when
constructing an error.

If @racket[options] includes @racket['quiet?] mapped to a true value,
then @racket[command] is not shown using @racket[displayln], and
@racket['quiet?] is removed before passing it on to @racket[process].
Similarly, @racket['no-thread?] is removed from @racket[options]
before passing it on to @racket[process].}

@defproc[(build-shell [shell-str string?] ...) string?]{

Appends the @racket[shell-str]s with separating spaces to form a
larger shell-command sequence. An empty-string @racket[shell-str] is
dropped, instead of creating extra spaces.

Note that @racket[build-shell] does @emph{not} attempt to protect any
@racket[shell-str] as a literal. Use @racket[string->shell] to convert
an individual path or literal string to a shell-command argument
encoding that string.}

@; ------------------------------------------------------------

@section[#:tag "zuo-c"]{C Tools}

@defzuomodule[zuo/c]

The C-tool procedures provided by @racketmodname[zuo/c] accept a
@deftech{tool configuration} hash table to describe a C compiler,
linker, archiver, and associated flags. When potential configuration
is missing, a default suitable for the current system is used. Values
in a tool configuration hash table are shell-command fragments, not
individual arguments. For example, it could make sense to configure
@racket['CC] as @racket["libtool cc"], which would run @exec{libtool}
in compilation mode, instead of trying to run a compile whose
executable name includes a space.

The following keys are recognized in a tool configuration:

@itemlist[

@item{@racket['CC]: a C compiler}

@item{@racket['CPPFLAGS]: C preprocessor flags}

@item{@racket['CFLAGS]: C compilation flags}

@item{@racket['LDFLAGS]: C linker flags}

@item{@racket['LIBS]: additional C libraries}

@item{@racket['AR]: library archiver}

@item{@racket['ARFLAGS]: library archiver flags}

]


@defproc*[([(c-compile [.o path-string?] [.c path-string?] [config hash?]) void?]
           [(c-compile [out path-string?] [ins (listof path-string?)] [config hash?]) void?])]{

Compiles @racket[.c] to @racket[.o] using the @tech{tool
configuration} @racket[config], or combines compiling and linking by
with @racket[ins] compiled and linked to @racket[out] using
@racket[config].}

@defproc[(c-link [.exe path-string?] [ins (listof path-string?)] [config hash?]) void?]{

Links the files @racket[ins] to create the executable @racket[.exe]
using the @tech{tool configuration} @racket[config].}

@defproc[(c-ar [.a path-string?] [ins (listof path-string?)] [config hash?]) void?]{

Combines the object files @racket[ins] to create the archive
@racket[.a] using the @tech{tool configuration} @racket[config].}

@defproc[(.c->.o [.c path-string?]) path-string?]{

Adjusts the filename @racket[.c] to be the conventional name of its
compiled object file on the current system.}

@defproc[(.exe [name path-string?]) path-string?]{

Adds @filepath{.exe} to the end of @racket[name] if conventional on
the current system.}

@defproc[(.a [name path-string?]) path-string?]{

Derives the conventional archive name for a library @racket[name] on
the current system.}

@defproc[(config-merge [config hash?] [key symbol?] [shell-str string?]) hash?]{

Adds @racket[shell-str] to the shell-command fragment for @racket[key]
in the @tech{tool configuration} @racket[config].}


@defproc[(config-include [config hash?] [path path-string?] ...) hash?]{

Adds the @racket[path]s as include directories in the @tech{tool
configuration} @racket[config].}


@defproc[(config-define [config hash?] [def string?] ...) hash?]{

Adds the preprocessor definitions @racket[def]s to preprocessor flags
in the @tech{tool configuration} @racket[config].}

@; ------------------------------------------------------------

@section[#:tag "zuo-config"]{Configuration Parsing}

@defzuomodule[zuo/config]

@defproc[(config-file->hash [file path-string?]) hash?]{

Parses @racket[file] as having configuration lines of the form
@nonterm{name} @litchar{=} @nonterm{value}, with any number of ignored
spaces at the start of the line, end of the line, or around the
@litchar{=}, and with a trailing @litchar{\} on a line deleted along
with its newline (to create a single line). Each @nonterm{name}
consists of alphanumeric characters and @litchar{_}; the symbol form
of the name is used as a key in the resulting hash table, mapped to
the @nonterm{value} as a string. Lines in @racket[file] that do not
match the configuration format are ignored. If a same @nonterm{name}
is configured multiple times, the last mapping overrides earlier
ones.}
