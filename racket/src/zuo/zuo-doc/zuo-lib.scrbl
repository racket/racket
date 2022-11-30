#lang scribble/manual
@(require scribble/bnf
          (for-label zuo-doc/fake-zuo
                     racket/contract/base)
          "real-racket.rkt"
          "defzuomodule.rkt")

@title[#:tag "zuo-lib" #:style '(toc)]{Zuo Libraries}

The @racketmodname[zuo] language includes libraries added to
@racketmodname[zuo/base] to support scripting and build tasks.

@local-table-of-contents[]

@; ------------------------------------------------------------

@section[#:tag "zuo-cmdline"]{Command-Line Parsing}

@defzuomodule[zuo/cmdline]

@defform[#:literals(:program :preamble :usage :args-in :init :multi :once-each :once-any :args)
         (command-line flag-clause ... args-clause)
         #:grammar ([flag-clause (code:line :program expr)
                                 (code:line :preamble expr)
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
addition of @racket[:preamble], which supplies a string to be
shown before a ``usage'' line, and @racket[:usage], which supplies a
usage-options string as an alternative to the one inferred from an
@racket[:args] clause.

A more significant difference is that @racketmodname[zuo] does not
have mutable data structures, so an explicit accumulator must be
threaded through flag parsing. An @racket[:init] clause provides the
initial accumulator value defaulting to @racket[(hash)]. Each
@racket[flag-spec] either starts with an @racket[accum-id], which is
bound to the value accumulated so far, or its body produces a function
to receive the accumulated value; either way, the result of the body
or procedure is a new accumulated value. Finally, the body of an
@racket[args-clause] must produce a function to receive the
accumulated value.

@history[#:changed "1.3" @elem{Added @racket[:preamble].}]}

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
@racket[call-in-main-thread], and perform thread operations---such
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
@defproc[(channel-try-get [ch channel?]) any/c]
)]{

Analogous to @realracket*[thread thread? make-channel channel? channel-put
channel-get channel-try-get] from @racketmodname[racket], but channels are
asynchronous (with an unbounded queue) instead of synchronous.

Except for @racket[thread?] and @racket[channel?], these procedures
can be used only in a @tech{threading context}. A channel can be used
only in the threading context where it was created.

Beware that attempting to use these operations outside of a threading
context will @emph{not} necessarily trigger an error, and may instead
deliver an opaque threading request to the enclosing continuation
prompt.

@history[#:changed "1.4" @elem{Added @racket[channel-try-get].}]}

@defproc[(thread-process-wait [process handle?] ...) handle?]{

Like @racket[process-wait], but can only be used in a @tech{threading
context}, and counts as a blocking operation that can allow other
threads to run.}

@; ------------------------------------------------------------

@section[#:tag "zuo-shell"]{Shell Commands}

@defzuomodule[zuo/shell]

@defproc[(shell [command string-tree?] ... [options hash? (hash)]) hash?]{

Like @racket[process], but runs the combination of @racket[command]
strings as a shell command (via @exec{/bin/sh} on Unix or
@exec{cmd.exe} on Windows).

The @racket[command] strings are combined into a single command string
in the same as by @racket[build-shell]. Spaces in a @racket[command]
are left as-is; so, for example, @racket["ls -a"] as a sole
@racket[command] string is the same as a sequence @racket["ls"] then
@racket["-a"]. Use @racket[string->shell] to protect characters like
spaces, and especially to convert from a path (that might have spaces
or other special characters) to part of a command.}


@defproc[(shell/wait [command string-tree?] ... [options hash? (hash)])
         void?]{

Like @racket[shell], but first @racket[displayln]s the command string,
uses @racket[thread-process-wait] (or @racket[process-wait] if
@racket[options] has a true value for @racket['no-thread?]) to wait on
the shell process, and reports an error if the process has a
non-@racket[0] exit code.

If @racket[options] includes @racket['quiet?] mapped to a true value,
then @racket[command] is not shown using @racket[displayln]. If
@racket[options] includes @racket['desc] mapped to a string value, the
string is used in place of @racket["shell command"] when reporting an
error. Any @racket['quiet?], @racket['no-thread?], or @racket['desc]
mapping is removed from @racket[options] before passing it on to
@racket[process].}


@defproc[(build-shell [shell-strs string-tree?] ...) string?]{

Appends the flattened @racket[shell-strs] sequence with separating
spaces to form a larger shell-command sequence. An empty-string among
@racket[shell-strs] is dropped, instead of creating extra spaces.

Note that @racket[build-shell] does @emph{not} attempt to protect any of the
@racket[shell-strs] as a literal. Use @racket[string->shell] to convert
an individual path or literal string to a shell-command argument
encoding that string.}


@defproc[(shell-subst [str string?] [vars hash?]) string?]{

Expands @racket[str] by replacing
@litchar|{${}|@nonterm{name}@litchar|{}}| with the value that the
symbol form of @nonterm{name} is mapped to in @racket[vars], where
@nonterm{name} can contain any character except @litchar["}"]. After a
replacement, the string is scanned again for further substitutions. An
error is reported if a reference @nonterm{name} has no value in
@racket[vars].}


@; ------------------------------------------------------------

@section[#:tag "zuo-c"]{C Tools}

@defzuomodule[zuo/c]

The C-tool procedures provided by @racketmodname[zuo/c] accept a
@deftech{tool configuration} hash table to describe a C compiler,
linker, archiver, and associated flags. When potential configuration
is missing, a default suitable for the current toolchain is used,
where the toolchain is determined through @racket[(hash-ref
(runtime-env) 'toolchain-type)]. Values
in a tool configuration hash table are shell-command fragments, not
individual arguments. For example, it could make sense to configure
@racket['CC] as @racket["libtool cc"], which would run @exec{libtool}
in compilation mode, instead of trying to run a compile whose
executable name includes a space.

The following keys are recognized in a tool configuration:

@itemlist[

@item{@racket['CC]: a C compiler}

@item{@racket['CPPFLAGS]: C preprocessor flags}

@item{@racket['CFLAGS]: C compilation and linking flags}

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

@defproc[(config-file->hash [file path-string?] [overrides hash? (hash)]) hash?]{

Parses @racket[file] as having configuration lines of the form
@nonterm{name} @litchar{=} @nonterm{value}, with any number of ignored
spaces at the start of the line, end of the line, or around the
@litchar{=}, and with a trailing @litchar{\} on a line deleted along
with its newline (to create a single line). A @litchar{#} character
terminates a line to start a comment, unless the @litchar{#} is
preceded by @litchar{\}, in which case the @litchar{\#} combination is
parsed as a literal @litchar{#}.

Each @nonterm{name} consists of alphanumeric characters and
@litchar{_}; the symbol form of the name is used as a key in the
resulting hash table, mapped to the @nonterm{value} as a string. Lines
in @racket[file] that do not match the configuration format are
ignored. If a same @nonterm{name} is configured multiple times, the
last mapping overrides earlier ones.

After reading @racket[file], keys from @racket[overrides] are merged
to the result hash table, where values in @racket[overrides] replace
ones read from @racket[file].}

@; ------------------------------------------------------------

@section[#:tag "zuo-jobserver"]{Jobserver Client}

@defzuomodule[zuo/jobserver]

@history[#:added "1.1"]

@defproc[(maybe-jobserver-client) (or/c procedure? #f)]{

Returns a procedure if a jobserver configuration is found via the
@envvar{MAKEFLAGS} environment variable, @racket[#f] otherwise. That
environment variable is normally set by GNU Make when it runs a target
command and when @Flag{j} was provided to @exec{make}. A jobserver
configuration allows parallelism to span @exec{make} and other
processes, such as a @exec{zuo} process, through a shared pool of
jobserver tokens. In other words, a @Flag{j} flag to @exec{make} gets
propagated to @exec{zuo}.

When a procedure is returned, it accepts one argument: @racket['get]
or @racket['put]. Apply the procedure with @racket['get] to acquire a
jobserver token, and apply the procedure with @racket['put] to release
a previously acquired token. The implicit jobserver token that belongs
to the @exec{zuo} process should be taken explicitly with
@racket['get] and released with @racket['put].

The @racket[maybe-jobserver-client] procedure must be called in a
@tech{threading context}. When it returns a procedure, that procedure
must also be called (with @racket['get] or @racket['put]) in the same
threading context.}

@defproc[(maybe-jobserver-jobs) (or/c integer? #f)]{

Similar to @racket[maybe-jobserver-client], but polls the jobserver
(if any) to determine how many job tokens appear to be immediately
available. The result is that number, or @racket[#f] if no jobserver
configuration is found.

Using @racket[maybe-jobserver-client] to cooperate interactively with
the jobserver is normally better, but @racket[maybe-jobserver-jobs]
can be useful to chaining to another tool that accepts job count as a
number.

Unlike @racket[maybe-jobserver-client], @racket[maybe-jobserver-jobs]
does not need to be called in a @tech{threading context}.}
