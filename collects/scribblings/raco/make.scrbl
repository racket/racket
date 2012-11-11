#lang scribble/doc
@(require scribble/manual scribble/bnf scribble/eval "common.rkt"
          (for-label racket/base
                     racket/include
                     racket/contract
                     racket/future
                     compiler/cm
                     compiler/cm-accomplice
                     setup/parallel-build))


@(define cm-eval (make-base-eval))
@(interaction-eval #:eval cm-eval (require compiler/cm))
@title[#:tag "make"]{@exec{raco make}: Compiling Source to Bytecode}

The @exec{raco make} command accept filenames for Racket modules to be
compiled to bytecode format. Modules are re-compiled only if the
source Racket file is newer than the bytecode file and has a different
SHA-1 hash, or if any imported module is recompiled or has a different
SHA-1 hash for its compiled form plus dependencies.

The @exec{raco make} command accepts a few flags:

@itemlist[

 @item{@Flag{j} @nonterm{n} --- Compiles argument modules in parallel,
       using up to @nonterm{n} parallel tasks.}

 @item{@DFlag{disable-inline} --- Disables function inlining while
      compiling (but does not re-compile files that are already
      up-to-date). This flag is often useful to simplify generated
      code before decompiling, and it corresponds to setting
      @racket[compile-context-preservation-enabled] to @racket[#t].}

 @item{@DFlag{disable-constant} --- Disables inference of definitions
      within a module as constant (but does not re-compile files that
      are already up-to-date). The value associated with a
      non-constant definition is never inlined or constant-propagated,
      either within its own module or an importing module. This flag
      corresponds to setting @racket[compile-enforce-module-constants]
      to @racket[#f].}

 @item{@DFlag{no-deps} --- Compiles a non-module file (i.e., one that
       is run via @racket[load] instead of @racket[require]). See
       @secref["zo"] for more information.}

 @item{@Flag{p} @nonterm{file} or @DFlag{prefix} @nonterm{file} ---
       For use with @DFlag{no-deps}; see @secref["zo"].}

 @item{@Flag{no-prim} --- For use with @DFlag{no-deps}; see
       @secref["zo"].}

 @item{@Flag{v} --- Verbose mode, which shows which files are
      compiled.}

 @item{@DFlag{vv} --- Very verbose mode, which implies @Flag{v} and
       also shows every dependency that is checked.}

]

@; ----------------------------------------------------------------------

@section{Bytecode Files}

A file @filepath{@nonterm{name}.@nonterm{ext}} is compiled to bytecode
that is saved as @filepath{compiled/@nonterm{name}_@nonterm{ext}.zo}
relative to the file. As a result, the bytecode file is normally used
automatically when @filepath{@nonterm{name}.@nonterm{ext}} is required
as a module, since the underlying @racket[load/use-compiled] operation
detects such a bytecode file.

For example, in a directory that contains the following files:

@itemize[

 @item{@filepath{a.rkt}:

@racketmod[
racket
(require "b.rkt" "c.rkt")
(+ b c)
]}

 @item{@filepath{b.rkt}:

@racketmod[
racket
(provide b)
(define b 1)
]}

 @item{@filepath{c.rkt}:

@racketmod[
racket
(provide c)
(define c 1)
]}]

then

@commandline{raco make a.rkt}

triggers the creation of @filepath{compiled/a_rkt.zo},
@filepath{compiled/b_rkt.zo}, and @filepath{compiled/c_rkt.zo}.
A subsequent

@commandline{racket a.rkt}

loads bytecode from the generated @filepath{.zo} files, paying
attention to the @filepath{.rkt} sources only to confirm that each
@filepath{.zo} file has a later timestamp.

In contrast,

@commandline{raco make b.rkt c.rkt}

would create only @filepath{compiled/b_rkt.zo} and
@filepath{compiled/c_rkt.zo}, since neither @filepath{b.rkt} nor
@filepath{c.rkt} imports @filepath{a.rkt}.

@; ----------------------------------------------------------------------

@section{Dependency Files}

In addition to a bytecode file, @exec{raco make} creates a file
@filepath{compiled/@nonterm{name}_@nonterm{ext}.dep} that records
dependencies of the compiled module on other module files and the
source file's SHA-1 hash.  Using this dependency information, a
re-compilation request via @exec{raco make} can consult both the
source file's timestamp/hash and the timestamps/hashes for the
bytecode of imported modules.  Furthermore, imported modules are
themselves compiled as necessary, including updating the bytecode and
dependency files for the imported modules, transitively.

Continuing the @exec{raco make a.rkt} example from the previous
section, the @exec{raco make} command creates
@filepath{compiled/a_rkt.dep}, @filepath{compiled/b_rkt.dep}, and
@filepath{compiled/c_rkt.dep} at the same time as the @filepath{.zo}
files. The @filepath{compiled/a_rkt.dep} file records the dependency
of @filepath{a.rkt} on @filepath{b.rkt}, @filepath{c.rkt} and the
@racketmodname[racket] library. If the @filepath{b.rkt} file is
modified (so that its timestamp and SHA-1 hash changes), then running

@commandline{raco make a.rkt}

again rebuilds @filepath{compiled/a_rkt.zo} and
@filepath{compiled/b_rkt.zo}.

For module files that are within library collections, @exec{raco
setup} uses the same @filepath{.zo} and @filepath{.dep} conventions
and files as @exec{raco make}, so the two tools can be used together.

@; ----------------------------------------------------------------------

@section{API for Making Bytecode}

@defmodule[compiler/cm]{The @racketmodname[compiler/cm] module
implements the compilation and dependency management used by
@exec{raco make} and @exec{raco setup}.}

@defproc[(make-compilation-manager-load/use-compiled-handler 
          [delete-zos-when-rkt-file-does-not-exist? any/c #f]
          [#:security-guard security-guard (or/c security-guard? #f) #f])
         (path? (or/c symbol? false/c) . -> . any)]{

Returns a procedure suitable as a value for the
@racket[current-load/use-compiled] parameter. The returned procedure
passes it arguments on to the @racket[current-load/use-compiled]
procedure that is installed when
@racket[make-compilation-manager-load/use-compiled-handler] is called,
but first it automatically compiles a source file to a @filepath{.zo}
file if

@itemize[

 @item{the file is expected to contain a module (i.e., the second
 argument to the handler is a symbol);}

 @item{the value of each of @racket[(current-eval)],
 @racket[(current-load)], and @racket[(namespace-module-registry
 (current-namespace))] is the same as when
 @racket[make-compilation-manager-load/use-compiled-handler] was
 called;}

 @item{the value of @racket[use-compiled-file-paths] contains the
 first path that was present when
 @racket[make-compilation-manager-load/use-compiled-handler] was
 called;}

 @item{the value of @racket[current-load/use-compiled] is the result
 of this procedure; and}

 @item{one of the following holds:

 @itemize[

  @item{the source file is newer than the @filepath{.zo} file in the
        first sub-directory listed in @racket[use-compiled-file-paths]
        (at the time that
        @racket[make-compilation-manager-load/use-compiled-handler]
        was called), and either no @filepath{.dep} file exists or it
        records a source-file SHA-1 hash that differs from the current
        version and source-file SHA-1 hash;}

  @item{no @filepath{.dep} file exists next to the @filepath{.zo}
        file;}

  @item{the version recorded in the @filepath{.dep} file does not
        match the result of @racket[(version)];}

  @item{one of the files listed in the @filepath{.dep} file has a
        @filepath{.zo} timestamp newer than the target @filepath{.zo},
        and the combined hashes of the dependencies recorded in the
        @filepath{.dep} file does not match the combined hash recorded
        in the @filepath{.dep} file.}

  ]}

]

If SHA-1 hashes override a timestamp-based decision to recompile the
file, then the target @filepath{.zo} file's timestamp is updated to
the current time.

After the handler procedure compiles a @filepath{.zo} file, it creates
a corresponding @filepath{.dep} file that lists the current version
and the identification of every file that is directly
@racket[require]d by the module in the compiled file. Additional
dependencies can be installed during compilation via
@racketmodname[compiler/cm-accomplice]. The @filepath{.dep} file also
records the SHA-1 hash of the module's source, and it records a
combined SHA-1 hash of all of the dependencies that includes their
recursive dependencies.

The handler caches timestamps when it checks @filepath{.dep} files,
and the cache is maintained across calls to the same handler. The
cache is not consulted to compare the immediate source file to its
@filepath{.zo} file, which means that the caching behavior is
consistent with the caching of the default module name resolver (see
@racket[current-module-name-resolver]).

If @racket[use-compiled-file-paths] contains an empty list when
@racket[make-compilation-manager-load/use-compiled-handler] is called,
then @racket[exn:fail:contract] exception is raised.

If the @racket[delete-zos-when-rkt-file-does-not-exist?] argument is a true
value, then the returned handler will delete @filepath{.zo} files
when there is no corresponding original source file.

If the @racket[security-guard] argument is supplied, it is used when
creating @filepath{.zo} files, @filepath{.dep} files, and @filepath{compiled/}
directories, and when it adjusts the timestamps for existing files.
If it is @racket[#f], then
the security guard in the @racket[current-security-guard] when 
the files are created is used (not the security guard at the point 
@racket[make-compilation-manager-load/use-compiled-handler] is called).


@emph{Do not} install the result of
@racket[make-compilation-manager-load/use-compiled-handler] when the
current namespace contains already-loaded versions of modules that may
need to be recompiled---unless the already-loaded modules are never
referenced by not-yet-loaded modules. References to already-loaded
modules may produce compiled files with inconsistent timestamps and/or
@filepath{.dep} files with incorrect information.}


@defproc[(managed-compile-zo [file path-string?]
                             [read-src-syntax (any/c input-port? . -> . syntax?) read-syntax]
                             [#:security-guard security-guard (or/c security-guard? #f) #f]) 
         void?]{

Compiles the given module source file to a @filepath{.zo}, installing
a compilation-manager handler while the file is compiled (so that
required modules are also compiled), and creating a @filepath{.dep} file
to record the timestamps of immediate files used to compile the source
(i.e., files @racket[require]d in the source).

If @racket[file] is compiled from source, then
@racket[read-src-syntax] is used in the same way as
@racket[read-syntax] to read the source module. The normal
@racket[read-syntax] is used for any required files, however.

If @racket[security-guard] is not @racket[#f], then the provided security
guard is used when creating the @filepath{compiled/} directories, 
@filepath{.dep} and @filepath{.zo} files, and when it adjusts the timestamps 
of existing files. If it is @racket[#f], then
the security guard in the @racket[current-security-guard] when 
the files are created is used (not the security guard at the point 
@racket[managed-compile-zo] is called).
}


@defboolparam[trust-existing-zos trust?]{

A parameter that is intended for use by @exec{setup-plt} when
installing with pre-built @filepath{.zo} files. It causes a
compilation-manager @racket[load/use-compiled] handler to ``touch''
out-of-date @filepath{.zo} files instead of re-compiling from source.}


@defproc[(make-caching-managed-compile-zo
          [read-src-syntax (any/c input-port? . -> . syntax?)]
          [#:security-guard security-guard (or/c security-guard? #f) #f])
         (path-string? . -> . void?)]{

Returns a procedure that behaves like @racket[managed-compile-zo]
(providing the same @racket[read-src-syntax] each time), but a cache
of timestamp information is preserved across calls to the procedure.}

@defparam[manager-compile-notify-handler notify (path? . -> . any)]{

A parameter for a procedure of one argument that is called whenever a
compilation starts. The argument to the procedure is the file's path.}


@defparam[manager-trace-handler notify (string? . -> . any)]{

A parameter for a procedure of one argument that is called to report
 compilation-manager actions, such as checking a file. The argument to
 the procedure is a string.}

@defparam[manager-skip-file-handler proc (-> path? (or/c (cons/c number? promise?) #f))]{

A parameter whose value is called for each file that is loaded and
 needs recompilation. If the procedure returns a pair, then the file
 is skipped (i.e., not compiled); the number in the pair is used as
 the timestamp for the file's bytecode, and the promise may be
 @racket[force]d to obtain a string that is used as hash of the
 compiled file plus its dependencies. If the procedure returns
 @racket[#f], then the file is compiled as usual. The default is
 @racket[(lambda (x) #f)].}

@defproc[(file-stamp-in-collection [p path?]) (or/c (cons/c number? promise?) #f)]{
  Calls @racket[file-stamp-in-paths] with @racket[p] and
  @racket[(current-library-collection-paths)].}

@defproc[(file-stamp-in-paths [p path?] [paths (listof path?)]) (or/c (cons/c number? promise?) #f)]{

Returns the file-modification date and @racket[delay]ed hash of
 @racket[p] or its bytecode form (i.e., @filepath{.zo} file), whichever
 exists and is newer, if @racket[p] is an extension of any path in
 @racket[paths] (i.e., exists in the directory, a subdirectory,
 etc.). Otherwise, the result is @racket[#f].

 This function is intended for use with @racket[manager-skip-file-handler].}

@defproc[(get-file-sha1 [p path?]) (or/c string? #f)]{

Computes a SHA-1 hash for the file @racket[p]; the result is
@racket[#f] if @racket[p] cannot be opened.}


@defproc[(get-compiled-file-sha1 [p path?]) (or/c string? #f)]{

Computes a SHA-1 hash for the bytecode file @racket[p], appending any
dependency-describing hash available from a @filepath{.dep} file when
available (i.e., the suffix on @racket[p] is replaced by
@filepath{.dep} to locate dependency information). The result is
@racket[#f] if @racket[p] cannot be opened.}

@defproc[(with-compile-output [p path-string?] [proc ([port input-port?] [tmp-path path?]  . -> . any)]) any]{

Opens a temporary path for writing and calls @racket[proc] passing the 
resulting @racket[port] and @racket[tmp-path].  Once @racket[proc]
returns, @racket[with-compile-output] renames @racket[tmp-path] to 
@racket[p] and arranges to delete @racket[temp-path] if there's an
exception.  Breaks are managed so that the @racket[port] is reliably
closed and the @racket[tmp-path] file is reliably deleted if there's a
break. The result of @racket[proc] is the result of the
@racket[with-compile-output] call.

Windows prevents programs from overwriting files that are open. As a result,
@racket[with-compile-output] calls to @racket[rename-file-or-directory] will
fail if the destination file argument is an open file. Windows, however, does 
allow you to rename an open file. To avoid overwriting open files
windows, @racket[with-compile-output] creates a second temporary file
@racket[tmp-path2], renames @racket[p] to @racket[tmp-path2], renames
@racket[tmp-path] to @racket[p], and finally deletes @racket[tmp-path2].}

@defparam[parallel-lock-client proc 
                               (or/c #f
                                     (->i ([command (or/c 'lock 'unlock)]
                                           [file bytes?])
                                          [res (command) (if (eq? command 'lock)
                                                             boolean?
                                                             void?)]))]{

Holds the parallel compilation lock client, which
is used by the result of @racket[make-compilation-manager-load/use-compiled-handler] to
prevent compilation races between parallel builders.  

When @racket[proc] is @racket[#f] (the default), no checking for parallel
compilation is done (and thus multiple threads or places running compilations
via @racket[make-compilation-manager-load/use-compiled-handler] will potentially
corrupt each other's @filepath{.zo} files).

When @racket[proc] is a function, its first argument is a command, indicating
if it wants to lock or unlock the path specified in the second argument.

When the @racket[proc] @racket['lock] command returns @racket[#t], the current
builder has obtained the lock for @racket[zo-path].
Once compilation of @racket[zo-path] is complete, the builder process must
release the lock by calling @racket[proc] @racket['unlock] with the exact same
@racket[zo-path].

When the @racket[proc] @racket['lock] command returns @racket[#f], another
parallel builder obtained the lock first and has already compiled the zo.  The
parallel builder should continue without compiling @racket[zo-path].
(In this case, @racket[make-compilation-manager-load/use-compiled-handler]'s
result will not call @racket[proc] with @racket['unlock].)

@examples[
  #:eval cm-eval
(let* ([lc (parallel-lock-client)]
       [zo-name  #"collects/racket/compiled/draw_rkt.zo"]
       [locked? (and lc (lc 'lock zo-name))]
       [ok-to-compile? (or (not lc) locked?)])
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (when ok-to-compile?
        (printf "Do compile here ...\n")))
    (lambda ()
      (when locked?
        (lc 'unlock zo-name)))))
]
}

@defproc[(compile-lock->parallel-lock-client [pc place-channel?] [cust (or/c #f custodian?) #f])
         (-> (or/c 'lock 'unlock) bytes? boolean?)]{

  Returns a function that follows the @racket[parallel-lock-client]
  by communicating over @racket[pc]. The argument must have 
  be the result of @racket[make-compile-lock].
  
  This communication protocol implementation is not kill safe. To make it kill safe,
  it needs a sufficiently powerful custodian, i.e., one that is not subject to
  termination (unless all of the participants in the compilation are also terminated).
  It uses this custodian to create a thread that monitors the threads that are
  doing the compilation. If one of them is terminated, the presence of the
  custodian lets another one continue. (The custodian is also used to create
  a thread that manages a thread safe table.)
}

@defproc[(make-compile-lock) place-channel?]{
  Creates a @racket[place-channel?] that can be used with
            @racket[compile-lock->parallel-lock-client] to avoid concurrent
            compilations of the same racket source files in multiple places.
}


@; ----------------------------------------------------------------------

@section[#:tag "api:parallel-build"]{API for Parallel Builds}

@defmodule[setup/parallel-build]{

The @racketmodname[setup/parallel-build] library provides the parallel-compilation
functionality of @exec{raco setup} and @exec{raco make}.}

@defproc[(parallel-compile-files [list-of-files (listof path?)] 
                                 [#:worker-count worker-count non-negative-integer? (processor-count)] 
                                 [#:handler handler (->i ([handler-type symbol?]
                                                          [path path-string?]
                                                          [msg string?] 
                                                          [out string?] 
                                                          [err string?])
                                                         void?)
                                            void])
         void?]{

The @racket[parallel-compile] utility function is used by @exec{raco make} to
compile a list of paths in parallel.  The optional
@racket[#:worker-count] argument specifies the number of compile workers to spawn during
parallel compilation.  The callback, @racket[handler], is called with the symbol
@racket['done] as the @racket[_handler-type] argument for each successfully compiled file, 
@racket['output] when a
successful compilation produces stdout/stderr output, @racket['error] when a
compilation error has occured, or @racket['fatal-error] when a unrecoverable
error occurs. The other arguments give more information for each status update.
 
  @racketblock[
    (parallel-compile-files 
      source-files 
      #:worker-count 4
      #:handler (lambda (type work msg out err)
        (match type
          ['done (when (verbose) (printf " Made ~a\n" work))]
          ['output (printf " Output from: ~a\n~a~a" work out err)]
          [else (printf " Error compiling ~a\n~a\n~a~a"
                        work 
                        msg 
                        out 
                        err)])))]
}

@defproc[(parallel-compile 
  [worker-count non-negative-integer?] 
  [setup-fprintf (->* ([stage string?] [format string?]) 
                      () 
                      #:rest (listof any/c) void)]
  [append-error (-> cc?
                    [prefix string?] 
                    [exn (or/c exn? null?)]
                    [out string?]
                    [err srtring?]
                    [message string?]
                    void?)]
  [collects-tree (listof any/c)])  (void)]{

The @racket[parallel-compile] internal utility function is used by @exec{rack
setup} to compile collects in parallel.  The @racket[worker-count] argument
specifies the number of compile workers to spawn during parallel compilation.
The @racket[setup-fprintf] and @racket[append-error] functions are internal
callback mechanisms that @exec{raco setup} uses to communicate intermediate
compilation results.  The @racket[collects-tree] argument is a compound
datastructure containing an in-memory tree representation of the collects
directory.
}

@section{Compilation Manager Hook for Syntax Transformers}

@defmodule[compiler/cm-accomplice]

@defproc[(register-external-file [file (and path? complete-path?)]) void?]{

Logs a message (see @racket[log-message]) at level @racket['info]. The
message data is a @racketidfont{file-dependency} prefab structure type
with two fields; the first field's value is @racket[file] and the second
field's value is @racket[#f] (to indicate a non-module dependency).

A compilation manager implemented by @racketmodname[compiler/cm] looks
for such messages to register an external dependency. The compilation
manager records (in a @filepath{.dep} file) the path as contributing
to the implementation of the module currently being
compiled. Afterward, if the registered file is modified, the
compilation manager will know to recompile the module.

The @racket[include] macro, for example, calls this procedure with the
path of an included file as it expands an @racket[include] form.}

@defproc[(register-external-module [file (and path? complete-path?)]) void?]{

Like @racket[register-external-file], but logs a message with a
@racketidfont{file-dependency} prefab structure type whose second
field is @racket[#t].

A compilation manager implemented by @racketmodname[compiler/cm]
recognizes the message to register a dependency on a
module (which implies a dependency on all of that module's
dependencies, etc.).}

@; ----------------------------------------------------------------------

@section[#:tag "zo"]{Compiling to Raw Bytecode}

The @DFlag{no-deps} mode for @exec{raco make} is an improverished
form of the compilation, because it does not track import
dependencies. It does, however, support compilation of non-module
source in an namespace that initially imports @racketmodname[scheme].

Outside of a module, top-level @racket[define-syntaxes],
@racket[module], @racket[#%require],
@racket[define-values-for-syntax], and @racket[begin] expressions
are handled specially by @exec{raco make --no-deps}: the compile-time
portion of the expression is evaluated, because it might affect later
expressions.

For example, when compiling the file containing

@racketblock[
(require racket/class)
(define f (class object% (super-new)))
]

the @racket[class] form from the @racketmodname[racket/class] library
must be bound in the compilation namespace at compile time. Thus, the
@racket[require] expression is both compiled (to appear in the output
code) and evaluated (for further computation).

Many definition forms expand to @racket[define-syntaxes]. For example,
@racket[define-signature] expands to @racket[define-syntaxes]. In
@DFlag{no-deps} mode, @exec{raco make --no-deps} detects
@racket[define-syntaxes] and other expressions after expansion, so
top-level @racket[define-signature] expressions affect the compilation
of later expressions, as a programmer would expect.

In contrast, a @racket[load] or @racket[eval] expression in a source
file is compiled---but @emph{not evaluated!}---as the source file is
compiled.  Even if the @racket[load] expression loads syntax or
signature definitions, these will not be loaded as the file is
compiled. The same is true of application expressions that affect the
reader, such as @racket[(read-case-sensitive #t)]. The @Flag{p} or
@DFlag{prefix} flag for @exec{raco make} takes a file and loads it before
compiling the source files specified on the command line.

By default, the namespace for compilation is initialized by a
@racket[require] of @racketmodname[scheme]. If the @DFlag{no-prim}
flag is specified, the namespace is instead initialized with
@racket[namespace-require/copy], which allows mutation and
redefinition of all initial bindings (other than syntactic forms, in
the case of mutation).

In general, a better solution is to put all code to compile into a
module and use @exec{raco make} in its default mode.

@(close-eval cm-eval)
