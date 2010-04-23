#lang scribble/doc
@(require "mz.ss"
          scheme/sandbox
          (for-label scheme/sandbox
                     racket/port
                     (only-in racket/gui make-gui-namespace)
                     racket/gui/dynamic))

@(define box-eval (make-base-eval))
@(interaction-eval #:eval box-eval (require scheme/sandbox))

@title{Sandboxed Evaluation}

@note-lib-only[scheme/sandbox]

The @schememodname[scheme/sandbox] module provides utilities for
creating ``sandboxed'' evaluators, which are configured in a
particular way and can have restricted resources (memory and time),
filesystem and network access, and much more.  Sandboxed evaluators can be
configured through numerous parameters --- and the defaults are set
for the common use case where sandboxes are very limited.

@defproc*[([(make-evaluator [language (or/c module-path?
                                            (list/c 'special symbol?)
                                            (cons/c 'begin list?))]
                            [input-program any/c] ...
                            [#:requires requires (listof (or/c module-path? path?))]
                            [#:allow-read allow (listof (or/c module-path? path?))])
            (any/c . -> . any)]
           [(make-module-evaluator [module-decl (or/c syntax? pair?)]
                                   [#:language   lang  (or/c #f module-path?)]
                                   [#:allow-read allow (listof (or/c module-path? path?))])
            (any/c . -> . any)])]{

The @scheme[make-evaluator] function creates an evaluator with a
@scheme[language] and @scheme[requires] specification, and starts
evaluating the given @scheme[input-program]s. The
@scheme[make-module-evaluator] function creates an evaluator that
works in the context of a given module. The result in either case is a
function for further evaluation.

The returned evaluator operates in an isolated and limited
environment.  In particular, filesystem access is restricted.  The
@scheme[allow] argument extends the set of files that are readable by
the evaluator to include the specified modules and their imports
(transitively). When @scheme[language] is a module path and when
@scheme[requires] is provided, the indicated modules are implicitly
included in the @scheme[allow] list.

Each @scheme[input-program] or @scheme[module-decl] argument provides
a program in one of the following forms:

@itemize[

 @item{an input port used to read the program;}

 @item{a string or a byte string holding the complete input;}

 @item{a path that names a file holding the input; or}

 @item{an S-expression or a @tech{syntax object}, which is evaluated
       as with @scheme[eval] (see also
       @scheme[get-uncovered-expressions]).}
]

In the first three cases above, the program is read using
@scheme[sandbox-reader], with line-counting enabled for sensible error
messages, and with @scheme['program] as the source (used for testing
coverage).  In the last case, the input is expected to be the complete
program, and is converted to a @tech{syntax object} (using
@scheme['program] as the source), unless it already is a @tech{syntax
object}.

The returned evaluator function accepts additional expressions
(each time it is called) in essentially the same form: a string or
byte string holding a sequence of expressions, a path for a file
holding expressions, an S-expression, or a @tech{syntax object}.  If
the evaluator receives an @scheme[eof] value, it is terminated and
raises errors thereafter.  See also @scheme[kill-evaluator], which
terminates the evaluator without raising an exception.

For @scheme[make-evaluator], multiple @scheme[input-program]s are
effectively concatenated to form a single program. The way that the
@scheme[input-program]s are evaluated depends on the @scheme[language]
argument:

@itemize[

 @item{The @scheme[language] argument can be a module path (i.e., a
       datum that matches the grammar for @scheme[_module-path] of
       @scheme[require]).

       In this case, the @scheme[input-program]s are automatically
       wrapped in a @scheme[module], and the resulting evaluator works
       within the resulting module's namespace.}

 @item{The @scheme[language] argument can be a list starting with
       @scheme['special], which indicates a built-in language with
       special input configuration. The possible values are
       @scheme['(special r5rs)] or a value indicating a teaching
       language: @scheme['(special beginner)], @scheme['(special
       beginner-abbr)], @scheme['(special intermediate)],
       @scheme['(special intermediate-lambda)], or @scheme['(special
       advanced)].

       In this case, the @scheme[input-program]s are automatically
       wrapped in a @scheme[module], and the resulting evaluator works
       within the resulting module's namespace. In addition, certain
       parameters (such as such as @scheme[read-accept-infix-dot]) are
       set to customize reading programs from strings and ports.

       This option is provided mainly for older test systems. Using
       @scheme[make-module-evaluator] with input starting with
       @schememodfont{#lang} is generally better.}

 @item{Finally, @scheme[language] can be a list whose first element is
       @scheme['begin].

       In this case, a new namespace is created using
       @scheme[sandbox-namespace-specs], which by default creates a
       new namespace using @scheme[make-base-namespace] or
       @scheme[make-gui-namespace] (depending on @scheme[gui?]).

       In the new namespace, @scheme[language] is evaluated as an
       expression to further initialize the namespace.}

]

The @scheme[requires] list adds additional imports to the module or
namespace for the @scheme[input-program]s, even in the case that
@scheme[require] is not made available through the @scheme[language].

The following examples illustrate the difference between an evaluator
that puts the program in a module and one that merely initializes a
top-level namespace:

@interaction[
#:eval box-eval
(define base-module-eval 
  (code:comment @#,t{a module cannot have free variables...})
  (make-evaluator 'racket/base '(define (f) later)))
(define base-module-eval 
  (make-evaluator 'racket/base '(define (f) later)
                               '(define later 5)))
(base-module-eval '(f))

(define base-top-eval 
  (code:comment @#,t{non-module code can have free variables:})
  (make-evaluator '(begin) '(define (f) later)))
(base-top-eval '(+ 1 2))
(base-top-eval '(define later 5))
(base-top-eval '(f))
]

The @scheme[make-module-evaluator] function is essentially a
restriction of @scheme[make-evaluator], where the program must be a
module, and all imports are part of the program.  In some cases it is
useful to restrict the program to be a module using a spcific module
in its language position --- use the optional @scheme[lang] argument
to specify such a restriction (the default, @scheme[#f], means no
restriction is enforced).

@schemeblock[
(define base-module-eval2
  (code:comment @#,t{equivalent to @scheme[base-module-eval]:})
  (make-module-evaluator '(module m racket/base
                            (define (f) later)
                            (define later 5))))
]

@scheme[make-module-evaluator] can be very convenient for testing
module files: all you need to do is pass in a path value for the file
name, and you get back an evaluator in the module's context which you
can use with your favorite test facility.

In all cases, the evaluator operates in an isolated and limited
environment:
@itemize[

 @item{It uses a new custodian and namespace. When @scheme[gui?] is
       true, it is also runs in its own eventspace.}

 @item{The evaluator works under the @scheme[sandbox-security-guard],
       which restricts file system and network access.}

 @item{The evaluator is contained in a memory-restricted environment,
       and each evaluation is wrapped in a @scheme[call-with-limits]
       (when memory accounting is available); see also
       @scheme[sandbox-memory-limit], @scheme[sandbox-eval-limits] and
       @scheme[set-eval-limits].}
]
Note that these limits apply to the creation of the sandbox
environment too --- so, for example, if the memory that is required to
create the sandbox is higher than the limit, then
@scheme[make-evaluator] will fail with a memory limit exception.

The sandboxed evironment is well isolated, and the evaluator function
essentially sends it an expression and waits for a result.  This form
of communication makes it impossible to have nested (or concurrent)
calls to a single evaluator.  Usually this is not a problem, but in
some cases you can get the evaluator function available inside the
sandboxed code, for example:
@schemeblock[
(let ([e (make-evaluator 'racket/base)])
  (e (,e 1)))
]
An error will be signalled in such cases.

Evaluation can also be instrumented to track coverage information when
@scheme[sandbox-coverage-enabled] is set. Exceptions (both syntax and
run-time) are propagated as usual to the caller of the evaluation
function (i.e., catch it with @scheme[with-handlers]).  However, note
that a sandboxed evaluator is convenient for testing, since all
exceptions happen in the same way, so you don't need special code to
catch syntax errors.

Finally, the fact that a sandboxed evaluator accept syntax objects
makes it usable as the value for @scheme[current-eval], which means
that you can easily start a sandboxed read-eval-print-loop.  For
example, here is a quick implementation of a networked REPL:

@schemeblock[
(define e (make-evaluator 'racket/base))
(let-values ([(i o) (tcp-accept (tcp-listen 9999))])
  (parameterize ([current-input-port  i]
                 [current-output-port o]
                 [current-error-port  o]
                 [current-eval e])
    (read-eval-print-loop)
    (fprintf o "\nBye...\n")
    (close-output-port o)))
]

Note that in this code it is only the REPL interactions that are going
over the network connection; using I/O operations inside the REPL will
still use the usual sandbox parameters (defaulting to no I/O).  In
addition, the code works only from an existing toplevel REPL ---
specifically, @scheme[read-eval-print-loop] reads a syntax value and
gives it the lexical context of the current namespace.  Here is a
variation that uses the networked ports for user I/O, and works when
used from a module (by using a new namespace):

@schemeblock[
(let-values ([(i o) (tcp-accept (tcp-listen 9999))])
  (parameterize ([current-input-port   i]
                 [current-output-port  o]
                 [current-error-port   o]
                 [sandbox-input        i]
                 [sandbox-output       o]
                 [sandbox-error-output o]
                 [current-namespace (make-empty-namespace)])
    (parameterize ([current-eval
                    (make-evaluator 'racket/base)])
      (read-eval-print-loop))
    (fprintf o "\nBye...\n")
    (close-output-port o)))
]

}


@defproc*[([(exn:fail:sandbox-terminated? [v any/c]) boolean?]
           [(exn:fail:sandbox-terminated-reason [exn exn:fail:sandbox-terminated?])
            symbol/c])]{

A predicate and accessor for exceptions that are raised when a sandbox
is terminated.  Once a sandbox raises such an exception, it will
continue to raise it on further evaluation attempts.

@scheme[call-with-limits].  The @scheme[resource] field holds a symbol,
either @scheme['time] or @scheme['memory].}


@; ----------------------------------------------------------------------

@section{Customizing Evaluators}

The sandboxed evaluators that @scheme[make-evaluator] creates can be
customized via many parameters.  Most of the configuration parameters
affect newly created evaluators; changing them has no effect on
already-running evaluators.

The default configuration options are set for a very restricted
sandboxed environment --- one that is safe to make publicly available.
Further customizations might be needed in case more privileges are
needed, or if you want tighter restrictions.  Another useful approach
for customizing an evaluator is to begin with a relatively
unrestricted configuration and add the desired restrictions.  This is
possible by the @scheme[call-with-trusted-sandbox-configuration]
function.

@defproc[(call-with-trusted-sandbox-configuration [thunk (-> any)])
         any]{

Invokes the @scheme[thunk] in a context where sandbox configuration
parameters are set for minimal restrictions.  More specifically, there
are no memory or time limits, and the existing existing inspectors,
security guard, exit handler, and logger are used.  (Note that the I/O
ports settings are not included.)}


@defparam[sandbox-init-hook thunk (-> any)]{

A parameter that determines a thunk to be called for initializing a
new evaluator.  The hook is called just before the program is
evaluated in a newly-created evaluator context.  It can be used to
setup environment parameters related to reading, writing, evaluation,
and so on.  Certain languages (@scheme['(special r5rs)] and the
teaching languages) have initializations specific to the language; the
hook is used after that initialization, so it can override settings.}


@defparam[sandbox-reader proc (any/c . -> . any)]{

A parameter that specifies a function that reads all expressions from
@scheme[(current-input-port)].  The function is used to read program
source for an evaluator when a string, byte string, or port is
supplied.  The reader function receives a value to be used as input
source (i.e., the first argument to @scheme[read-syntax]), and it
should return a list of @tech{syntax objects}.  The default reader
calls @scheme[read-syntax], accumulating results in a list until it
receives @scheme[eof].}


@defparam[sandbox-input in (or/c #f
                                 string? bytes?
                                 input-port?
                                 'pipe
                                 (-> input-port?))]{

A parameter that determines the initial @scheme[current-input-port]
setting for a newly created evaluator. It defaults to @scheme[#f],
which creates an empty port.  The following other values are allowed:

@itemize[

 @item{a string or byte string, which is converted to a port using
       @scheme[open-input-string] or @scheme[open-input-bytes];}

 @item{an input port;}

 @item{the symbol @scheme['pipe], which triggers the creation of a
       pipe, where @scheme[put-input] can return the output end of the
       pipe or write directly to it;}

 @item{a thunk, which is called to obtain a port (e.g., using
       @scheme[current-input-port] means that the evaluator input is
       the same as the calling context's input).}

]}


@defparam[sandbox-output in (or/c #f
                                  output-port? 
                                  'pipe
                                  'bytes
                                  'string
                                  (-> output-port?))]{

A parameter that determines the initial @scheme[current-output-port]
setting for a newly created evaluator. It defaults to @scheme[#f],
which creates a port that discrds all data.  The following other
values are allowed:

@itemize[

 @item{an output port, which is used as-is;}

 @item{the symbol @scheme['bytes], which causes @scheme[get-output]
       to return the complete output as a byte string;}

 @item{the symbol @scheme['string], which is similar to
       @scheme['bytes], but makes @scheme[get-output] produce a
       string;}

 @item{the symbol @scheme['pipe], which triggers the creation of a
       pipe, where @scheme[get-output] returns the input end of the
       pipe;}

 @item{a thunk, which is called to obtain a port (e.g., using
       @scheme[current-output-port] means that the evaluator output is
       not diverted).}

]}


@defparam[sandbox-error-output in (or/c #f
                                        output-port? 
                                        'pipe
                                        'bytes
                                        'string
                                        (-> output-port?))]{

Like @scheme[sandbox-output], but for the initial
@scheme[current-error-port] value. An evaluator's error output is set
after its output, so using @scheme[current-output-port] (the parameter
itself, not its value) for this parameter value means that the error
port is the same as the evaluator's initial output port.

The default is @scheme[(lambda () (dup-output-port
(current-error-port)))], which means that the error output of the
generated evaluator goes to the calling context's error port.}


@defboolparam[sandbox-coverage-enabled enabled?]{

A parameter that controls whether syntactic coverage information is
collected by sandbox evaluators.  Use
@scheme[get-uncovered-expressions] to retrieve coverage information.}


@defboolparam[sandbox-propagate-breaks propagate?]{

When both this boolean parameter and @scheme[(break-enabled)] are true, 
breaking while an evaluator is
running propagates the break signal to the sandboxed
context.  This makes the sandboxed evaluator break, typically, but
beware that sandboxed evaluation can capture and avoid the breaks (so
if safe execution of code is your goal, make sure you use it with a
time limit).  Also, beware that a break may be received after the
evaluator's result, in which case the evaluation result is lost. Finally,
beware that a break may be propagated after an evaluator has produced
a result, so that the break is visible on the next interaction with
the evaluator (or the break is lost if the evaluator is not used
further). The default is @scheme[#t].}


@defparam[sandbox-namespace-specs spec (cons/c (-> namespace?) 
                                               (listof module-path?))]{

A parameter that holds a list of values that specify how to create a
namespace for evaluation in @scheme[make-evaluator] or
@scheme[make-module-evaluator].  The first item in the list is a thunk
that creates the namespace, and the rest are module paths for modules
to be attached to the created namespace using
@scheme[namespace-attach-module].

The default is @scheme[(list make-base-namespace)] if @scheme[gui?] is
@scheme[#f], @scheme[(list make-gui-namespace)] if @scheme[gui?] is
@scheme[#t].

The module paths are needed for sharing module instantiations between
the sandbox and the caller.  For example, sandbox code that returns
@scheme[posn] values (from the @schemeidfont{lang/posn} module) will
not be recognized as such by your own code by default, since the
sandbox will have its own instance of @schemeidfont{lang/posn} and
thus its own struct type for @scheme[posn]s.  To be able to use such
values, include @scheme['lang/posn] in the list of module paths.

When testing code that uses a teaching language, the following piece
of code can be helpful:

@schemeblock[
(sandbox-namespace-specs
 (let ([specs (sandbox-namespace-specs)])
   `(,(car specs)
     ,@(cdr specs)
     lang/posn
     ,@(if gui? '(mrlib/cache-image-snip) '()))))
]}


@defparam[sandbox-override-collection-paths paths (listof path-string?)]{

A parameter that determines a list of collection directories to prefix
@scheme[current-library-collection-paths] in an evaluator. This
parameter is useful for cases when you want to test code using an
alternate, test-friendly version of a collection, for example, testing
code that uses a GUI (like the @scheme[htdp/world] teachpack) can be
done using a fake library that provides the same interface but no
actual interaction. The default is @scheme[null].}


@defparam[sandbox-security-guard guard
          (or/c security-guard? (-> security-guard?))]{

A parameter that determines the initial
@scheme[(current-security-guard)] for sandboxed evaluations.  It can
be either a security guard, or a function to construct one.  The
default is a function that restricts the access of the current
security guard by forbidding all filesystem I/O except for
specifications in @scheme[sandbox-path-permissions], and it uses
@scheme[sandbox-network-guard] for network connections.}


@defparam[sandbox-path-permissions perms
          (listof (list/c (or/c 'execute 'write 'delete 
                                'read-bytecode 'read 'exists)
                          (or/c byte-regexp? bytes? string? path?)))]{

A parameter that configures the behavior of the default sandbox
security guard by listing paths and access modes that are allowed for
them.  The contents of this parameter is a list of specifications,
each is an access mode and a byte-regexp for paths that are granted this
access.

The access mode symbol is one of: @scheme['execute], @scheme['write],
@scheme['delete], @scheme['read], or @scheme['exists].  These symbols
are in decreasing order: each implies access for the following modes
too (e.g., @scheme['read] allows reading or checking for existence).

The path regexp is used to identify paths that are granted access.  It
can also be given as a path (or a string or a byte string), which is
(made into a complete path, cleansed, simplified, and then) converted
to a regexp that allows the path and sub-directories; e.g.,
@scheme["/foo/bar"] applies to @scheme["/foo/bar/baz"].

An additional mode symbol, @scheme['read-bytecode], is not part of the
linear order of these modes.  Specifying this mode is similar to
specifying @scheme['read], but it is not implied by any other mode.
(For example, even if you specify @scheme['write] for a certain path,
you need to also specify @scheme['read-bytecode] to grant this
permission.)  The sandbox usually works in the context of a lower code
inspector (see @scheme[sandbox-make-code-inspector]) which prevents
loading of untrusted bytecode files --- the sandbox is set-up to allow
loading bytecode from files that are specified with
@scheme['read-bytecode].  This specification is given by default to
the PLT collection hierarchy (including user-specific libraries) and
to libraries that are explicitly specified in an @scheme[#:allow-read]
argument.  (Note that this applies for loading bytecode files only,
under a lower code inspector it is still impossible to use protected
module bindings (see @secref["modprotect"]).)

The default value is null, but when an evaluator is created, it is
augmented by @scheme['read-bytecode] permissions that make it possible
to use collection libraries (including
@scheme[sandbox-override-collection-paths]). See
@scheme[make-evalautor] for more information.}


@defparam[sandbox-network-guard proc
          (symbol?
           (or/c (and/c string? immutable?) #f)
           (or/c (integer-in 1 65535) #f)
           (or/c 'server 'client)
           . -> . any)]{

A parameter that specifieds a procedure to be used (as is) by the
default @scheme[sandbox-security-guard].  The default forbids all
network connection.}


@defparam[sandbox-exit-handler handler (any/c . -> . any)]{

A parameter that determines the initial @scheme[(exit-handler)] for
sandboxed evaluations.  The default kills the evaluator with an
appropriate error message (see
@scheme[exn:fail:sandbox-terminated-reason]).}


@defparam[sandbox-memory-limit limit (or/c nonnegative-number? #f)]{

A parameter that determines the total memory limit on the sandbox in
megabytes (it can hold a rational or a floating point number).  When
this limit is exceeded, the sandbox is terminated.  This value is used
when the sandbox is created and the limit cannot be changed
afterwards.  It defaults to 30mb.  See @scheme[sandbox-eval-limits]
for per-evaluation limits and a description of how the two limits work
together.

Note that (when memory accounting is enabled) memory is attributed to
the highest custodian that refers to it.  This means that if you
inspect a value that sandboxed evaluation returns outside of the
sandbox, your own custodian will be charged for it.  To ensure that it
is charged back to the sandbox, you should remove references to such
values when the code is done inspecting it.

This policy has an impact on how the sandbox memory limit interacts
with the per-expression limit specified by
@scheme[sandbox-eval-limits]: values that are reachable from the
sandbox, as well as from the interaction will count against the
sandbox limit.  For example, in the last interaction of this code,
@schemeblock[
  (define e (make-evaluator 'racket/base))
  (e '(define a 1))
  (e '(for ([i (in-range 20)]) (set! a (cons (make-bytes 500000) a))))
]
the memory blocks are allocated within the interaction limit, but
since they're chained to the defined variable, they're also reachable
from the sandbox --- so they will count against the sandbox memory
limit but not against the interaction limit (more precisely, no more
than one block counts against the interaction limit).}


@defparam[sandbox-eval-limits limits
          (or/c (list/c (or/c nonnegative-number? #f)
                        (or/c nonnegative-number? #f))
                #f)]{

A parameter that determines the default limits on @italic{each} use of
a @scheme[make-evaluator] function, including the initial evaluation
of the input program.  Its value should be a list of two numbers;
where the first is a timeout value in seconds, and the second is a
memory limit in megabytes (note that they don't have to be integers).
Either one can be @scheme[#f] for disabling the corresponding limit;
alternately, the parameter can be set to @scheme[#f] to disable all
per-evaluation limits (useful in case more limit kinds are available
in future versions). The default is @scheme[(list 30 20)].

Note that these limits apply to the creation of the sandbox
environment too --- even @scheme[(make-evaluator 'racket/base)] can
fail if the limits are strict enough.  For example,
@schemeblock[
  (parameterize ([sandbox-eval-limits '(0.25 5)])
    (make-evaluator 'racket/base '(sleep 2)))
]
will throw an error instead of creating an evaluator.  Therefore, to
avoid surprises you need to catch errors that happen when the sandbox
is created.

When limits are set, @scheme[call-with-limits] (see below) is wrapped
around each use of the evaluator, so consuming too much time or memory
results in an exception.  Change the limits of a running evaluator
using @scheme[set-eval-limits].

@margin-note{A custodian's limit is checked only after a garbage
             collection, except that it may also be checked during
             certain large allocations that are individually larger
             than the custodian's limit.}

The memory limit that is specified by this parameter applies to each
individual evaluation, but not to the whole sandbox --- that limit is
specified via @scheme[sandbox-memory-limit].  When the global limit is
exceeded, the sandbox is terminated, but when the per-evaluation limit
is exceeded the @exnraise[exn:fail:resource].  For example, say that
you evaluate an expression like
@schemeblock[
  (for ([i (in-range 1000)])
    (set! a (cons (make-bytes 1000000) a))
    (collect-garbage))
]
then, assuming sufficiently small limits,
@itemize[

 @item{if a global limit is set but no per-evaluation limit, the
       sandbox will eventually be terminated and no further
       evaluations possible;}

 @item{if there is a per-evaluation limit, but no global limit, the
       evaluation will abort with an error and it can be used again
       --- specifically, @scheme[a] will still hold a number of
       blocks, and you can evaluate the same expression again which
       will add more blocks to it;}

  @item{if both limits are set, with the global one larger than the
        per-evaluation limit, then the evaluation will abort and you
        will be able to repeat it, but doing so several times will
        eventually terminate the sandbox (this will be indicated by
        the error message, and by the @scheme[evaluator-alive?]
        predicate).}

]}


@defparam[sandbox-eval-handlers handlers
          (list/c (or/c #f ((-> any) . -> . any))
                  (or/c #f ((-> any) . -> . any)))]{

A parameter that determines two (optional) handlers that wrap
sandboxed evaluations.  The first one is used when evaluating the
initial program when the sandbox is being set-up, and the second is
used for each interaction.  Each of these handlers should expect a
thunk as an argument, and they should execute these thunks ---
possibly imposing further restrictions.  The default values are
@scheme[#f] and @scheme[call-with-custodian-shutdown], meaning no
additional restrictions on initial sandbox code (e.g., it can start
background threads), and a custodian-shutdown around each interaction
that follows.  Another useful function for this is
@scheme[call-with-killing-threads] which kills all threads, but leaves
other resources intact.}


@defparam[sandbox-make-inspector make (-> inspector?)]{

A parameter that determines the procedure used to create the inspector
for sandboxed evaluation.  The procedure is called when initializing
an evaluator, and the default parameter value is
@scheme[make-inspector].}


@defparam[sandbox-make-code-inspector make (-> inspector?)]{

A parameter that determines the procedure used to create the code
inspector for sandboxed evaluation.  The procedure is called when
initializing an evaluator, and the default parameter value is
@scheme[make-inspector].  The @scheme[current-load/use-compiled]
handler is setup to still allow loading of bytecode files under the
original code inspector when @scheme[sandbox-path-permissions] allows
it through a @scheme['read-bytecode] mode symbol, to make it possible
to load libraries.}


@defparam[sandbox-make-logger make (-> logger?)]{

A parameter that determines the procedure used to create the logger
for sandboxed evaluation.  The procedure is called when initializing
an evaluator, and the default parameter value is
@scheme[current-logger].  This means that it is not creating a new
logger (this might change in the future).}

@; ----------------------------------------------------------------------

@section{Interacting with Evaluators}

The following functions are used to interact with a sandboxed
evaluator in addition to using it to evaluate code.


@defproc[(evaluator-alive? [evaluator (any/c . -> . any)]) boolean?]{

Determines whether the evaluator is still alive.}


@defproc[(kill-evaluator [evaluator (any/c . -> . any)]) void?]{

Releases the resources that are held by @scheme[evaluator] by shutting
down the evaluator's custodian.  Attempting to use an evaluator after
killing raises an exception, and attempts to kill a dead evaluator are
ignored.

Killing an evaluator is similar to sending an @scheme[eof] value to
the evaluator, except that an @scheme[eof] value will raise an error
immediately.}


@defproc[(break-evaluator [evaluator (any/c . -> . any)]) void?]{

Sends a break to the running evaluator.  The effect of this is as if
Ctrl-C was typed when the evaluator is currently executing, which
propagates the break to the evaluator's context.}


@defproc[(set-eval-limits [evaluator (any/c . -> . any)]
                          [secs (or/c exact-nonnegative-integer? #f)]
                          [mb (or/c exact-nonnegative-integer? #f)])
         void?]{

Changes the per-expression limits that @scheme[evaluator] uses to
@scheme[sec] seconds and @scheme[mb] megabytes (either one can be
@scheme[#f], indicating no limit).

This procedure should be used to modify an existing evaluator limits,
because changing the @scheme[sandbox-eval-limits] parameter does not
affect existing evaluators. See also @scheme[call-with-limits].}


@defproc[(set-eval-handler [evaluator (any/c . -> . any)]
                           [handler (or/c #f ((-> any) . -> . any))])
         void?]{

Changes the per-expression handler that the @scheme[evaluator] uses
around each interaction.  A @scheme[#f] value means no handler is
used.

This procedure should be used to modify an existing evaluator handler,
because changing the @scheme[sandbox-eval-handlers] parameter does not
affect existing evaluators. See also
@scheme[call-with-custodian-shutdown] and
@scheme[call-with-killing-threads] for two useful handlers that are
provided.}


@defproc*[([(call-with-custodian-shutdown [thunk (-> any)]) any]
           [(call-with-killing-threads [thunk (-> any)]) any])]{

These functions are useful for use as an evaluation handler.
@scheme[call-with-custodian-shutdown] will execute the @scheme[thunk]
in a fresh custodian, then shutdown that custodian, making sure that
@scheme[thunk] could not have left behind any resources.
@scheme[call-with-killing-threads] is similar, except that it kills
threads that were left, but leaves other resources as is.}


@defproc*[([(put-input [evaluator (any/c . -> . any)]) output-port?]
           [(put-input [evaluator (any/c . -> . any)]
                       [i/o (or/c bytes? string? eof-object?)]) void?])]{

If @scheme[(sandbox-input)] is @scheme['pipe] when an evaluator is
created, then this procedure can be used to retrieve the output port
end of the pipe (when used with no arguments), or to add a string or a
byte string into the pipe.  It can also be used with @scheme[eof],
which closes the pipe.}


@defproc*[([(get-output [evaluator (any/c . -> . any)]) (or/c #f input-port? bytes? string?)]
           [(get-error-output [evaluator (any/c . -> . any)]) (or/c #f input-port? bytes? string?)])]{

Returns the output or error-output of the @scheme[evaluator],
in a way that depends on the setting of @scheme[(sandbox-output)] or
@scheme[(sandbox-error-output)] when the evaluator was created:

@itemize[

 @item{if it was @scheme['pipe], then @scheme[get-output] returns the
      input port end of the created pipe;}

 @item{if it was @scheme['bytes] or @scheme['string], then the result
       is the accumulated output, and the output port is reset so each
       call returns a different piece of the evaluator's output (note
       that any allocations of such output are still subject to the
       sandbox memory limit);}

  @item{otherwise, it returns @scheme[#f].}
]}


@defproc[(get-uncovered-expressions [evaluator (any/c . -> . any)]
                                    [prog? any/c #t]
                                    [src any/c 'program])
         (listof syntax?)]{

Retrieves uncovered expression from an evaluator, as longs as the
@scheme[sandbox-coverage-enabled] parameter had a true value when the
evaluator was created. Otherwise, an exception is raised to indicate
that no coverage information is available.

The @scheme[prog?] argument specifies whether to obtain expressions that
were uncovered after only the original input program was evaluated
(@scheme[#t]) or after all later uses of the evaluator (@scheme[#f]).
Using @scheme[#t] retrieves a list that is saved after the input
program is evaluated, and before the evaluator is used, so the result is
always the same.

A @scheme[#t] value of @scheme[prog?] is useful for testing student
programs to find out whether a submission has sufficient test coverage
built in. A @scheme[#f] value is useful for writing test suites for a
program to ensure that your tests cover the whole code.

The second optional argument, @scheme[src], specifies that the result
should be filtered to hold only @tech{syntax objects} whose source
matches @scheme[src]. The default, @scheme['program], is the source
associated with the input program by the default
@scheme[sandbox-reader]---which provides only @tech{syntax objects}
from the input program (and not from required modules or expressions
that were passed to the evaluator). A @scheme[#f] avoids filtering.

The resulting list of @tech{syntax objects} has at most one expression
for each position and span.  Thus, the contents may be unreliable, but
the position information is reliable (i.e., it always indicates source
code that would be painted red in DrScheme when coverage information
is used).

Note that if the input program is a sequence of syntax values, either
make sure that they have @scheme['program] as the source field, or use
the @scheme[src] argument.  Using a sequence of S-expressions (not
@tech{syntax objects}) for an input program leads to unreliable
coverage results, since each expression may be assigned a single
source location.}

@defproc[(call-in-sandbox-context [evaluator (any/c . -> . any)]
                                  [thunk (-> any)]
                                  [unrestricted? boolean? #f])
         any]{

Calls the given @scheme[thunk] in the context of a sandboxed
evaluator.  The call is performed under the resource limits and
evaluation handler that are used for evaluating expressions, unless
@scheme[unrestricted?] is specified as true.

This is usually similar to @scheme[(evaluator (list thunk))], except
that this relies on the common meaning of list expressions as function
application (which is not true in all languages), and it relies on
MzScheme's @scheme[eval] forgiving a non-S-expression input.  In
addition, you can avoid some of the sandboxed restrictions by using
your own permissions, for example,
@schemeblock[
  (let ([guard (current-security-guard)])
    (call-in-sandbox-context
      (lambda ()
        (parameterize ([current-security-guard guard])
          (code:comment @#,t{can access anything you want here})
          ))))
]}

@; ----------------------------------------------------------------------

@section{Miscellaneous}

@defthing[gui? boolean?]{

True if the @schememodname[racket/gui] module can be used, @scheme[#f]
otherwise; see @scheme[gui-available?].

Various aspects of the @schememodname[scheme/sandbox] library change
when the GUI library is available, such as using a new eventspace for
each evaluator.}


@defproc[(call-with-limits [secs (or/c exact-nonnegative-integer? #f)]
                           [mb (or/c exact-nonnegative-integer? #f)]
                           [thunk (-> any)])
         any]{

Executes the given @scheme[thunk] with memory and time restrictions:
if execution consumes more than @scheme[mb] megabytes or more than
@scheme[sec] seconds, then the computation is aborted and the
@exnraise[exn:fail:resource].  Otherwise the result of the thunk is
returned as usual (a value, multiple values, or an exception).  Each
of the two limits can be @scheme[#f] to indicate the absence of a
limit. See also @scheme[custodian-limit-memory] for information on
memory limits.

Sandboxed evaluators use @scheme[call-with-limits], according to the
@scheme[sandbox-eval-limits] setting and uses of
@scheme[set-eval-limits]: each expression evaluation is protected from
timeouts and memory problems. Use @scheme[call-with-limits] directly
only to limit a whole testing session, instead of each expression.}


@defform[(with-limits sec-expr mb-expr body ...)]{

A macro version of @scheme[call-with-limits].}


@defproc*[([(exn:fail:resource? [v any/c]) boolean?]
           [(exn:fail:resource-resource [exn exn:fail:resource?])
            (or/c 'time 'memory)])]{

A predicate and accessor for exceptions that are raised by
@scheme[call-with-limits].  The @scheme[resource] field holds a symbol,
either @scheme['time] or @scheme['memory].}

@; ----------------------------------------------------------------------

@close-eval[box-eval]
