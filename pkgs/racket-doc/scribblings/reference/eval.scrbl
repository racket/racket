#lang scribble/doc
@(require "mz.rkt"
          (for-label setup/dirs))

@title[#:tag "eval"]{Evaluation and Compilation}

@guideintro["reflection"]{dynamic evaluation}

Racket provides programmatic control over evaluation through
@racket[eval] and related functions. See @secref["compiler"] for
information about extra-linguistic facilities related to the Racket
compiler.

@defparam[current-eval proc (any/c . -> . any)]{

A @tech{parameter} that determines the current @deftech{evaluation handler}.
The evaluation handler is a procedure that takes a top-level form and
evaluates it, returning the resulting values. The @tech{evaluation
handler} is called by @racket[eval], @racket[eval-syntax], the default
@tech{load handler}, and @racket[read-eval-print-loop] to evaluate a
top-level form. The handler should evaluate its argument in tail
position.

The @racket[_top-level-form] provided to the handler can be a
@tech{syntax object}, a compiled form, a compiled form wrapped as a
syntax object, or an arbitrary datum.

The default handler converts an arbitrary datum to a syntax object
using @racket[datum->syntax], and then enriches its @tech{lexical
information} in the same way as @racket[eval]. (If
@racket[_top-level-form] is a syntax object, then its @tech{lexical
information} is not enriched.)  The default evaluation handler
partially expands the form to splice the body of top-level
@racket[begin] forms into the top level (see
@racket[expand-to-top-form]), and then individually compiles and
evaluates each spliced form before continuing to expand, compile, and
evaluate later forms.}


@defproc*[([(eval [top-level-form any/c]) any]
           [(eval [top-level-form any/c] [namespace namespace?]) any])]{

@guidealso["namespaces"]

Calls the current @tech{evaluation handler} to evaluate
@racket[top-level-form]. The evaluation handler is called in
tail position with respect to the @racket[eval] call. An evaluation
handler uses the @tech{current namespace}; in the two-argument case of
@racket[eval], the call to the evaluation handler is
@racket[parameterize]d to set @racket[current-namespace] to
@racket[namespace].

If @racket[top-level-form] is a syntax object whose datum is not a
compiled form, then its @tech{lexical information} is enriched before
it is sent to the @tech{evaluation handler}:

@itemize[

 @item{If @racket[top-level-form] is a pair whose @racket[car] is a
       symbol or identifier, and if applying
       @racket[namespace-syntax-introduce] to the
       (@racket[datum->syntax]-converted) identifier produces an
       identifier bound to @racket[module] in a @tech{phase level}
       that corresponds to @racket[namespace]'s @tech{base phase},
       then only that identifier is enriched.}

 @item{For any other @racket[top-level-form],
       @racket[namespace-syntax-introduce] is applied to the entire
       syntax object.}

]

For interactive evaluation in the style of
@racket[read-eval-print-loop] and @racket[load], wrap each expression
with @racketidfont{#%top-interaction}, which is normally bound to
@racket[#%top-interaction], before passing it to @racket[eval].}

@defproc*[([(eval-syntax [stx syntax?]) any]
           [(eval-syntax [stx syntax?] [namespace namespace?]) any])]{

Like @racket[eval], except that @racket[stx] must be a syntax object,
and its lexical context is not enriched before it is passed to the
@tech{evaluation handler}.}


@defparam[current-load proc (path? (or/c #f
                                         symbol?
                                         (cons/c (or/c #f symbol?)
                                                 (non-empty-listof symbol?)))
                                   . -> . 
                                   any)]{

A @tech{parameter} that determines the current @deftech{load handler} to load
top-level forms from a file. The @tech{load handler} is called by
@racket[load], @racket[load-relative], @racket[load/cd], and the
default @tech{compiled-load handler}.

A load handler takes two arguments: a path (see @secref["pathutils"])
and an expected module name. The expected module name is a symbol or a
list when the call is to load a module declaration in response to a
@racket[require] (in which case the file should contain a module
declaration), or @racket[#f] for any other load.

When loading a module from a stream that starts with a compiled module
that contains submodules, the load handler should load only the
requested module, where a symbol as the load handler's indicates the
root module and a list indicates a submodule whose path relative to
the root module is given by the @racket[cdr] of the list. The list
starts with @racket[#f] when a submodule should be loaded @emph{only}
if it can be loaded independently (i.e., from compiled form---never
from source); if the submodule cannot be loaded independently, the
load handler should return without loading from a file. When the
expected module name is a list that starts with a symbol, the root
module and any other submodules can be loaded from the given file,
which might be from source, and the load handler still should not
complain if the expected submodule is not found.
When loading modules from a nonexistent source file, the load handler
may raise an exception regardless of whether submodules are
requested or not.
 
The default load handler reads forms from the file in
@racket[read-syntax] mode with line-counting enabled for the file
port, unless the path has a @racket[".zo"] suffix. It also
@racket[parameterize]s each read to set @racket[read-accept-compiled],
@racket[read-accept-reader], and @racket[read-accept-lang] to
@racket[#t]. In addition, if @racket[load-on-demand-enabled] is
@racket[#t], then @racket[read-on-demand-source] is set to
the @tech{cleanse}d, absolute form of @racket[path] during the
@racket[read-syntax] call. After reading a single form, the form is
passed to the current @tech{evaluation handler}, wrapping the
evaluation in a continuation prompt (see
@racket[call-with-continuation-prompt]) for the default continuation
prompt tag with handler that propagates the abort to the continuation
of the @racket[load] call.

If the second argument to the load handler is a symbol, then:

@itemize[

 @item{The @racket[read-syntax] from the file is additionally
       @racket[parameterize]d as follows (to provide consistent reading
       of module source):

       @racketblock[
       (current-readtable #f)
       (read-case-sensitive #t)
       (read-square-bracket-as-paren #t)
       (read-curly-brace-as-paren #t)
       (read-accept-box #t)
       (read-accept-compiled #t)
       (read-accept-bar-quote #t)
       (read-accept-graph #t)
       (read-syntax-accept-graph #f)
       (read-decimal-as-inexact #t)
       (read-accept-dot #t)
       (read-accept-infix-dot #t)
       (read-accept-quasiquote #t)
       (read-accept-reader #t)
       (read-accept-lang #t)
       ]}

 @item{If the read result is not a @racketidfont{module} form, or if a
       second @racket[read-syntax] does not produce an end-of-file,
       then the @exnraise[exn:fail] without evaluating the form that
       was read from the file. (In previous versions, the module
       declaration was checked to match the name given as the second
       argument to the load handler, but this check is no longer
       performed.)}

 @item{The @tech{lexical information} of the initial
       @racketidfont{module} identifier is enriched with a binding for
       @racket[module], so that the form corresponds to a module
       declaration independent of the current namespace's bindings.}

]

If the second argument to the load handler is @racket[#f], then each
expression read from the file is wrapped with
@racketidfont{#%top-interaction}, which is normally bound to
@racket[#%top-interaction], before passing it to the @tech{evaluation
handler}.

The return value from the default @tech{load handler} is the value of
the last form from the loaded file, or @|void-const| if the file
contains no forms. If the given path is a relative path, then it is
resolved using the value of @racket[current-directory].}


@defproc[(load [file path-string?]) any]{

@guidealso["namespaces"]

Calls the current @tech{load handler} in tail position. The call is
@racket[parameterized] to set @racket[current-load-relative-directory]
to the directory of @racket[file], which is resolved relative to
the value of @racket[current-directory].}


@defproc[(load-relative [file path-string?]) any]{

Like @racket[load/use-compiled], but when @racket[file] is a relative
path, it is resolved using the value of
@racket[current-load-relative-directory] instead of the value of
@racket[current-directory] if the former is not @racket[#f], otherwise
@racket[current-directory] is used.}


@defproc[(load/cd [file path-string?]) any]{

Like @racket[load], but @racket[load/cd] sets both
@racket[current-directory] and
@racket[current-load-relative-directory] before calling the @tech{load
handler}.}


@defparam[current-load-extension proc (path? (or/c #f
                                                   symbol?
                                                   (cons/c (or/c #f symbol?)
                                                           (non-empty-listof symbol?)))
                                       . -> . 
                                       any)]{

A @tech{parameter} that determines a @deftech{extension-load handler}, which is
called by @racket[load-extension] and the default @tech{compiled-load
handler}.

An @tech{extension-load handler} takes the same arguments as a
@tech{load handler}, but the file should be a platform-specific
@deftech{dynamic extension}, typically with the file suffix
@filepath{.so} (Unix), @filepath{.dll} (Windows), or @filepath{.dylib}
(Mac OS).  The file is loaded using internal, OS-specific
primitives. See @other-manual['(lib
"scribblings/inside/inside.scrbl")] for more information on
@tech{dynamic extensions}.

Extensions are supported only when @racket[(system-type 'vm)] returns
@racket['racket].}


@defproc[(load-extension [file path-string?]) any]{

Sets @racket[current-load-relative-directory] like @racket[load], and
calls the @tech{extension-load handler} in tail position.

Extensions are supported only when @racket[(system-type 'vm)] returns
@racket['racket].}


@defproc[(load-relative-extension [file path-string?]) any]{

Like @racket[load-extension], but resolves @racket[file] using
@racket[current-load-relative-directory] like @racket[load-relative].

Extensions are supported only when @racket[(system-type 'vm)] returns
@racket['racket].}


@defparam[current-load/use-compiled proc (path? (or/c #f
                                                      symbol?
                                                      (cons/c (or/c #f symbol?)
                                                              (non-empty-listof symbol?)))
                                                . -> . any)]{

A @tech{parameter} that determines the current @deftech{compiled-load
handler} to load from a file that may have a compiled form. The
@tech{compiled-load handler} is called by @racket[load/use-compiled].

The protocol for a @tech{compiled-load handler} is the same as for the
@tech{load handler} (see @racket[current-load]), except that a
@tech{compiled-load handler} is expected to set
@racket[current-load-relative-directory] itself. Additionally, the default
@tech{compiled-load handler} does the following:

@itemize[

 @item{When the given path ends with @filepath{.rkt}, no @filepath{.rkt}
   file exists, and when the handler's second argument is not @racket[#f],
   the default @tech{compiled-load handler} checks for a @filepath{.ss} file.}

 @item{The default @tech{compiled-load handler} checks for the opportunity
   to load from @filepath{.zo} (bytecode) files and, when @racket[(system-type 'vm)]
   returns @racket['racket], for @filepath{.so} (native Unix), @filepath{.dll} (native Windows),
   or @filepath{.dylib} (native Mac OS) files.}

 @item{When the default @tech{compiled-load handler} needs to load from
  the given path, the given path does not exist, and when the handler's
  second argument is not @racket[#f],
  the default @tech{compiled-load handler} returns without
  raising an exception.}

 ]

The check for a compiled file occurs whenever the given path
@racket[_file] ends with any extension (e.g., @filepath{.rkt} or
@filepath{.scrbl}), and the check consults the subdirectories
indicated by the @racket[current-compiled-file-roots] and
@racket[use-compiled-file-paths] parameters relative to
@racket[_file], where the former supplies ``roots'' for compiled files
and the latter provides subdirectories.
@margin-note*{See also @racketmodname[compiler/compilation-path].}
A ``root'' can be an absolute
path, in which case @racket[_file]'s directory is combined with
@racket[reroot-path] and the root as the second argument; if the
``root'' is a relative path, then the relative path is instead
suffixed onto the directory of @racket[_file]. The roots are tried in
order, and the subdirectories are checked in order within each root. A
@filepath{.zo} version of the file (whose name is formed by passing
@racket[_file] and @racket[#".zo"] to @racket[path-add-extension]) is
loaded if it exists directly in one of the indicated subdirectories,
or when @racket[(system-type 'vm)] returns
@racket['racket], then a @filepath{.so}/@filepath{.dll}/@filepath{.dylib} version of the
file is loaded if it exists within a @filepath{native} subdirectory of
a @racket[use-compiled-file-paths] directory, in an even deeper
subdirectory as named by @racket[system-library-subpath]. A compiled
file is loaded only if it checks out according to
@racket[(use-compiled-file-check)]; with the default parameter value
of @racket['modify-seconds], a compiled file is used only if its
modification date is not older than the
date for @racket[_file]. If both @filepath{.zo} and
@filepath{.so}/@filepath{.dll}/@filepath{.dylib} files are available
when @racket[(system-type 'vm)] returns @racket['racket],
the @filepath{.so}/@filepath{.dll}/@filepath{.dylib} file is used.  If
@racket[_file] ends with @filepath{.rkt}, no such file exists, the
handler's second argument is a symbol, and a @filepath{.ss} file
exists, then @filepath{.zo} and
@filepath{.so}/@filepath{.dll}/@filepath{.dylib} files are used only
with names based on @racket[_file] with its suffixed replaced by
@filepath{.ss}.

While a @filepath{.zo}, @filepath{.so}, @filepath{.dll}, or
@filepath{.dylib} file is loaded, the current @racket[load-relative]
directory is set to the directory of the original @racket[_file].  If
the file to be loaded has the suffix @filepath{.ss} while the
requested file has the suffix @filepath{.rkt}, then the
@racket[current-module-declare-source] parameter is set to the full
path of the loaded file, otherwise the
@racket[current-module-declare-source] parameter is set to
@racket[#f].

If the original @racket[_file] is loaded or a @filepath{.zo} variant is
loaded, the @tech{load handler} is called to load the file. If any
other kind of file is loaded, the @tech{extension-load handler} is
called.

When the default @tech{compiled-load handler} loads a module from a
bytecode (i.e., @filepath{.zo}) file, the handler records the bytecode
file path in the current namespace's @tech{module registry}. More
specifically, the handler records the path for the top-level module of
the loaded module, which is an enclosing module if the loaded module
is a submodule. Thereafter, loads via the default @tech{compiled-load
handler} for modules within the same top-level module use the recorded
file, independent of the file that otherwise would be selected by the
@tech{compiled-load handler} (e.g., even if the
@racket[use-compiled-file-paths] parameter value changes). The default
@tech{module name resolver} transfers bytecode-file information when a
module declaration is attached to a new namespace. This protocol supports
independent but consistent loading of submodules from bytecode files.}


@defproc[(load/use-compiled [file path-string?]) any]{

Calls the current @tech{compiled-load handler} in tail position.}


@defparam[current-load-relative-directory path 
          (or/c (and/c path-string? complete-path?) #f)]{

A @tech{parameter} that is set by @racket[load], @racket[load-relative],
@racket[load-extension], @racket[load-relative-extension], and the
default @tech{compiled-load handler}, and used by
@racket[load-relative], @racket[load-relative-extension], and the
default @tech{compiled-load handler}.

When a new path or string is provided as the parameter's value, it is
immediately expanded (see @secref["pathutils"]) and converted to a
path. (The directory need not exist.)}


@defparam*[use-compiled-file-paths paths (listof (and/c path-string? relative-path?))
                                         (listof (and/c path? relative-path?))]{

A list of relative paths, which defaults to @racket[(list
(string->path "compiled"))]. It is used by the @tech{compiled-load
handler} (see @racket[current-load/use-compiled]).

If the @envvar-indexed{PLT_ZO_PATH} environment variable is set on
startup, it supplies a path instead of @racket["compiled"] to
use for the initial parameter value.

@history[#:changed "7.7.0.9" @elem{Added @envvar{PLT_ZO_PATH}.}]}


@defparam*[current-compiled-file-roots paths (listof (or/c path-string? 'same)) (listof (or/c path? 'same))]{

A list of paths and @racket['same]s that is used by the default
@tech{compiled-load handler} (see @racket[current-load/use-compiled]).

The parameter is normally initialized to @racket[(list 'same)], but
the parameter's initial value can be adjusted by the installation
configuration as reported by @racket[(find-compiled-file-roots)],
and it can be further adjusted by the
@as-index{@envvar{PLTCOMPILEDROOTS}} environment variable or the
@DFlag{compiled} or @Flag{R} command-line flag for @exec{racket}.  If
the environment variable is defined and not overridden by a
command-line flag, it is parsed by first replacing any
@litchar["@(version)"] with the result of @racket[(version)], then using
@racket[path-list-string->path-list] with a path list produced by
@racket[(find-compiled-file-roots)] to arrive at the parameter's
initial value.}


@defproc[(find-compiled-file-roots) 
         (listof (or/c path? 'same))]{

Produces a list of paths and @racket['same], which is normally used to
initialize @racket[current-compiled-file-roots]. The list is
determined by consulting the @filepath{config.rtkd} file in the
directory reported by @racket[(find-config-dir)], and it defaults to
@racket[(list 'same)] if not configured there.

See also @racket['compiled-file-roots] in @secref[#:doc raco-doc
"config-file"].

@history[#:added "8.0.0.9"]}


@defparam[use-compiled-file-check check (or/c 'modify-seconds 'exists)]{

A @tech{parameter} that determines how a compiled file is checked
against its source to enable use of the compiled file. By default, the
file-check mode is @racket['modify-seconds], which uses a compiled
file when its filesystem modification date is at least as new as the
source file's. The @racket['exists] mode causes a compiled file to be
used in place of its source as long as the compiled file exists.

If the @indexed-envvar{PLT_COMPILED_FILE_CHECK} environment variable is
set to @litchar{modify-seconds} or @litchar{exists}, then the
environment variable's value configures the parameter when Racket
starts.

@history[#:added "6.6.0.3"]}


@defproc[(read-eval-print-loop) any]{

Starts a new @deftech{REPL} using the current input, output, and error
ports. The REPL wraps each expression to evaluate with
@racketidfont{#%top-interaction}, which is normally bound to
@racket[#%top-interaction], and it wraps each evaluation with a
continuation prompt using the default continuation prompt tag and
prompt handler (see @racket[call-with-continuation-prompt]). The REPL
also wraps the read and print operations with a prompt for the default
tag whose handler ignores abort arguments and continues the loop. The
@racket[read-eval-print-loop] procedure does not return until
@racket[eof] is read, at which point it returns @|void-const|.

The @racket[read-eval-print-loop] procedure can be configured through
the @racket[current-prompt-read], @racket[current-eval], and
@racket[current-print] parameters.}


@defparam[current-prompt-read proc (-> any)]{

A @tech{parameter} that determines a @deftech{prompt read handler}, which is
a procedure that takes no arguments, displays a prompt string, and
returns a top-level form to evaluate. The prompt read handler is
called by @racket[read-eval-print-loop], and after printing a prompt,
the handler typically should call the @tech{read interaction handler}
(as determined by the @racket[current-read-interaction] parameter)
with the port produced by the @tech{interaction port handler}
(as determined by the @racket[current-get-interaction-input-port] parameter).

The default prompt read handler prints @litchar{> } and returns the
result of

@racketblock[
(let ([in ((current-get-interaction-input-port))])
  ((current-read-interaction) (object-name in) in))
]

If the input and output ports are both terminals (in the sense of
@racket[terminal-port?]) and if the output port appears to be counting
lines (because @racket[port-next-location] returns a non-@racket[#f]
line and column), then the output port's line is incremented and its
column is reset to @racket[0] via @racket[set-port-next-location!] 
before returning the read result.}


@defparam[current-get-interaction-input-port proc (-> input-port?)]{

A @tech{parameter} that determines the @deftech{interaction port handler},
which returns a port to use for @racket[read-eval-print-loop] inputs.

The default interaction port handler returns the current input port.
In addition, if that port is the initial current input port,
the initial current output and error ports are flushed.

The @racketmodname[racket/gui/base] library adjusts this parameter's
value by extending the current value. The extension wraps the result
port so that GUI events can be handled when reading from the port
blocks.}


@defparam[current-get-interaction-evt proc (-> evt?)]{

A @tech{parameter} that determines the @deftech{interaction event
handler}, which returns an @tech{synchronizable event} that should be
used in combination with blocking that is similar to
@racket[read-eval-print-loop] waiting for input---but where an input
port is not read directly, so
@racket[current-get-interaction-input-port] does not apply.

When the interaction event handler returns an event that becomes
ready, and when the event's ready value is a procedure, then the
procedure is meant to be called with zero arguments blocking resumes.
The default interaction event handler returns @racket[never-evt].

The @racketmodname[racket/gui/base] library adjusts this parameter's
value by extending the current value. The extension combines the
current value's result with @racket[choice-evt] and an event that
becomes ready when a GUI event is available, and the event's value is
a procedure that yields to one or more available GUI events.

@history[#:added "8.3.0.3"]}


@defparam[current-read-interaction proc (any/c input-port? . -> . any)]{

A @tech{parameter} that determines the current @deftech{read interaction
handler}, which is procedure that takes an arbitrary value and an
input port and returns an expression read from the input port. 

The default read interaction handler accepts @racket[_src] and
@racket[_in] and returns

@racketblock[
(parameterize ([read-accept-reader #t]
               [read-accept-lang #f])
  (read-syntax _src _in))
]}


@defparam[current-print proc (any/c . -> . any)]{

A @tech{parameter} that determines the @deftech{print handler} that is called
 by @racket[read-eval-print-loop] to print the result of an evaluation
 (and the result is ignored).

The default @tech{print handler} @racket[print]s the value to the
 current output port (as determined by the
 @racket[current-output-port] parameter) and then outputs a newline,
 except that it prints nothing when the value is @|void-const|.}


@defparam[current-compile proc (any/c boolean? . -> . compiled-expression?)]{

A @tech{parameter} that determines the current @deftech{compilation handler}.
The @tech{compilation handler} is a procedure that takes a top-level form and
returns a compiled form; see @secref["compilation-model"] for
more information on compilation.

The @tech{compilation handler} is called by @racket[compile], and
indirectly by the default @tech{evaluation handler} and the default
@tech{load handler}.

The handler's second argument is @racket[#t] if the compiled form will
be used only for immediate evaluation, or @racket[#f] if the compiled
form may be saved for later use; the default compilation handler is
optimized for the special case of immediate evaluation.

When a compiled form is written to an output port, the written form
starts with @litchar{#~}. See @secref["print-compiled"] for more
information.

For internal testing purposes, when the
@as-index{@envvar{PLT_VALIDATE_COMPILE}} environment variable is set,
the default compilation handler runs a bytecode validator immediately
on its own compilation results (instead of relying only on validation
when compiled bytecode is loaded).

The @racket[current-compile] binding is provided as @tech{protected}
in the sense of @racket[protect-out].

@history[#:changed "8.2.0.4" @elem{Changed binding to @tech{protected}.}]}


@defproc[(compile [top-level-form any/c]) compiled-expression?]{

Like @racket[eval], but calls the current @tech{compilation handler} in
tail position with @racket[top-level-form].}


@defproc[(compile-syntax [stx syntax?]) compiled-expression?]{

Like @racket[eval-syntax], but calls the current @tech{compilation
handler} in tail position with @racket[stx].}


@defproc[(compiled-expression-recompile [ce compiled-expression?]) compiled-expression?]{

Recompiles @racket[ce]. If @racket[ce] was compiled as
machine-independent and @racket[current-compile-target-machine] is
not set to @racket[#f], then recompiling effectively converts to the current
machine format. Otherwise, recompiling effectively re-runs
optimization passes to produce an equivalent compiled form with
potentially different performance characteristics.

@history[#:added "6.3"]}

@defproc[(compiled-expression-add-target-machine [ce compiled-expression?]
                                                 [other-ce (or/c compiled-expression? hash?)])
         compiled-expression?]{

Returns a compiled expression like @racket[ce], but augments or
replaces cross-compilation information in @racket[ce] with information
from @racket[other-ce]. The intent is that @racket[ce] and
@racket[other-ce] have been compiled with different values for
@racket[current-compile-target-machine], and @racket[ce] will be used
to run a module on the compiling machine, while information from
@racket[other-ce] is needed for cross-compiling imports of the module.

The @racket[other-ce] argument can be a compiled module or a summary of a module's
information as produced by @racket[compiled-expression-summarize-target-machine].

@history[#:added "8.12.0.3"
         #:changed "8.17.0.3" @elem{Added support for @racket[other-ce]
                                    as a summary.}]}


@defproc[(compiled-expression-summarize-target-machine [other-ce compiled-expression?])
         hash?]{

Returns a value that has the same information as @racket[other-ce]
for @racket[compiled-expression-add-target-machine], but in a form that can be
portably serialized via @racketmodname[racket/fasl].

@history[#:added "8.17.0.3"]}


@defproc[(compiled-expression? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a compiled form, @racket[#f]
otherwise.}


@defboolparam[compile-enforce-module-constants on?]{

A @tech{parameter} that determines how a module declaration is compiled. 

When constants are enforced, and when the macro-expanded body of a
module contains no @racket[set!] assignment to a particular variable
defined within the module, then the variable is marked as constant
when the definition is evaluated. Afterward, the variable's value
cannot be assigned or undefined through @racket[module->namespace],
and it cannot be defined by redeclaring the module.

Enforcing constants allows the compiler to inline some variable
values, and it allows the native-code just-in-time compiler to
generate code that skips certain run-time checks.}


@defboolparam[compile-allow-set!-undefined allow?]{

A @tech{parameter} that determines how a @racket[set!] expression is compiled
when it mutates a global variable. If the value of this parameter is a
true value, @racket[set!] expressions for global variables are
compiled so that the global variable is set even if it was not
previously defined.  Otherwise, @racket[set!] expressions for global
variables are compiled to raise the
@racket[exn:fail:contract:variable] exception if the global variable
is not defined at the time the @racket[set!] is performed.  Note that
this parameter is used when an expression is @italic{compiled}, not
when it is @italic{evaluated}.}

@defboolparam[compile-context-preservation-enabled on?]{

A @tech{parameter} that determines whether compilation should avoid
function-call inlining and other optimizations that may cause
information to be lost from stack traces (as reported by
@racket[continuation-mark-set->context]). The default is @racket[#f],
which allows such optimizations.}

@defparam[current-compile-target-machine target (or/c #f (and/c symbol? compile-target-machine?))]{

A @tech{parameter} that determines the platform and/or virtual machine
target for a newly compiled expression.

If the target is @racket[#f], the the compiled expression writes in a
machine-independent format (usually in @filepath{.zo} files).
Machine-independent compiled code works for any platform and any
Racket virtual machine. When the machine-independent compiled
expression is read back in, it is subject to further compilation for
the current platform and virtual machine, which can be considerably
slower than reading a format that is fully compiled for a platform and
virtual machine.

The default is something other than @racket[#f], unless
machine-independent mode is enabled through the
@Flag{M}/@DFlag{compile-any} command-line flag to stand-alone Racket
(or GRacket) or through the @as-index{@envvar{PLT_COMPILE_ANY}}
environment variable (set to any value).

@history[#:added "7.1.0.6"]}


@defproc[(compile-target-machine? [sym symbol?]) boolean?]{

Reports whether @racket[sym] is a supported compilation target for the
currently running Racket.

When @racket[(system-type 'vm)] reports @racket['racket], then the
only target symbol is @racket['racket]. When @racket[(system-type
'vm)] reports @racket['chez-scheme], then a symbol corresponding to
the current platform is a target, and other targets may also be
supported. The @racket['target-machine] mode of @racket[system-type]
reports the running Racket's native target machine.

@history[#:added "7.1.0.6"]}


@defparam[current-compile-realm realm symbol?]{

Determines the @tech{realm} that is assigned to modules and procedures
when they are compiled.

@history[#:added "8.4.0.2"]}


@defboolparam[eval-jit-enabled on?]{

@guidealso["JIT"]

A @tech{parameter} that determines whether the native-code just-in-time
compiler (@deftech{JIT}) is enabled for code (compiled or not) that is passed to
the default evaluation handler. A true parameter value is effective
only on platforms for which the JIT is supported and for Racket virtual machines
that rely on a JIT.

The default is @racket[#t], unless the JIT is not supported by the
current platform but is supported on the same virtual machine for other
platforms, unless it is disabled through the
@Flag{j}/@DFlag{no-jit} command-line flag to stand-alone Racket (or
GRacket), and unless it is disabled through the
@as-index{@envvar{PLTNOMZJIT}} environment variable (set to any
value).}

@defboolparam[load-on-demand-enabled on?]{

A @tech{parameter} that determines whether the default @tech{load handler}
sets @racket[read-on-demand-source]. See @racket[current-load] for
more information. The default is @racket[#t], unless it is disabled
through the @Flag{d}/@DFlag{no-delay} command-line flag.}
