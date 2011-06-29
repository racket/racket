#lang scribble/doc

@(require scribble/manual
          (for-label scheme
                     trace))

@title[#:tag "top"]{Trace: Instrumentation to Show Function Calls}

@bold{Calltrace} is a tool that displays all calls to user procedures.
It displays the arguments to the calls, and it indents to show the
depth of the continuation.

@; ----------------------------------------

@section[#:tag "quick-instructions"]{Quick Instructions}

@itemize[
         @item{Throw away @filepath{.zo} versions of your source}
         @item{Prefix your program with 
                      @racketblock[(require trace)]
               perhaps by starting @exec{racket} with
                  @commandline{racket -l trace ...}
               before arguments to load your program.}
         @item{Run your program}
         ]

The @racketmodname[trace] module is odd; don't import it
into another module. Instead, the @racketmodname[trace]
module is meant to be invoked from the top-level, so that it can
install an evaluation handler, exception handler, etc.

To reuse parts of the code of @racketmodname[trace], import
@racketmodname[trace/calltrace-lib]. It contains all of the bindings
described here, but it but does not set the @racket[current-eval]
parameter.

@; ----------------------------------------

@section{Installing Calltrace}

@defmodule[trace]{Invoking the
@racketmodname[trace] module sets the evaluation handler
(via @racket[current-eval]) to instrument Racket source code.}

 NOTE: @racketmodname[trace] has no effect on code loaded as
 compiled byte code (i.e., from a @filepath{.zo} file) or native code
 (i.e., from a @filepath{.dll}, @filepath{.so}, or @filepath{.dylib}
 file).

Calltrace's instrumentation can be explicitly disabled via the
@racket[instrumenting-enabled] parameter. Instrumentation is on by
default. The @racket[instrumenting-enabled] parameter affects only the
way that source code is compiled, not the way that exception
information is reported.

Do not load @racketmodname[trace] before writing
@filepath{.zo} files.  Calltrace instruments S-expressions with
unprintable values; this works fine if the instrumented S-expression
is passed to the default eval handler, but neither the S-expression
nor its byte-code form can be marshalled to a string.

@; ----------------------------------------

@section{Calltrace Library}

@defmodule[trace/calltrace-lib]{The
@racketmodname[trace/calltrace-lib] module provides functions that
implement @racket[trace].}

@defboolparam[instrumenting-enabled on?]{

A parameter that determines whether tracing instrumentation is
enabled.}

@defproc[(calltrace-eval-handler [e any/c]) any]{

A procedure suitable for use with @racket[current-eval], which
instruments expressions for Calltrace output (when instrumentation is
not disabled via @racket[instrumenting-enabled]).

Requiring @racket[trace] installs this procedure as the
value for @racket[current-eval].}


@defproc[(annotate [e any/c]) syntax?]{

Instruments the expression @racket[e] with Calltrace support. This
function is normally used by @racket[calltrace-eval-handler].}
