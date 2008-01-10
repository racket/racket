#lang scribble/doc

@require[scribble/manual
         (for-label scheme
                    trace/calltrace-lib)]

@title[#:tag "top"]{@bold{Calltrace}}

@bold{Calltrace} is a tool that displays all calls to user procedures.
It displays the arguments to the calls, and it indents to show the
depth of the continuation.

@; ----------------------------------------

@section[#:tag "quick-instructions"]{Quick Instructions}

@itemize{
         @item{Throw away @filepath{.zo} versions of your source}
         @item{Prefix your program with 
                      @schemeblock[(require trace/calltrace)]
               perhaps by starting @exec{mzscheme} with
                  @commandline{mzscheme -l trace/calltrace}
               before arguments to load your program.}
         @item{Run your program}
         }

The @schememodname[trace/calltrace] module is odd; don't import it
into another module. Instead, the @schememodname[trace/calltrace]
module is meant to be invoked from the top-level, so that it can
install an evaluation handler, exception handler, etc.

To reuse parts of the code of @schememodname[trace/calltrace], import
@schememodname[trace/calltrace-lib]. It contains all of the bindings
described here, but it but does not set the @scheme[current-eval]
parameter.

@; ----------------------------------------

@section{Installing Calltrace}

@defmodule[trace/calltrace]{Invoking the
@schememodname[trace/calltrace] module sets the evaluation handler
(via @scheme[current-eval]) to instrument Scheme source code.}

 NOTE: @schememodname[trace/calltrace] has no effect on code loaded as
 compiled byte code (i.e., from a @filepath{.zo} file) or native code
 (i.e., from a @filepath{.dll}, @filepath{.so}, or @filepath{.dylib}
 file).

Calltrace's instrumentation can be explicitly disabled via the
@scheme[instrumenting-enabled] parameter. Instrumentation is on by
default. The @scheme[instrumenting-enabled] parameter affects only the
way that source code is compiled, not the way that exception
information is reported.

Do not load @schememodname[trace/calltrace] before writing
@filepath{.zo} files.  Calltrace instruments S-expressions with
unprintable values; this works fine if the instrumented S-expression
is passed to the default eval handler, but neither the S-expression
nor its byte-code form can be marshalled to a string.

@; ----------------------------------------

@section{Calltrace Library}

@defmodule[trace/calltrace-lib]{The
@schememodname[trace/calltrace-lib] module provides functions that
implement @scheme[trace/calltrace].}

@defboolparam[instrumenting-enabled on?]{

A parameter that determines whether tracing instrumentation is
enabled.}

@defproc[(calltrace-eval-handler [e any/c]) any]{

A procedure suitable for use with @scheme[current-eval], which
instruments expressions for Calltrace output (when instrumentation is
not disabled via @scheme[instrumenting-enabled]).

Requiring @scheme[trace/calltrace] installs this procedure as the
value for @scheme[current-eval].}


@defproc[(annotate [e any/c]) syntax?]{

Instruments the expression @scheme[e] with Calltrace support. This
function is normally used by @scheme[calltrace-eval-handler].}
