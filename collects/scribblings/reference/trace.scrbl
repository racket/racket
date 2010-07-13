#lang scribble/doc
@(require "mz.rkt" (for-label racket/trace)
	  scribble/eval)

@(begin (define ev (make-base-eval))
	(ev '(require racket/trace)))

@title["Tracing"]

@note-lib-only[racket/trace]

The @schememodname[racket/trace] library mimics the tracing facility
available in Chez Scheme.

@defform[(trace id ...)]{

Each @scheme[id] must be bound to a procedure in the environment of
the @scheme[trace] expression.  Each @scheme[id] is @scheme[set!]ed to
a new procedure that traces procedure calls and returns by printing
the arguments and results of the call via
@scheme[current-trace-notify].  If multiple values are returned, each
value is displayed starting on a separate line.

When traced procedures invoke each other, nested invocations are shown
by printing a nesting prefix. If the nesting depth grows to ten and
beyond, a number is printed to show the actual nesting depth.

The @scheme[trace] form can be used on an identifier that is already
traced.  In this case, assuming that the variable's value has not been
changed, @scheme[trace] has no effect.  If the variable has been
changed to a different procedure, then a new trace is installed.

Tracing respects tail calls to preserve loops, but its effect may be
visible through continuation marks. When a call to a traced procedure
occurs in tail position with respect to a previous traced call, then
the tailness of the call is preserved (and the result of the call is
not printed for the tail call, because the same result will be printed
for an enclosing call). Otherwise, however, the body of a traced
procedure is not evaluated in tail position with respect to a call to
the procedure.

The result of a @scheme[trace] expression is @|void-const|.

@examples[#:eval ev
(define (f x) (if (zero? x) 0 (add1 (f (sub1 x)))))
(trace f)
(f 10)
]

}

@defform[(untrace id ...)]{

Undoes the effects of the @scheme[trace] form for each @scheme[id],
@scheme[set!]ing each @scheme[id] back to the untraced procedure, but
only if the current value of @scheme[id] is a traced procedure.  If
the current value of a @scheme[id] is not a procedure installed by
@scheme[trace], then the variable is not changed.

The result of an @scheme[untrace] expression is @|void-const|.}


@defparam[current-trace-notify proc (string? . -> . any)]{

A parameter that determines the way that trace output is
displayed. The string given to @scheme[proc] is a trace; it does not
end with a newline, but it may contain internal newlines. Each call or
result is converted into a string using @scheme[pretty-print].  The
parameter's default value prints the given string followed by a newline to
@scheme[(current-output-port)].}

@defproc[(trace-call [id symbol?] [proc procedure?]
                     [#:<kw> kw-arg any/c] ...) any/c]{

Calls @scheme[proc] with the arguments supplied in
@scheme[args], and possibly using keyword arguments. Also prints out the
trace information during the call, as described above in the docs for
@scheme[trace], using @scheme[id] as the name of @scheme[proc].

}

@defparam[current-trace-print-args trace-print-args 
	  (-> symbol?
              (listof keyword?)
              list? 
              list?
              number?)]{

The value of this parameter is invoked to print out the arguments of a
traced call. It receives the name of the function, the function's
ordinary arguments, its keywords, the values of the keywords, and a
number indicating the depth of the call.

}
