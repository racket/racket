#lang scribble/doc
@(require "mz.rkt" (for-label racket/trace)
          scribble/eval)

@(begin (define ev (make-base-eval))
        (ev '(require racket/trace)))

@title{Tracing}

@note-lib-only[racket/trace]

The @racketmodname[racket/trace] library mimics the tracing facility
available in Chez Scheme.

@defform[(trace id ...)]{

Each @racket[id] must be bound to a procedure in the environment of
the @racket[trace] expression.  Each @racket[id] is @racket[set!]ed to
a new procedure that traces procedure calls and returns by printing
the arguments and results of the call via
@racket[current-trace-notify].  If multiple values are returned, each
value is displayed starting on a separate line.

When traced procedures invoke each other, nested invocations are shown
by printing a nesting prefix. If the nesting depth grows to ten and
beyond, a number is printed to show the actual nesting depth.

The @racket[trace] form can be used on an identifier that is already
traced.  In this case, assuming that the variable's value has not been
changed, @racket[trace] has no effect.  If the variable has been
changed to a different procedure, then a new trace is installed.

Tracing respects tail calls to preserve loops, but its effect may be
visible through continuation marks. When a call to a traced procedure
occurs in tail position with respect to a previous traced call, then
the tailness of the call is preserved (and the result of the call is
not printed for the tail call, because the same result will be printed
for an enclosing call). Otherwise, however, the body of a traced
procedure is not evaluated in tail position with respect to a call to
the procedure.

The result of a @racket[trace] expression is @|void-const|.

@examples[#:eval ev
(define (f x) (if (zero? x) 0 (add1 (f (sub1 x)))))
(trace f)
(f 10)
]

}

@defform[(untrace id ...)]{

Undoes the effects of the @racket[trace] form for each @racket[id],
@racket[set!]ing each @racket[id] back to the untraced procedure, but
only if the current value of @racket[id] is a traced procedure.  If
the current value of a @racket[id] is not a procedure installed by
@racket[trace], then the variable is not changed.

The result of an @racket[untrace] expression is @|void-const|.}


@defparam[current-trace-notify proc (string? . -> . any)]{

A @tech{parameter} that determines the way that trace output is
displayed. The string given to @racket[proc] is a trace; it does not
end with a newline, but it may contain internal newlines. Each call or
result is converted into a string using @racket[pretty-print].  The
parameter's default value prints the given string followed by a newline to
@racket[(current-output-port)].}

@defproc[(trace-call [id symbol?] [proc procedure?]
                     [#:<kw> kw-arg any/c] ...) any/c]{

Calls @racket[proc] with the arguments supplied in
@racket[args], and possibly using keyword arguments. Also prints out the
trace information during the call, as described above in the docs for
@racket[trace], using @racket[id] as the name of @racket[proc].

}

@defparam[current-trace-print-args trace-print-args 
          (-> symbol?
              list?
              (listof keyword?)
              list?
              number?
              void?)]{

The value of this parameter is invoked to print out the arguments of a
traced call. It receives the name of the function, the function's
ordinary arguments, its keywords, the values of the keywords, and a
number indicating the depth of the call.

}

@defparam[current-trace-print-results trace-print-results 
          (-> symbol?
              list?
              number?
              any)]{

The value of this parameter is invoked to print out the results of a
traced call. It receives the name of the function, the function's
results, and a number indicating the depth of the call.

}

@defparam[current-prefix-in prefix string?]{
  This string is used by the default value of @racket[current-trace-print-args]
  indicating that the current line is showing the a call to a
  traced function.
  
  It defaults to @racket[">"].
}


@defparam[current-prefix-out prefix string?]{
  This string is used by the default value of @racket[current-trace-print-results]
  indicating that the current line is showing the result
  of a traced call.
  
  It defaults to @racket["<"].
}


@close-eval[ev]
