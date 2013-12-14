#lang scribble/doc
@(require "mz.rkt" (for-label racket/trace)
          scribble/eval)

@(begin (define ev (make-base-eval))
        (ev '(require racket/trace))
        (ev '(require (for-syntax racket/base))))

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

@defform*[((trace-define id expr)
           (trace-define (head args) body ...+))]{

The @racket[trace-define] form is short-hand for first defining a
function then tracing it. This form supports all @racket[define] forms.

@examples[#:eval ev
(trace-define (f x) (if (zero? x) 0 (add1 (f (sub1 x)))))
(f 5)
]

@examples[#:eval ev
(trace-define ((+n n) x) (+ n x))
(map (+n 5) (list 1 3 4))
]}

@defform*[((trace-define-syntax id expr)
           (trace-define-syntax (head args) body ...+))]{

The @racket[trace-define-syntax] form is short-hand for first defining a
macro then tracing it. This form supports all @racket[define-syntax] forms.

For example:

@examples[#:eval ev
(trace-define-syntax fact
  (syntax-rules ()
    [(_ x) 120]))
(fact 5)
]

By default, @racket[trace] prints out syntax objects when tracing a
macro. This can result in too much output if you do not need to see,
e.g., source information. To get more readable output, try this:

@examples[#:eval ev
  (require (for-syntax racket/trace))
  (begin-for-syntax
    (current-trace-print-args
      (let ([ctpa (current-trace-print-args)])
        (lambda (s l kw l2 n)
          (ctpa s (map syntax->datum l) kw l2 n))))
    (current-trace-print-results
      (let ([ctpr (current-trace-print-results)])
        (lambda (s l n)
         (ctpr s (map syntax->datum l) n)))))

  (trace-define-syntax fact
  (syntax-rules ()
    [(_ x) #'120]))
  (fact 5)]}

@defform[(trace-lambda [#:name id] args expr)]{

The @racket[trace-lambda] form enables tracing an anonymous function. This
form will attempt to infer a name using
@racket[syntax-local-infer-name], or a name can be specified using the
optional @racket[#:name] argument.  A syntax error is raised if a name
is not given and a name cannot be inferred.

@examples[#:eval ev
  ((trace-lambda (x) 120) 5)]}

@defform[(trace-let id ([arg expr] ...+) body ...+)]{

The @racket[trace-let] form enables tracing a named let.

@examples[#:eval ev
  (trace-let f ([x 5])
    (if (zero? x)
        1
        (* x (f (sub1 x)))))]}

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
