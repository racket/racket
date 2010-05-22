#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/function))

@title[#:style 'quiet #:tag "cce-function"]{Functions}

@defmodule[unstable/cce/function]

This module provides tools for higher-order programming and creating functions.

@section{Simple Functions}

@defproc[(identity [x any/c]) (one-of/c x)]{

Returns @scheme[x].

}

@defform[(thunk body ...)]{

Creates a function that ignores its inputs and evaluates the given body.  Useful
for creating event handlers with no (or irrelevant) arguments.

@defexamples[
#:eval (evaluator 'unstable/cce/function)
(define f (thunk (define x 1) (printf "~a\n" x)))
(f)
(f 'x)
(f #:y 'z)
]

}

@defproc[(const [x any/c]) (unconstrained-domain-> (one-of/c x))]{

Produces a function that returns @scheme[x] regardless of input.

This function is reprovided from @schememodname[scheme/function].  In versions
of PLT Scheme before @scheme[const] was implemented, this module provides its
own definition.

@defexamples[
#:eval (evaluator 'unstable/cce/function)
(define f (const 5))
(f)
(f 'x)
(f #:y 'z)
]

}

@section{Higher Order Predicates}

@defproc[((negate [f (-> A ... boolean?)]) [x A] ...) boolean?]{

Negates the results of @scheme[f]; equivalent to @scheme[(not (f x ...))].

This function is reprovided from @schememodname[scheme/function].

@defexamples[
#:eval (evaluator 'unstable/cce/function)
(define f (negate exact-integer?))
(f 1)
(f 'one)
]

}

@defproc[((conjoin [f (-> A ... boolean?)] ...) [x A] ...) boolean?]{

Combines calls to each function with @scheme[and].  Equivalent to
@scheme[(and (f x ...) ...)]

@defexamples[
#:eval (evaluator 'unstable/cce/function)
(define f (conjoin exact? integer?))
(f 1)
(f 1.0)
(f 1/2)
(f 0.5)
]

}

@defproc[((disjoin [f (-> A ... boolean?)] ...) [x A] ...) boolean?]{

Combines calls to each function with @scheme[or].  Equivalent to
@scheme[(or (f x ...) ...)]

@defexamples[
#:eval (evaluator 'unstable/cce/function)
(define f (disjoin exact? integer?))
(f 1)
(f 1.0)
(f 1/2)
(f 0.5)
]

}

@section{Currying and (Partial) Application}

@defproc[(call [f (-> A ... B)] [x A] ...) B]{

Passes @scheme[x ...] to @scheme[f].  Keyword arguments are allowed.  Equivalent
to @scheme[(f x ...)].  Useful for application in higher-order contexts.

@defexamples[
#:eval (evaluator 'unstable/cce/function)
(map call
     (list + - * /)
     (list 1 2 3 4)
     (list 5 6 7 8))
(define count 0)
(define (inc)
  (set! count (+ count 1)))
(define (reset)
  (set! count 0))
(define (show)
  (printf "~a\n" count))
(for-each call (list inc inc show reset show))
]

}

@deftogether[(
@defproc[(papply [f (A ... B ... -> C)] [x A] ...) (B ... -> C)]
@defproc[(papplyr [f (A ... B ... -> C)] [x B] ...) (A ... -> C)]
)]{

The @scheme[papply] and @scheme[papplyr] functions partially apply @scheme[f] to
@scheme[x ...], which may include keyword arguments.  They obey the following
equations:

@schemeblock[
((papply f x ...) y ...) = (f x ... y ...)
((papplyr f x ...) y ...) = (f y ... x ...)
]

@defexamples[
#:eval (evaluator 'unstable/cce/function)
(define reciprocal (papply / 1))
(reciprocal 3)
(reciprocal 4)
(define halve (papplyr / 2))
(halve 3)
(halve 4)
]

}

@deftogether[(
@defproc[(curryn [n exact-nonnegative-integer?]
                 [f (A0 ... A1 ... ooo An ... -> B)]
                 [x A0] ...)
         (A1 ... -> ooo -> An ... -> B)]
@defproc[(currynr [n exact-nonnegative-integer?]
                  [f (A1 ... ooo An ... An+1 ... -> B)]
                  [x An+1] ...)
         (An ... -> ooo -> A1 ... -> B)]
)]{

@emph{Note:} The @scheme[ooo] above denotes a loosely associating ellipsis.

The @scheme[curryn] and @scheme[currynr] functions construct a curried version
of @scheme[f], specialized at @scheme[x ...], that produces a result after
@scheme[n] further applications.  Arguments at any stage of application may
include keyword arguments, so long as no keyword is duplicated.  These curried
functions obey the following equations:

@schemeblock[
(curryn 0 f x ...) = (f x ...)
((curryn (+ n 1) f x ...) y ...) = (curryn n f x ... y ...)

(currynr 0 f x ...) = (f x ...)
((currynr (+ n 1) f x ...) y ...) = (currynr n f y ... x ...)
]

The @scheme[call], @scheme[papply], and @scheme[papplyr] utilities are related
to @scheme[curryn] and @scheme[currynr] in the following manner:

@schemeblock[
(call f x ...) = (curryn 0 f x ...) = (currynr 0 f x ...)
(papply f x ...) = (curryn 1 f x ...)
(papplyr f x ...) = (currynr 1 f x ...)
]

@defexamples[
#:eval (evaluator 'unstable/cce/function)

(define reciprocal (curryn 1 / 1))
(reciprocal 3)
(reciprocal 4)

(define subtract-from (curryn 2 -))
(define from-10 (subtract-from 10))
(from-10 5)
(from-10 10)
(define from-0 (subtract-from 0))
(from-0 5)
(from-0 10)

(define halve (currynr 1 / 2))
(halve 3)
(halve 4)

(define subtract (currynr 2 -))
(define minus-10 (subtract 10))
(minus-10 5)
(minus-10 10)
(define minus-0 (subtract 0))
(minus-0 5)
(minus-0 10)

]

}

@section{Eta Expansion}

@defform[(eta f)]{

Produces a function equivalent to @scheme[f], except that @scheme[f] is
evaluated every time it is called.

This is useful for function expressions that may be run, but not called, before
@scheme[f] is defined.  The @scheme[eta] expression will produce a function
without evaluating @scheme[f].

@defexamples[
#:eval (evaluator 'unstable/cce/function)
(define f (eta g))
f
(define g (lambda (x) (+ x 1)))
(f 1)
]

}

@defform[(eta* f x ...)]{

Produces a function equivalent to @scheme[f], with argument list @scheme[x ...].
In simple cases, this is equivalent to @scheme[(lambda (x ...) (f x ...))].
Optional (positional or keyword) arguments are not allowed.

This macro behaves similarly to @scheme[eta], but produces a function with
statically known arity which may improve efficiency and error reporting.

@defexamples[
#:eval (evaluator 'unstable/cce/function)
(define f (eta* g x))
f
(procedure-arity f)
(define g (lambda (x) (+ x 1)))
(f 1)
]

}

@section{Parameter Arguments}

@defform/subs[
  (lambda/parameter (param-arg ...) body ...)
  ([param-arg param-arg-spec (code:line keyword param-spec)]
   [param-arg-spec id [id default-expr] [id #:param param-expr]])
]{

Constructs a function much like @scheme[lambda], except that some optional
arguments correspond to the value of a parameter.  For each clause of the form
@scheme[[id #:param param-expr]], @scheme[param-expr] must evaluate to a value
@scheme[param] satisfying @scheme[parameter?].  The default value of the
argument @scheme[id] is @scheme[(param)]; @scheme[param] is bound to @scheme[id]
via @scheme[parameterize] during the function call.

@defexamples[
#:eval (evaluator 'unstable/cce/function)
(define p (open-output-string))
(define hello-world
  (lambda/parameter ([port #:param current-output-port])
    (display "Hello, World!")
    (newline port)))
(hello-world p)
(get-output-string p)
]

}
