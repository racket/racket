#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/function))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/function))

@title{Functions}

@defmodule[unstable/function]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

This module provides tools for higher-order programming and creating functions.

@section{Simple Functions}

@section{Higher Order Predicates}

@defproc[((negate [f (-> A ... boolean?)]) [x A] ...) boolean?]{

Negates the results of @racket[f]; equivalent to @racket[(not (f x ...))].

This function is reprovided from @racketmodname[scheme/function].

@defexamples[
#:eval the-eval
(define f (negate exact-integer?))
(f 1)
(f 'one)
]

}

@defproc[((conjoin [f (-> A ... boolean?)] ...) [x A] ...) boolean?]{

Combines calls to each function with @racket[and].  Equivalent to
@racket[(and (f x ...) ...)]

@defexamples[
#:eval the-eval
(define f (conjoin exact? integer?))
(f 1)
(f 1.0)
(f 1/2)
(f 0.5)
]

}

@defproc[((disjoin [f (-> A ... boolean?)] ...) [x A] ...) boolean?]{

Combines calls to each function with @racket[or].  Equivalent to
@racket[(or (f x ...) ...)]

@defexamples[
#:eval the-eval
(define f (disjoin exact? integer?))
(f 1)
(f 1.0)
(f 1/2)
(f 0.5)
]

}

@section{Currying and (Partial) Application}

@defproc[(call [f (-> A ... B)] [x A] ...) B]{

Passes @racket[x ...] to @racket[f].  Keyword arguments are allowed.  Equivalent
to @racket[(f x ...)].  Useful for application in higher-order contexts.

@defexamples[
#:eval the-eval
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

The @racket[papply] and @racket[papplyr] functions partially apply @racket[f] to
@racket[x ...], which may include keyword arguments.  They obey the following
equations:

@racketblock[
((papply f x ...) y ...) = (f x ... y ...)
((papplyr f x ...) y ...) = (f y ... x ...)
]

@defexamples[
#:eval the-eval
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

@emph{Note:} The @racket[ooo] above denotes a loosely associating ellipsis.

The @racket[curryn] and @racket[currynr] functions construct a curried version
of @racket[f], specialized at @racket[x ...], that produces a result after
@racket[n] further applications.  Arguments at any stage of application may
include keyword arguments, so long as no keyword is duplicated.  These curried
functions obey the following equations:

@racketblock[
(curryn 0 f x ...) = (f x ...)
((curryn (+ n 1) f x ...) y ...) = (curryn n f x ... y ...)

(currynr 0 f x ...) = (f x ...)
((currynr (+ n 1) f x ...) y ...) = (currynr n f y ... x ...)
]

The @racket[call], @racket[papply], and @racket[papplyr] utilities are related
to @racket[curryn] and @racket[currynr] in the following manner:

@racketblock[
(call f x ...) = (curryn 0 f x ...) = (currynr 0 f x ...)
(papply f x ...) = (curryn 1 f x ...)
(papplyr f x ...) = (currynr 1 f x ...)
]

@defexamples[
#:eval the-eval

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

Produces a function equivalent to @racket[f], except that @racket[f] is
evaluated every time it is called.

This is useful for function expressions that may be run, but not called, before
@racket[f] is defined.  The @racket[eta] expression will produce a function
without evaluating @racket[f].

@defexamples[
#:eval the-eval
(define f (eta g))
f
(define g (lambda (x) (+ x 1)))
(f 1)
]

}

@defform[(eta* f x ...)]{

Produces a function equivalent to @racket[f], with argument list @racket[x ...].
In simple cases, this is equivalent to @racket[(lambda (x ...) (f x ...))].
Optional (positional or keyword) arguments are not allowed.

This macro behaves similarly to @racket[eta], but produces a function with
statically known arity which may improve efficiency and error reporting.

@defexamples[
#:eval the-eval
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

Constructs a function much like @racket[lambda], except that some optional
arguments correspond to the value of a parameter.  For each clause of the form
@racket[[id #:param param-expr]], @racket[param-expr] must evaluate to a value
@racket[param] satisfying @racket[parameter?].  The default value of the
argument @racket[id] is @racket[(param)]; @racket[param] is bound to @racket[id]
via @racket[parameterize] during the function call.

@defexamples[
#:eval the-eval
(define p (open-output-string))
(define hello-world
  (lambda/parameter ([port #:param current-output-port])
    (display "Hello, World!")
    (newline port)))
(hello-world p)
(get-output-string p)
]

}

@(close-eval the-eval)
