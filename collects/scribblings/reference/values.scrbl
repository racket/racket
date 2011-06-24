#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "values"]{Multiple Values}

See @secref["values-model"] for general information about multiple
result values. In addition to @racket[call-with-values] (described in
this section), the @racket[let-values], @racket[let*-values],
@racket[letrec-values], and @racket[define-values] forms (among
others) create continuations that receive multiple values.

@defproc[(values [v any/c] ...) any]{

Returns the given @racket[v]s. That is, @racket[values] returns as
provided arguments.

@examples[
(values 1)
(values 1 2 3)
(values)
]}

@defproc[(call-with-values [generator (-> any)] [receiver procedure?]) any]{

Calls @racket[generator], and passes the values that
@racket[generator] produces as arguments to @racket[receiver]. Thus,
@racket[call-with-values] creates a continuation that accepts any
number of values that @racket[receiver] can accept. The
@racket[receiver] procedure is called in tail position with respect to
the @racket[call-with-values] call.

@examples[
(call-with-values (lambda () (values 1 2)) +)
(call-with-values (lambda () 1) (lambda (x y) (+ x y)))
]}
