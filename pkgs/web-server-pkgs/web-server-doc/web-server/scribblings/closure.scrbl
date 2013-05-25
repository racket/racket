#lang scribble/doc
@(require "web-server.rkt")

@title[#:tag "closure"]{Serializable Closures}
@(require (for-label racket/serialize
                     web-server/lang/closure
                     web-server/lang/serial-lambda
                     web-server/private/define-closure))

The defunctionalization process of the Web Language (see
@secref["stateless" #:doc '(lib "web-server/scribblings/web-server.scrbl")])
requires an explicit representation of closures that is serializable.

@defmodule[web-server/lang/serial-lambda]{

@defform[(serial-lambda formals body ...)]{
  Returns @racket[(lambda formals body ...)], except it is serializable.
}

@defform[(serial-case-lambda [formals body ...] ...)]{
 Returns @racket[(case-lambda [formals body ...] ...)], except it is
 serializable.
}

}

@section[#:style 'hidden]{Definition Syntax}

@defmodule[web-server/private/define-closure]{

@defform[(define-closure tag formals (free-var ...) body)]{
 Defines a closure, constructed with @racket[make-tag] that accepts a closure that returns
 @racket[freevar ...], that when invoked with @racket[formals]
 executes @racket[body].
}

Here is an example:
@racketmod[
 racket
(require racket/serialize)

(define-closure foo (a b) (x y)
  (+ (- a b)
     (* x y)))

(define f12 (make-foo (lambda () (values 1 2))))
(serialize f12)
#,(racketresult '((1) 1 (('page . foo:deserialize-info)) 0 () () (0 1 2)))
(f12 6 7)
#,(racketresult 1)
(f12 9 1)
#,(racketresult 10)

(define f45 (make-foo (lambda () (values 4 5))))
(serialize f45)
#,(racketresult '((1) 1 (('page . foo:deserialize-info)) 0 () () (0 4 5)))
(f45 1 2)
#,(racketresult 19)
(f45 8 8)
#,(racketresult 20)
]

}
