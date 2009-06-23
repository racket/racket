#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "closure.ss"]{Serializable Closures}
@(require (for-label scheme/serialize
                     web-server/lang/closure
                     web-server/lang/serial-lambda
                     web-server/private/define-closure))

                                              
The defunctionalization process of the Web Language (see @secref["stateless" #:doc '(lib "web-server/scribblings/web-server.scrbl")])
requires an explicit representation of closures that is serializable.

@defmodule[web-server/lang/serial-lambda]{
 
@defform[(serial-lambda formals body ...)]{
 Returns @scheme[(lambda formals body ...)], except it is serializable.
}
        
@defform[(serial-case-lambda [formals body ...] ...)]{
 Returns @scheme[(case-lambda [formals body ...] ...)], except it is serializable.
}
        
}

@section[#:style 'hidden]{Definition Syntax}

@defmodule[web-server/private/define-closure]{

@defform[(define-closure tag formals (free-var ...) body)]{
 Defines a closure, constructed with @scheme[make-tag] that accepts a closure that returns
 @scheme[freevar ...], that when invoked with @scheme[formals]
 executes @scheme[body].
}

Here is an example:
@schememod[
 scheme
(require scheme/serialize)

(define-closure foo (a b) (x y)
  (+ (- a b)
     (* x y)))

(define f12 (make-foo (lambda () (values 1 2))))
(serialize f12)
#,(schemeresult '((1) 1 (('page . foo:deserialize-info)) 0 () () (0 1 2)))
(f12 6 7)
#,(schemeresult 1)
(f12 9 1)
#,(schemeresult 10)

(define f45 (make-foo (lambda () (values 4 5))))
(serialize f45)
#,(schemeresult '((1) 1 (('page . foo:deserialize-info)) 0 () () (0 4 5)))
(f45 1 2)
#,(schemeresult 19)
(f45 8 8)
#,(schemeresult 20)
]

}