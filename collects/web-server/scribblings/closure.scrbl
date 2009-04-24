#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "closure.ss"]{Serializable Closures}
@(require (for-label web-server/private/closure
                     web-server/private/define-closure))

@defmodule[web-server/private/closure]{

The defunctionalization process of the Web Language (see @secref["stateless" #:doc '(lib "web-server/scribblings/web-server.scrbl")])
requires an explicit representation of closures that is serializable. This module provides that representation.

@defproc[(make-closure-definition-syntax [tag syntax?]
                                         [fvars (listof identifier?)]
                                         [proc syntax?])
         syntax?]{
 Outputs a syntax object that defines a serializable structure,
 with @scheme[tag] as the tag, that represents a closure over
 @scheme[fvars], that acts a procedure and when invoked calls
 @scheme[proc], which is assumed to be syntax of @scheme[lambda]
 or @scheme[case-lambda].
}

@defproc[(closure->deserialize-name [c closure?])
         symbol?]{
 Extracts the unique tag of a closure @scheme[c].
}
                 
}
                 
These are difficult to use directly, so we provide a helper syntactic form:

@section[#:style 'hidden]{Define Closure}
@defmodule[web-server/private/define-closure]{

@defform[(define-closure tag formals (free-vars ...) body)]{
 Defines a closure, constructed with @scheme[make-tag] that accepts closure that returns
 @scheme[freevars ...], that when invoked with @scheme[formals]
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