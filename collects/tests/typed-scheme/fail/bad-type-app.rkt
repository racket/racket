#;
(exn-pred exn:fail:syntax?)
#lang typed-scheme

(define-typed-struct type ())

(define-typed-struct (type-base type) ([name : Symbol]) #:transparent)
(define-typed-struct (type-var  type) ([uniq : (U Symbol Number)]) #:transparent)
(define-typed-struct (type-dots type) ([base : type])   #:transparent)

(define-typed-struct (type-fun  type) ([args  : (Listof type)] [ret : type])  #:transparent)
(define-typed-struct (type-un   type) ([cases : (Listof type)])               #:transparent)
(define-typed-struct (type-vals type) ([elems : (Listof type)])               #:transparent)

(define-typed-struct (type-poly type) ([vars  : (Listof (U (type-dots type-var) type-var))]))
