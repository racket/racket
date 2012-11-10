#lang racket

(require racket/generic rackunit)

(define-namespace-anchor generic-env)

(define-syntax-rule (check-syntax exp ...)
  (begin
    (check-exn
     exn:fail:syntax?
     (lambda () (eval '(module foo racket/base
                         (require racket/generic)
                         exp)
                      (namespace-anchor->namespace generic-env))))
    ...))

(check-syntax
 (define-generics stream
   (stream-first stream)
   (stream-rest stream)
   (stream-empty? stream)
   #:defaults
   foo)

 (define-generics stream
   (stream-first stream)
   (stream-rest stream)
   (stream-empty? stream)
   #:defaults
   ([list?
     (define stream-first car)
     (define stream-rest cdr)
     (define stream-rest 5)
     (define stream-empty? null?)]))

 (define-generics stream
   (stream-first stream)
   (stream-rest stream)
   (stream-empty? stream)
   #:defaults
   ([])))
