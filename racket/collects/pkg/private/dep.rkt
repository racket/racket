#lang racket/base
(require setup/matching-platform
         "../name.rkt")

(provide (all-defined-out))

(define (dependency->name dep)
  (package-source->name
   (dependency->source dep)))

(define (dependency->source dep)
  (if (string? dep)
      dep
      (car dep)))

(define (dependency->version dep)
  (cond
   [(string? dep) #f]
   [(null? (cdr dep)) #f]
   [(keyword? (cadr dep))
    (dependency-lookup '#:version dep)]
   [else (cadr dep)]))

(define (dependency-lookup kw dep)
  (cond
   [(string? dep) #f]
   [(null? (cdr dep)) #f]
   [(keyword? (cadr dep))
    (define p (member kw (cdr dep)))
    (and p (cadr p))]
   [else #f]))

(define (dependency-this-platform? dep)
  (define p (dependency-lookup '#:platform dep))
  (or (not p) (matching-platform? p #:cross? #t)))

