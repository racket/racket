#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide bad-assign-value
         bad-cast-value
         bad-argument
         bad-result
         type-name->string
         check-wrap-proc
         check-fail-proc
         build-fail
         (struct-out failure-result)
         compose-racket->c
         check-c->racket
         check-release)

(define-syntax-rule (discourage-inline)
  (#%foreign-inline (void)))

(define (bad-assign-value who what val)
  (discourage-inline)
  (raise-arguments-error who "value does not match type"
                         "value" val
                         "ffi2 type" (unquoted-printing-string (type-name->string what))))

(define (bad-cast-value who what val)
  (bad-assign-value who what val))

(define (bad-argument what val)
  (discourage-inline)
  (raise-arguments-error 'ffi2 "foreign-procedure argument does not match type"
                         "argument" val
                         "argument ffi2 type" (unquoted-printing-string (type-name->string what))))

(define (bad-result what val)
  (discourage-inline)
  (raise-arguments-error 'ffi2 "foreign-callback result does not match type"
                         "result" val
                         "result ffi2 type" (unquoted-printing-string (type-name->string what))))

(define (type-name->string what)
  (if what
      (format "~a" what)
      ;; unnamed types are from `struct` without tag, etc., which
      ;; are represented by generic pointers
      "ptr_t"))

(define (check-wrap-proc who wrap)
  (unless (and (procedure? wrap) (procedure-arity-includes? wrap 1))
    (raise-argument-error who "(procedure-arity-includes/c 1)" wrap))
  wrap)

(define (check-fail-proc who fail)
  (unless (or (not fail) (and (procedure? fail) (procedure-arity-includes? fail 1)))
    (raise-argument-error who "(procedure-arity-includes/c 1)" fail))
  fail)

(define (build-fail who fail name)
  (check-fail-proc who fail)
  (and fail (lambda () (failure-result (fail name)))))

(define-struct failure-result (v))

(define-syntax (compose-racket->c stx)
  (syntax-parse stx
    [(_ who racket->c-expr next-pred)
     #`(let ([racket->c racket->c-expr])
         (if (and (procedure? racket->c) (procedure-arity-includes? racket->c 1))
             (lambda (v)
               (let ([r (racket->c v)])
                 (unless (next-pred r)
                   (bad-convert-result who v r))
                 r))
             (raise-argument-error who "(procedure-arity-includes/c 1)" racket->c)))]))

(define-syntax (check-c->racket stx)
  (syntax-parse stx
    [(_ who c->racket-expr)
     #'(let ([c->racket c->racket-expr])
         (if (and (procedure? c->racket) (procedure-arity-includes? c->racket 1))
             c->racket
             (raise-argument-error who "(procedure-arity-includes/c 1)" c->racket)))]))

(define-syntax (check-release stx)
  (syntax-parse stx
    [(_ who release-expr)
     #'(let ([release release-expr])
         (if (and (procedure? racket->c) (procedure-arity-includes? racket->c 1))
             release
             (raise-argument-error who "(procedure-arity-includes/c 1)" release)))]))

(define (bad-convert-result who v r)
  (raise-arguments-error who
                         "ffi type converter result does not satisfy next type's predicate"
                         "converter input" v
                         "converter result" r))
