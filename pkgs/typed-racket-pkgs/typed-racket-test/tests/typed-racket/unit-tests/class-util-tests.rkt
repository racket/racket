#lang racket/base

;; Tests for utilities and helpers for the internals of
;; class type-checking, parsing, etc.

(require (except-in "test-utils.rkt" private)
         racket/class
         rackunit
         syntax/id-table
         syntax/parse
         syntax/stx
         ;; phase-shift down for use in tests below
         (for-template (submod typed-racket/base-env/class-prims internal)
                       (submod typed-racket/base-env/class-clauses internal)
                       (only-in typed-racket/base-env/class-clauses
                                class-clause clause init-clause)))

(provide tests)
(gen-test-main)

;; equal? check but considers id & stx pair equality for the
;; specific test cases that appear here (it's not a general check)
(define (equal?/id x y)
  (cond [(and (identifier? x) (identifier? y))
         (free-identifier=? x y)]
        [(and (syntax? x) (syntax? y))
         (and (identifier? (stx-car x))
              (identifier? (stx-car y))
              (free-identifier=? (stx-car x) (stx-car y))
              (free-identifier=? (stx-car (stx-cdr x))
                                 (stx-car (stx-cdr y))))]
        [else (equal?/recur x y equal?/id)]))

;; utility macro for checking if a syntax matches a
;; given syntax class
(define-syntax-rule (syntax-parses? stx syntax-class)
  (syntax-parse stx
    [(~var _ syntax-class) #t]
    [_ #f]))

;; for rackunit with equal?/id
(define-binary-check (check-equal?/id equal?/id actual expected))

(define tests
  (test-suite "Class utility tests"
    (check-true (syntax-parses? #'x init-decl))
    (check-true (syntax-parses? #'([x y]) init-decl))
    (check-true (syntax-parses? #'(x 0) init-decl))
    (check-true (syntax-parses? #'([x y] 0) init-decl))

    (check-equal?/id
     (get-optional-inits
      (list (init-clause #'(init [x 0]) #'init #'([x x])
                         (list #f) (list #t))
            (init-clause #'(init [(a b)]) #'init #'([a b])
                         (list #f) (list #f))))
     (list #'x))))

