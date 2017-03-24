#lang racket/base

(require racket/generic
         racket/private/generic-methods
         rackunit)

;; ---------------------------
;; Without #:scope argument

(check-equal? (generic-method-table gen:equal+hash
                (define equal-proc 'a)
                (define hash-proc 'b)
                (define hash2-proc 'c))
              '#(a b c))

;; missing implementation for hash2-proc filled in with false
(check-equal? (generic-method-table gen:equal+hash
                (define equal-proc 'd)
                (define hash-proc 'e))
              '#(d e #f))

;; missing implementation for equal-proc filled in with false
;; changing the order doesn't affect it
(check-equal? (generic-method-table gen:equal+hash
                (define hash2-proc 'f)
                (define hash-proc 'g))
              '#(#f g f))

;; ---------------------------
;; With #:scope argument

(check-equal? (generic-method-table gen:equal+hash
                #:scope here
                (define equal-proc 'a)
                (define hash-proc 'b))
              '#(a b #f))

(test-case "macro introducing generic-interface identifier"
  ;; This messes with scope and prevents it from implementing the methods
  (define-syntax-rule (equal+hash-method-table def ...)
    (generic-method-table gen:equal+hash def ...))

  (check-equal? (equal+hash-method-table
                 (define equal-proc 'a)
                 (define hash-proc 'b))
                '#(#f #f #f))

  ;; But the scope argument can specify the scope for capturing methods
  (define-syntax-rule (equal+hash-method-table/scope scope def ...)
    (generic-method-table gen:equal+hash #:scope scope def ...))

  (check-equal? (equal+hash-method-table/scope
                 here
                 (define equal-proc 'a)
                 (define hash-proc 'b))
                '#(a b #f)))

;; ---------------------------
;; With define/generic

(define-generics foo
  (f foo)
  (g foo))

(check-equal? (generic-method-table gen:foo
                (define/generic gen-f f)
                (define f (list gen-f)))
              (vector (list f) #false))

(check-equal? (generic-method-table gen:foo
                (define/generic gen-f f)
                ;; gen-f refers the global f
                (define f (list gen-f))
                (define/generic gen-g g)
                ;; f refers to the local f, gen-g to the global g
                (define g (list f gen-g)))
              (vector (list f) (list (list f) g)))

