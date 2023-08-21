#lang racket/base

(require racket/generic
         racket/private/generic-methods
         rackunit
         syntax/macro-testing)

;; ---------------------------
;; Without #:scope argument

(check-equal? (generic-method-table gen:equal+hash
                (define equal-proc 'a)
                (define hash-proc 'b)
                (define hash2-proc 'c))
              '#(a b c))

;; missing implementation for hash2-proc is invalid
(check-exn #rx"hash2-proc: required method is not implemented"
           (lambda ()
             (convert-syntax-error
              (generic-method-table gen:equal+hash
                                    (define equal-proc 'd)
                                    (define hash-proc 'e)))))

;; missing implementation for equal-proc is invalid
;; changing the order doesn't affect it
(check-exn #rx"equal-proc: required method is not implemented"
           (lambda ()
             (convert-syntax-error
              (generic-method-table gen:equal+hash
                                    (define hash2-proc 'f)
                                    (define hash-proc 'g)))))

;; ---------------------------
;; With #:scope argument

(check-equal? (generic-method-table gen:equal+hash
                #:scope here
                (define equal-proc 'a)
                (define hash-proc 'b)
                (define hash2-proc 'c))
              '#(a b c))

(test-case "macro introducing generic-interface identifier"
  ;; This messes with scope and prevents it from implementing the methods
  (define-syntax-rule (equal+hash-method-table def ...)
    (generic-method-table gen:equal+hash def ...))

  (check-exn #rx"required method is not implemented"
             (lambda ()
               (convert-syntax-error
                (equal+hash-method-table
                 (define equal-proc 'a)
                 (define hash-proc 'b)
                 (define hash2-proc 'c)))))

  ;; But the scope argument can specify the scope for capturing methods
  (define-syntax-rule (equal+hash-method-table/scope scope def ...)
    (generic-method-table gen:equal+hash #:scope scope def ...))

  (check-equal? (equal+hash-method-table/scope
                 here
                 (define equal-proc 'a)
                 (define hash-proc 'b)
                 (define hash2-proc 'c))
                '#(a b c)))

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

