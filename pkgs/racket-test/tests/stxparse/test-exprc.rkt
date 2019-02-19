#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/contract
         rackunit)

;; See also tests/syntax/contract/test-errors.rkt.

(define (check-ctc-exn rx swapped? thunk)
  ((with-handlers ([(lambda (e) #t)
                    (lambda (e)
                      (check-pred exn:fail:contract:blame? e)
                      (define b (exn:fail:contract:blame-object e))
                      (check-equal? (blame-swapped? b) swapped?)
                      (check-regexp-match rx (exn-message e))
                      void)])
     (thunk)
     (lambda () (fail "no exn raised")))))

(define-syntax (m-str stx)
  (syntax-parse stx
    [(_ e)
     #:declare e (expr/c #'string?)
     #'e.c]))

(check-equal? (m-str "string") "string")
(check-ctc-exn #rx"m-str: contract violation.*expected: string?" #t
               (lambda () (m-str 'not-a-string)))

(define-syntax (m-arr stx)
  (syntax-parse stx
    [(_ f arg)
     #:declare f (expr/c #'(-> string? any))
     #'(f.c arg)]))

(check-equal? (m-arr string->symbol "a") 'a)
(check-ctc-exn #rx"m-arr: contract violation.*expected: string?" #f
               (lambda () (m-arr string->symbol 'a)))

(define-syntax (m-app stx)
  (syntax-parse stx
    [(_ ctc f arg)
     #:declare f (expr/c #'ctc)
     #'(f.c arg)]))

(check-equal? (m-app (-> string? symbol?) string->symbol "A") 'A)
(check-equal? ((m-app (-> string? (-> string? string?))
                      (lambda (s) (lambda (t) (string-append s t)))
                      "abc")
               "def")
              "abcdef")

(check-ctc-exn #rx"m-app: contract violation.*expected: string?" #f
               ;; Yes, it's m-app's fault, because it didn't protect
               ;; f from bad arguments.
               (lambda ()
                 ((m-app (-> string? (-> string? string?))
                         (lambda (s) (lambda (t) (string-append s t)))
                         "abc")
                  'def)))

(define-syntax (m-res stx)
  (syntax-parse stx
    [(_ ctc v)
     #:declare v (expr/c #'ctc #:arg? #f)
     #'v.c]))

(check-equal? (m-res string? "hello") "hello")
(check-equal? (((m-res (-> string? (-> string? string?))
                       (lambda (s) (lambda (t) (string-append s t))))
                "abc") "def")
              "abcdef")
(check-ctc-exn #rx"m-res: contract violation.*expected: string?" #t
               (lambda ()
                 (((m-res (-> string? (-> string? string?))
                          (lambda (s) (lambda (t) (string-append s t))))
                   'abc) "def")))
(check-ctc-exn #rx"m-res: contract violation.*expected: string?" #t
               (lambda ()
                 (((m-res (-> string? (-> string? string?))
                          (lambda (s) (lambda (t) (string-append s t))))
                   "abc") 'def)))
