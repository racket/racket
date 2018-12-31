#lang racket/base
(require (for-syntax racket/base syntax/contract)
         racket/contract
         rackunit)

(define-syntax (m-str stx)
  (syntax-case stx ()
    [(_ e)
     (wrap-expr/c #'string? #'e #:context stx)]))

(check-equal? (m-str "string") "string")
(check-exn #rx"m-str: contract violation.*expected: string?"
           (lambda () (m-str 'not-a-string)))

(define-syntax (m-arr stx)
  (syntax-case stx ()
    [(_ f arg)
     (with-syntax ([f* (wrap-expr/c #'(-> string? any) #'f #:context stx)])
       #'(f* arg))]))

(check-equal? (m-arr string->symbol "a") 'a)
(check-exn #rx"m-arr: broke its own contract.*promised: string?"
           (lambda () (m-arr string->symbol 'a)))

(define-syntax (m-app stx)
  (syntax-case stx ()
    [(_ f ctc arg)
     (with-syntax ([f* (wrap-expr/c #'ctc #'f #:context stx)])
       #'(f* arg))]))

(check-equal? (m-app string->symbol (-> string? symbol?) "A") 'A)
(check-equal? ((m-app (lambda (s) (lambda (t) (string-append s t)))
                      (-> string? (-> string? string?))
                      "abc")
               "def")
              "abcdef")

(check-exn #rx"m-app: broke its own contract.*promised: string?"
           ;; Yes, it's m-app's fault, because it didn't protect
           ;; f from bad arguments.
           (lambda ()
             ((m-app (lambda (s) (lambda (t) (string-append s t)))
                     (-> string? (-> string? string?))
                     "abc")
              'def)))

(define-syntax (m-res stx)
  (syntax-case stx ()
    [(_ v ctc)
     (wrap-expr/c #'ctc #'v #:arg? #f #:context stx)]))

(check-equal? (m-res "hello" string?) "hello")
(check-equal? (((m-res (lambda (s) (lambda (t) (string-append s t)))
                      (-> string? (-> string? string?)))
                "abc") "def")
              "abcdef")
(check-exn #rx"m-res: contract violation.*expected: string?"
           (lambda ()
             (((m-res (lambda (s) (lambda (t) (string-append s t)))
                      (-> string? (-> string? string?)))
               'abc) "def")))
(check-exn #rx"m-res: contract violation.*expected: string?"
           (lambda ()
             (((m-res (lambda (s) (lambda (t) (string-append s t)))
                      (-> string? (-> string? string?)))
               "abc") 'def)))

(let ()
  (define fruit/c (and/c string? (or/c "orange" "peach" "strawberry")))
  (define-syntax (smoothie stx)
    (syntax-case stx ()
      [(_ ing)
       (with-syntax ([ing.c (wrap-expr/c #'fruit/c #'ing #:context stx)])
         #'(format "icy blended ~s" ing.c))]))
  (check-exn
   (regexp
    (string-append
     "^smoothie: contract violation.*"
     "given: \"kale\".*"
     "in:.*\\(and/c string[?] \\(or/c \"orange\" \"peach\" \"strawberry\"\\)\\).*"))
   (lambda () (smoothie "kale"))))
