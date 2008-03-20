#lang scheme/base

(provide (all-defined-out))

(define-syntax try-parsing 
  (syntax-rules ()
    [(_ v () body) (body v)]
    [(_ v ([id expr] [id2 expr2] ...) body)
     (let-values ([(this rest) (expr v)])
       (let ([id this])
         (try-parsing rest ([id2 expr2] ...) body)))]))

;; get-next-fragment : string -> (values string string)
(define (((get-next-fragment rx) #:on-error [error-action (λ (s) (values #f s))]) str)
  (let ([thematch (regexp-match rx str)])
    (cond
      [(not thematch) (error-action str)]
      [else
       (let ([this (list-ref thematch 1)]
             [rest (list-ref thematch 2)])
         (values this rest))])))

(define get-next-slash (get-next-fragment #rx"([^/]+)/(.*)"))
(define get-to-next-colon-or-end (get-next-fragment #rx"([^:]+):?(.*)"))

(define (parse-package package stx)
  (try-parsing package
               ([pkgname (get-to-next-colon-or-end)]
                [maj     (get-to-next-colon-or-end)])
               (λ (min) (values (parse-pkgname pkgname stx) 
                                (parse-majspec maj     stx)
                                (parse-minspec min     stx)))))

(define (parse-pkgname pn stx)
  (let ([m (regexp-match #rx"\\.plt$" pn)])
    (if m pn (string-append pn ".plt"))))

(define (parse-majspec majstr stx)
  (cond
    [(not majstr) #f]
    [else 
     (let ([num (string->number majstr)])
       (unless (and (integer? num) (> num 0))
         (raise-syntax-error #f 
                             (format "Illegal major version specifier; expected positive integer, received ~e" majstr)
                             stx))
       num)]))

(define-syntax regexp-case
  (syntax-rules ()
    [(_ str clause ...)
     (let ([s str])
       (regexp-case* s clause ...))]))

(define-syntax regexp-case*
  (syntax-rules (else)
    [(_ str [else body] c ...)
     body]
    [(_ str [re ([id ...] body)] c ...)
     (let ([args (regexp-match re str)])
       (if args
           (let-values ([(id ...) (apply values (cdr args))]) body)
           (regexp-case* str c ...)))]))

(define (parse-minspec minstr stx)
  (cond
    [(not minstr) #f]
    [else
     (regexp-case minstr
       [#rx"^>=([0-9]+)$"        ((n)   `(+ ,(string->number n)))]
       [#rx"^<=([0-9]+)$"        ((n)   `(- ,(string->number n)))]
       [#rx"^=([0-9]+)$"         ((n)   `(= ,(string->number n)))]
       [#rx"^([0-9]+)-([0-9]+)$" ((m n) `(,(string->number m) ,(string->number n)))]
       [#rx"^([0-9]+)$"          ((n)   (string->number n))]
       [#rx"^$"                  (()    #f)] ;; here for convenience reasons. a bit gross, i know
       [else
        (raise-syntax-error #f
                            (format "Illegal minor version specifier; expected <=n, >=n, =n, n-m, or n, where n, m are positive integers; received ~e" minstr)
                            stx)])]))
