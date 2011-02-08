#lang racket/base

(provide (all-defined-out))

;; specialized version of haskell do notation for the particular parsing monad i'm using
(define-syntax try-parsing 
  (syntax-rules ()
    [(_ v () body) (body v)]
    [(_ v ([expr] clause ...) body)
     (let-values ([(dummy rest) (expr v)])
       (try-parsing rest (clause ...) body))]
    [(_ v ([id expr] clause ...) body)
     (let-values ([(id rest) (expr v)])
       (try-parsing rest (clause ...) body))]))

;; get-next-fragment : regexp -> [#:on-error (string -> a)] -> string -> (union (values string string) a)
;; helper for the below two functions
(define (((get-next-fragment rx) #:on-error [error-action (λ (s) (values #f s))]) str)
  (let ([thematch (regexp-match rx str)])
    (cond
      [(not thematch) (error-action str)]
      [else
       (let ([this (list-ref thematch 1)]
             [rest (list-ref thematch 2)])
         (values this rest))])))

;; get-next-slash : [#:on-error (string -> a)] -> string -> (union (values string string) a)
;; splits the given string into the nonempty substring before the first slash and the substring after it
;; on failure returns whatever the given #:on-error function returns when given the entire string
(define consume-whitespace (get-next-fragment #rx"^([ ]*)(.*)$"))
(define get-next-slash (get-next-fragment #rx"^([^/]+)/(.*)$"))
(define get-next-slash-or-end (get-next-fragment #rx"^([^/ ]+)/? ?(.*)$"))

;; get-to-next-colon-or-end : [#:on-error (string -> a)] -> string -> (union (values string string) a)
;; splits the given string into the nonempty substring before the initial : and the substring after it, or 
;; (values [initial-string] "") if the given string has no : in it.
(define get-to-next-colon-or-end (get-next-fragment #rx"^([^:]+):?(.*)$"))

;; parse-package : string (string -> 'a) -> (values string nat min-spec)
;; given a package specifier, returns the package name, the package major version, and a descriptor
;; for the acceptable minor versions
(define (parse-package package yell)
  (try-parsing package
               ([pkgname (get-to-next-colon-or-end)]
                [maj     (get-to-next-colon-or-end)])
               (λ (min) 
                 (values (parse-pkgname pkgname yell) 
                                (parse-majspec maj     yell)
                                (parse-minspec min     yell)))))

;; parse-pkgname : string (string -> 'a) -> string
;; given a literal package name string as it would appear in shorthand syntax, returns
;; a fully-embellished name for the package being specified. yell  is provided as a function
;; to call to generate an error message if something goes wrong
(define (parse-pkgname pn yell)
  (let ([m (regexp-match #rx"\\.plt$" pn)])
    (if m pn (string-append pn ".plt"))))

;; parse-majspec : (#f (string -> 'a) -> #f) intersect (string (string -> 'a) -> number)
;; given the literal major version string (or #f) returns the major version corresponding
;; to that string. yell is the function to call with an error message if something goes wrong 
(define (parse-majspec majstr yell)
  (cond
    [(not majstr) #f]
    [else
     (cond
       [(and (regexp-match #rx"^[0-9]+$" majstr))
        (let ([n (string->number majstr)])
          (if (> n 0)
              n
              (yell (format "Illegal major version specifier; expected version number greater than 0, received ~e" 
                            majstr))))]
       [else 
        (yell (format "Illegal major version specifier; expected positive integer, received ~e" majstr))])]))

;; regexp-case : SYNTAX
;; provides a case operation for trying different regular expressions in sequence on a test string,
;; stoppingas soon as one of those expressions matches the string. If one does, then all the
;; parenthesized subparts are bound to names in the right-hand side of the corresponding clause
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

;; parse-minspec : string (string -> 'a) -> min-spec
;; returns the minor-version specification corresponding to the given string as an s-expression.
;; yell is the function to call if the string doesn't correspond to minor-version spec. 
(define (parse-minspec minstr yell)
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
        (yell (format "Illegal minor version specifier; expected <=n, >=n, =n, n-m, or n, where n, m are positive integers; received ~e"
                      minstr))])]))
