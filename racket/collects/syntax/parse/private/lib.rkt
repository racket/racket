#lang racket/base
(require "sc.rkt"
         "keywords.rkt"
         (for-syntax racket/base))

(provide identifier
         boolean
         str
         character
         keyword
         number
         integer
         exact-integer
         exact-nonnegative-integer
         exact-positive-integer

         id
         nat
         char

         expr
         static)


(define (expr-stx? x)
  (not (keyword-stx? x)))

(define ((stxof pred?) x) (and (syntax? x) (pred? (syntax-e x))))
(define keyword-stx? (stxof keyword?))
(define boolean-stx? (stxof boolean?))
(define string-stx? (stxof string?))
(define char-stx? (stxof char?))
(define number-stx? (stxof number?))
(define integer-stx? (stxof integer?))
(define exact-integer-stx? (stxof exact-integer?))
(define exact-nonnegative-integer-stx? (stxof exact-nonnegative-integer?))
(define exact-positive-integer-stx? (stxof exact-positive-integer?))

;; == Integrable syntax classes ==

(define-integrable-syntax-class identifier (quote "identifier") identifier?)
(define-integrable-syntax-class expr (quote "expression") expr-stx?)
(define-integrable-syntax-class keyword (quote "keyword") keyword-stx?)
(define-integrable-syntax-class boolean (quote "boolean") boolean-stx?)
(define-integrable-syntax-class character (quote "character") char-stx?)
(define-integrable-syntax-class str (quote "string") string-stx?)
(define-integrable-syntax-class number (quote "number") number-stx?)
(define-integrable-syntax-class integer (quote "integer") integer-stx?)
(define-integrable-syntax-class exact-integer (quote "exact-integer") exact-integer-stx?)
(define-integrable-syntax-class exact-nonnegative-integer
  (quote "exact-nonnegative-integer")
  exact-nonnegative-integer-stx?)
(define-integrable-syntax-class exact-positive-integer
  (quote "exact-positive-integer")
  exact-positive-integer-stx?)

;; Aliases
(define-syntax id (make-rename-transformer #'identifier))
(define-syntax nat (make-rename-transformer #'exact-nonnegative-integer))
(define-syntax char (make-rename-transformer #'character))

;; == Normal syntax classes ==

(define notfound (box 'notfound))

(define-syntax-class (static pred [name #f])
  #:attributes (value)
  #:description name
  #:commit
  (pattern x:id
           #:fail-unless (syntax-transforming?)
                         "not within the dynamic extent of a macro transformation"
           #:attr value (syntax-local-value #'x (lambda () notfound))
           #:fail-when (eq? (attribute value) notfound) #f
           #:fail-unless (pred (attribute value)) #f))
