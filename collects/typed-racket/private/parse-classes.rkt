#lang racket/base

(require syntax/parse)
(provide star ddd ddd/bound)

(define-syntax-class star
  #:description "*"
  (pattern star:id
           #:fail-unless (eq? '* (syntax-e #'star)) "missing *")
  (pattern star:id
           #:fail-unless (eq? '...* (syntax-e #'star)) "missing ...*"))

(define-syntax-class ddd
  #:description "..."
  (pattern ddd:id
           #:fail-unless (eq? '... (syntax-e #'ddd)) "missing ..."))

(define-splicing-syntax-class ddd/bound
  #:description "... followed by variable name"
  #:attributes (bound)
  (pattern i:id
           #:attr s (symbol->string (syntax-e #'i))
           #:fail-unless ((string-length (attribute s)) . > . 3) #f
           #:fail-unless (equal? "..." (substring (attribute s) 0 3)) "missing ..."
           #:attr bound (datum->syntax #'i (string->symbol (substring (attribute s) 3)) #'i #'i))
  (pattern (~seq _:ddd bound:id)))
