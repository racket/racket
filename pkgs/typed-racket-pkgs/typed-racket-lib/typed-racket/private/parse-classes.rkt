#lang racket/base

(require syntax/parse
         "../utils/literal-syntax-class.rkt"
         (for-label "../base-env/base-types-extra.rkt"))
(provide star ddd ddd/bound omit-parens)

(define-literal-syntax-class #:for-label ->)

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

;; The body of a Forall type or type annotation after the second :
;; Allows one level of parentheses to be omitted for infix -> types
(define-syntax-class omit-parens
  #:attributes (type)
  ;; FIXME: the error message when a failure is triggered by this case
  ;;        is not very good, but I have been unsuccessful with ~fail
  ;;        or with #:fail-when. -- AT
  (pattern (~and (:->^ x y ~! z ...) (~fail))
           #:with type 'dummy)
  (pattern (~and type ((~or (~once :->^) (~not :->^)) ...)))
  (pattern (type)))
