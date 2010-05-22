#lang scheme/base

(require (for-syntax scheme/base scheme/list "syntax-core.ss"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Definition Generalization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-single-definition)

(define-syntax-rule (define-single-definition define-one define-many)
  (define-syntax define-one
    (syntax-rules []
      [(_ (head . args) . body) (define-one head (lambda args . body))]
      [(_ name expr) (define-many [name] expr)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Potentially Redundant Bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide define-if-unbound
         define-values-if-unbound
         define-syntaxes-if-unbound
         define-syntax-if-unbound)

(define-syntax (define-many-if-unbound stx)
  (syntax-case stx []
    [(_ def [name ...] expr)
     (let* ([ids (syntax->list #'(name ...))])
       (for ([bad (in-list ids)] #:when (not (identifier? bad)))
         (syntax-error bad "expected an identifier"))
       (let*-values ([(bound unbound) (partition identifier-binding ids)])
         (cond
          [(null? bound) (syntax/loc stx (def [name ...] expr))]
          [(null? unbound) (syntax/loc stx (def [] (values)))]
          [else (syntax-error
                 stx
                 "conflicting definitions for ~s; none for ~s"
                 (map syntax-e bound)
                 (map syntax-e unbound))])))]))

(define-syntax-rule (define-values-if-unbound [name ...] expr)
  (define-many-if-unbound define-values [name ...] expr))

(define-single-definition define-if-unbound define-values-if-unbound)

(define-syntax-rule (define-syntaxes-if-unbound [name ...] expr)
  (define-many-if-unbound define-syntaxes [name ...] expr))

(define-single-definition define-syntax-if-unbound define-syntaxes-if-unbound)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Trampoline Expansion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide #%trampoline)

(define-syntax (#%trampoline stx)
  (syntax-case stx ()
    [(_ thunk)
     (procedure? (syntax-e #'thunk))
     (#%app (syntax-e #'thunk))]))
