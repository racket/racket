#lang scheme

(provide define-keywords function-with-arity except err)

(require 
 (for-template "syn-aux-aux.ss" 
               scheme
               (rename-in lang/prim (first-order->higher-order f2h))))

(define-syntax-rule (define-keywords the-list (kw coerce) ...)
  (begin
    (provide kw ...)
    (define-syntax (kw x)
      (raise-syntax-error 'kw "used out of context" x))
    ...
    (define-for-syntax the-list (list (list 'kw (coerce ''kw)) ...))))

(define-syntax function-with-arity 
  (syntax-rules (except)
    [(_ arity)
     (lambda (tag)
       (lambda (p)
         (syntax-case p ()
           [(x) #`(proc> #,tag (f2h x) arity)]
           [_ (err tag p)])))]
    [(_ arity except extra)
     (lambda (tag)
       (lambda (p)
         (syntax-case p ()
           [(x) #`(proc> #,tag (f2h x) arity)]
           extra
           [_ (err tag p)])))]))

(define (err spec p . extra-spec)
  (raise-syntax-error (cadr spec)
    (if (null? extra-spec)
	"illegal specification"
	(string-append "illegal specification: " (car extra-spec)))
    #`(#,spec . #,p) p))

