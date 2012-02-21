#lang eopl

;; type environments and associated procedures.
;; In chapter7/checked, this is in checker.scm.

(require "lang.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;

(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv
   (syms (list-of symbol?))
   (vals (list-of type?))
   (tenv type-environment?))
  (extend-tenv-with-self-and-super
   (self type?)
   (super-name symbol?)
   (saved-env type-environment?)))

(define init-tenv
  (lambda ()
    (extend-tenv
     '(i v x)
     (list (int-type) (int-type) (int-type))
     (empty-tenv))))

(define apply-tenv
  (lambda (env search-sym)
    (cases type-environment env
      (empty-tenv ()
                  (eopl:error 'apply-tenv "No type found for ~s" search-sym))
      (extend-tenv (bvars types saved-env)
                   (cond
                     ((location search-sym bvars)
                      => (lambda (n) (list-ref types n)))
                     (else
                      (apply-tenv saved-env search-sym))))
      (extend-tenv-with-self-and-super (self-name super-name saved-env)
                                       (case search-sym
                                         ((%self) self-name)
                                         ((%super) super-name)
                                         (else (apply-tenv saved-env search-sym)))))))

(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms)) => (lambda (n) (+ n 1)))
      (else #f))))
