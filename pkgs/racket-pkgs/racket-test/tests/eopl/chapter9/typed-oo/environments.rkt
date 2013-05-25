#lang eopl

(require "data-structures.rkt")
(require "store.rkt")
(provide init-env empty-env extend-env apply-env env->list)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> environment

;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.  

(define init-env 
  (lambda ()
    (extend-env1
     'i (newref (num-val 1))
     (extend-env1
      'v (newref (num-val 5))
      (extend-env1
       'x (newref (num-val 10))
       (empty-env))))))

(define extend-env1
  (lambda (id val env)
    (extend-env (list id) (list val) env)))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No binding for ~s" search-sym))
      (extend-env (bvars bvals saved-env)
                  (cond
                    ((location search-sym bvars)
                     => (lambda (n)
                          (list-ref bvals n)))
                    (else
                     (apply-env saved-env search-sym))))
      (extend-env-rec** (p-names b-varss p-bodies saved-env)
                        (cond 
                          ((location search-sym p-names)
                           => (lambda (n)
                                (newref
                                 (proc-val
                                  (procedure 
                                   (list-ref b-varss n)
                                   (list-ref p-bodies n)
                                   env)))))
                          (else (apply-env saved-env search-sym))))
      (extend-env-with-self-and-super (self super-name saved-env)
                                      (case search-sym
                                        ((%self) self)
                                        ((%super) super-name)
                                        (else (apply-env saved-env search-sym)))))))

;; location : Sym * Listof(Sym) -> Maybe(Int)
;; (location sym syms) returns the location of sym in syms or #f is
;; sym is not in syms.  We can specify this as follows:
;; if (memv sym syms)
;;   then (list-ref syms (location sym syms)) = sym
;;   else (location sym syms) = #f
(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms))
       => (lambda (n) 
            (+ n 1)))
      (else #f))))

