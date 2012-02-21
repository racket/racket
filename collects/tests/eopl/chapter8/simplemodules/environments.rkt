#lang eopl

(require "data-structures.rkt")
(require "lang.rkt")

(provide empty-env extend-env apply-env)
(provide lookup-module-name-in-env)
(provide lookup-qualified-var-in-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; initial-value-env : module-env -> environment

;; (init-env m-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10, and in which m-env is the module
;; environment.

(define inital-value-env 
  (lambda (m-env)
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env m-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

;; for variables bound by extend-env or extend-env-recursively

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No value binding for ~s" search-sym))
      (extend-env (bvar bval saved-env)
                  (if (eqv? search-sym bvar)
                      bval
                      (apply-env saved-env search-sym)))
      (extend-env-recursively
       (id bvar body saved-env)
       (if (eqv? search-sym id)
           (proc-val (procedure bvar body env))          
           (apply-env saved-env search-sym)))
      (extend-env-with-module
       (m-name m-val saved-env)
       (apply-env saved-env search-sym)) )))

;; for names bound by extend-env-with-module

;; lookup-module-name-in-env : Sym * Env -> Typed-Module
(define lookup-module-name-in-env
  (lambda (m-name env)            
    (cases environment env
      (empty-env ()
                 (eopl:error 'lookup-module-name-in-env
                             "No module binding for ~s" m-name))
      (extend-env (bvar bval saved-env)
                  (lookup-module-name-in-env m-name saved-env))
      (extend-env-recursively  (id bvar body saved-env)
                               (lookup-module-name-in-env m-name saved-env))
      (extend-env-with-module
       (m-name1 m-val saved-env)
       (if (eqv? m-name1 m-name)
           m-val
           (lookup-module-name-in-env m-name saved-env))))))

;; lookup-qualified-var-in-env : Sym * Sym * Env -> ExpVal
;; Page: 283
(define lookup-qualified-var-in-env
  (lambda (m-name var-name env)
    (let ((m-val (lookup-module-name-in-env m-name env)))
      ; (pretty-print m-val)
      (cases typed-module m-val
        (simple-module (bindings)
                       (apply-env bindings var-name))
        (proc-module (bvar body saved-env)
                     (eopl:error 'lookup-qualified-var
                                 "can't retrieve variable from ~s take ~s from proc module"
                                 m-name var-name))))))
