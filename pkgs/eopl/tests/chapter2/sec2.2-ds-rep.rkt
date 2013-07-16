#lang eopl
(require eopl/tests/private/utils)

;; Simple data structure representation of environments
;; Page: 38

;; data definition:
;; Env ::= (empty-env) | (extend-env Var Schemeval Env)

;; empty-env : () -> Env
(define empty-env
  (lambda () (list 'empty-env)))

;; extend-env : Var * Schemeval * Env -> Env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

;; apply-env : Env * Var -> Schemeval
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))

(check-equal? (apply-env e 'd) 6)
(check-equal? (apply-env e 'y) 8)
(check-equal? (apply-env e 'x) 7)

(check-error (apply-env (empty-env) 'x)
             "apply-env: No binding for x")

(check-error (apply-env '(env-extend x 5 (empty-env)) 'x)
             "apply-env: Bad environment: (env-extend x 5 (empty-env))")

;(report-unit-tests-completed 'apply-env)

