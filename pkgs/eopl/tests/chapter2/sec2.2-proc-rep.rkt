#lang eopl
(require eopl/tests/private/utils)

;; Simple procedural representation of environments
;; Page: 40

;; data definition:
;; Env = Var -> Schemeval

;; empty-env : () -> Env
(define empty-env
  (lambda ()
    (lambda (search-var) 
      (report-no-binding-found search-var))))

;; extend-env : Var * Schemeval * Env -> Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

;; apply-env : Env * Var -> Schemeval
(define apply-env
  (lambda (env search-var) 
    (env search-var)))

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

;(report-unit-tests-completed 'apply-env)



