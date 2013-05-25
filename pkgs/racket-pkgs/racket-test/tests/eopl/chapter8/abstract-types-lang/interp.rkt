#lang eopl

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> Expval
;; Page: 284
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (module-defs body)
                 (let ((env 
                        (add-module-defns-to-env module-defs (empty-env))))
                   ;; (eopl:pretty-print env)
                   (value-of body env))))))

;; add-module-defns-to-env : Listof(Defn) * Env -> Env
;; Page: 284
(define add-module-defns-to-env
  (lambda (defs env)
    (if (null? defs)
        env
        (cases module-definition (car defs)
          (a-module-definition (m-name iface m-body)
                               (add-module-defns-to-env
                                (cdr defs)
                                (extend-env-with-module
                                 m-name 
                                 (value-of-module-body m-body env)
                                 env)))))))

;; We will have let* scoping inside a module body.
;; We put all the values in the environment, not just the ones
;; that are in the interface.  But the typechecker will prevent
;; anybody from using the extras.

;; value-of-module-body : ModuleBody * Env -> TypedModule
;; Page: 285, 320
(define value-of-module-body
  (lambda (m-body env)    
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-module
         (defns-to-env defns env))) )))


(define raise-cant-apply-non-proc-module!
  (lambda (rator-val)
    (eopl:error 'value-of-module-body 
                "can't apply non-proc-module-value ~s" rator-val)))

;; defns-to-env : Listof(Defn) * Env -> Env
;; Page: 285, 303
(define defns-to-env
  (lambda (defns env)
    (if (null? defns)
        (empty-env)                ; we're making a little environment
        (cases definition (car defns)
          (val-defn (var exp)
                    (let ((val (value-of exp env)))
                      ;; new environment for subsequent definitions
                      (let ((new-env (extend-env var val env)))
                        (extend-env var val
                                    (defns-to-env
                                      (cdr defns) new-env)))))
          ;; type definitions are ignored at run time
          (else
           (defns-to-env (cdr defns) env))
          ))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    
    (cases expression exp
      
      (const-exp (num) (num-val num))
      
      (var-exp (var) (apply-env env var))
      
      (qualified-var-exp (m-name var-name)
                         (lookup-qualified-var-in-env m-name var-name env))
      
      (diff-exp (exp1 exp2)
                (let ((val1
                       (expval->num
                        (value-of exp1 env)))
                      (val2
                       (expval->num
                        (value-of exp2 env))))
                  (num-val
                   (- val1 val2))))
      
      (zero?-exp (exp1)
                 (let ((val1 (expval->num (value-of exp1 env))))
                   (if (zero? val1)
                       (bool-val #t)
                       (bool-val #f))))
      
      (if-exp (exp0 exp1 exp2) 
              (if (expval->bool (value-of exp0 env))
                  (value-of exp1 env)
                  (value-of exp2 env)))
      
      (let-exp (var exp1 body)       
               (let ((val (value-of exp1 env)))
                 (let ((new-env (extend-env var val env)))
                   ;; (eopl:pretty-print new-env)
                   (value-of body new-env))))
      
      (proc-exp (bvar ty body)
                (proc-val
                 (procedure bvar body env)))
      
      (call-exp (rator rand)          
                (let ((proc (expval->proc (value-of rator env)))
                      (arg  (value-of rand env)))
                  (apply-procedure proc arg)))
      
      (letrec-exp (ty1 proc-name bvar ty2 proc-body letrec-body)
                  (value-of letrec-body
                            (extend-env-recursively proc-name bvar proc-body env)))
      
      )))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var arg saved-env))))))


