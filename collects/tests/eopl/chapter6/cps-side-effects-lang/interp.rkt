#lang eopl

(require "cps-out-lang.rkt")
(require "data-structures.rkt")
(require "store.rkt")

(provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (initialize-store!)
    (cases cps-out-program pgm
      (cps-a-program (body)
                     (value-of/k body (init-env) (end-cont))))))

(define value-of-simple-exp
  (lambda (exp env)
    
    (cases simple-expression exp
      (cps-const-exp (num) (num-val num))
      (cps-var-exp (var) (apply-env env var))
      (cps-diff-exp (exp1 exp2)
                    (let ((val1
                           (expval->num
                            (value-of-simple-exp exp1 env)))
                          (val2
                           (expval->num
                            (value-of-simple-exp exp2 env))))
                      (num-val
                       (- val1 val2))))
      (cps-zero?-exp (exp1)
                     (bool-val
                      (zero?
                       (expval->num
                        (value-of-simple-exp exp1 env)))))
      (cps-sum-exp (exps)
                   (let ((nums (map
                                (lambda (exp)
                                  (expval->num
                                   (value-of-simple-exp exp env)))
                                exps)))
                     (num-val
                      (let sum-loop ((nums nums))
                        (if (null? nums) 0
                            (+ (car nums) (sum-loop (cdr nums))))))))
      (cps-proc-exp (vars body)
                    (proc-val
                     (procedure vars body env)))
      )))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 228 and 230-231
(define value-of/k
  (lambda (exp env cont)
    (cases tfexp exp
      (simple-exp->exp (bexp)
                       (apply-cont cont
                                   (value-of-simple-exp bexp env)))
      (cps-let-exp (var exp1 body)
                   (let ((val (value-of-simple-exp exp1 env)))
                     (value-of/k body
                                 (extend-env* (list var) (list val) env)
                                 cont)))
      (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
                      (value-of/k letrec-body
                                  (extend-env-rec** p-names b-varss p-bodies env)
                                  cont))
      (cps-if-exp (exp1 exp2 exp3)
                  (value-of/k
                   (if (expval->bool (value-of-simple-exp exp1 env))
                       exp2
                       exp3)
                   env
                   cont))
      (cps-call-exp (rator rands)
                    (let ((rator-proc (expval->proc (value-of-simple-exp rator env)))
                          (rand-vals (map 
                                      (lambda (bexp) (value-of-simple-exp bexp
                                                                          env))
                                      rands)))
                      (apply-procedure/k rator-proc rand-vals cont)))
      
      (cps-printk-exp (simple body)
                      (begin
                        (eopl:printf "~s~%" (value-of-simple-exp simple env))
                        (value-of/k body env cont)))
      
      (cps-newrefk-exp (simple1 simple2)
                       (let ((val1 (value-of-simple-exp simple1 env))
                             (val2 (value-of-simple-exp simple2 env)))
                         (let ((newval (ref-val (newref val1))))
                           (apply-procedure/k
                            (expval->proc val2)
                            (list newval)
                            cont))))
      
      (cps-derefk-exp (simple1 simple2)
                      (apply-procedure/k
                       (expval->proc (value-of-simple-exp simple2 env))
                       (list
                        (deref
                         (expval->ref
                          (value-of-simple-exp simple1 env))))
                       cont))
      
      (cps-setrefk-exp (simple1 simple2 body)
                       (let ((ref (expval->ref (value-of-simple-exp simple1 env)))
                             (val (value-of-simple-exp simple2 env)))
                         (begin
                           (setref! ref val)
                           (value-of/k body env cont))))
      )))

;; apply-cont : Cont * ExpVal -> Final-ExpVal
;; there's only one continuation, and it only gets invoked once, at
;; the end of the computation.
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont () val))))      

;; apply-procedure/k : Proc * ExpVal * Cont -> ExpVal
;; Page: 209 
(define apply-procedure/k
  (lambda (proc1 args cont)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (value-of/k body
                             (extend-env* vars args saved-env)
                             cont)))))

