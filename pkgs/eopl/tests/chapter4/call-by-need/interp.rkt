#lang eopl

;; interpreter for the CALL-BY-NEED language.  

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")
(require "store.rkt")
(require "pairvals.rkt")

(provide value-of-program value-of instrument-newref)

;;; exercise: add instrumentation around let and procedure calls, as
;;; in the call-by-reference language.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (initialize-store!)             
    (cases program pgm
      (a-program (body)
                 (value-of body (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 137 and 138
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))
      
      (var-exp (var) 
               (let ((ref1 (apply-env env var)))
                 (let ((w (deref ref1)))
                   (if (expval? w)
                       w
                       (let ((v1 (value-of-thunk w)))
                         (begin
                           (setref! ref1 v1)   
                           v1))))))
      
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
                 (value-of body
                           (extend-env var (newref val) env))))
      
      (proc-exp (var body)
                (proc-val
                 (procedure var body env)))
      
      (call-exp (rator rand)          
                (let ((proc (expval->proc (value-of rator env)))
                      (arg  (value-of-operand rand env)))
                  (apply-procedure proc arg)))
      
      (letrec-exp (p-names b-vars p-bodies letrec-body)
                  (value-of letrec-body
                            (extend-env-rec* p-names b-vars p-bodies env)))
      
      (begin-exp (exp1 exps)
                 (letrec 
                     ((value-of-begins
                       (lambda (e1 es)
                         (let ((v1 (value-of e1 env)))
                           (if (null? es)
                               v1
                               (value-of-begins (car es) (cdr es)))))))
                   (value-of-begins exp1 exps)))
      
      (assign-exp (x e)
                  (begin
                    (setref!
                     (apply-env env x)
                     (value-of e env))
                    (num-val 27)))
      
      (newpair-exp (exp1 exp2)
                   (let ((v1 (value-of exp1 env))
                         (v2 (value-of exp2 env)))
                     (mutpair-val (make-pair v1 v2))))
      
      (left-exp (exp1)
                (let ((v1 (value-of exp1 env)))
                  (let ((p1 (expval->mutpair v1)))
                    (left p1))))
      
      (setleft-exp (exp1 exp2)
                   (let ((v1 (value-of exp1 env))
                         (v2 (value-of exp2 env)))
                     (let ((p (expval->mutpair v1)))
                       (begin
                         (setleft p v2)
                         (num-val 82)))))
      
      (right-exp (exp1)
                 (let ((v1 (value-of exp1 env)))
                   (let ((p1 (expval->mutpair v1)))
                     (right p1))))
      
      (setright-exp (exp1 exp2)
                    (let ((v1 (value-of exp1 env))
                          (v2 (value-of exp2 env)))
                      (let ((p (expval->mutpair v1)))
                        (begin
                          (setright p v2)
                          (num-val 83)))))
      
      )))

;; apply-procedure : Proc * Ref -> ExpVal
(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body
                           (extend-env var arg saved-env))))))

;; value-of-operand : Exp * Env -> Ref
;; Page: 137
(define value-of-operand
  (lambda (exp env)
    (cases expression exp
      (var-exp (var) (apply-env env var)) ; no deref!
      (else
       (newref (a-thunk exp env))))))

;; value-of-thunk : Thunk -> ExpVal
(define value-of-thunk
  (lambda (th)
    (cases thunk th
      (a-thunk (exp1 saved-env)
               (value-of exp1 saved-env)))))




