#lang eopl

;; imperative cps interpreter for the LETREC language, using the
;; data structure representation of continuations (Figure 5.3)

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of/k)

(provide trace-apply-procedure instrument-end)

(define trace-apply-procedure (make-parameter #f))
(define instrument-end (make-parameter #f))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;;; have the interpreter procedures communicate via registers

(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define proc1 'uninitialized)         ; we've already used "proc".

;; value-of-program : Program -> FinalAnswer
;; Page: 167
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (set! cont (end-cont))
                 (set! exp body)
                 (set! env (init-env))
                 (value-of/k)))))

;; value-of : Exp * Env * Cont -> FinalAnswer
;; value-of/k : () -> FinalAnswer
;; usage : relies on registers 
;;      exp  : Exp
;;      env  : Env
;;      cont : Cont
;; Page 167 and 168
;;
;; The code from the corresponding portions of interp.scm is shown
;; as comments.
(define value-of/k
  (lambda ()                         
    (cases expression exp
      (const-exp (num)
                 ;; (apply-cont cont (num-val num)))                   
                 (set! val (num-val num))
                 ;; cont is unchanged          
                 (apply-cont))
      (var-exp (var)
               ;; (apply-cont cont (apply-env env id)))                 
               (set! val (apply-env env var))
               ;; cont is unchanged          
               (apply-cont))
      (proc-exp (var body)
                ;; (apply-cont cont (proc-val (procedure bvar body env))
                (set! val (proc-val (procedure var body env)))
                (apply-cont))
      (letrec-exp (p-name b-var p-body letrec-body)
                  ;; (value-of/k letrec-body
                  ;;   (extend-env-rec proc-name bvar proc-body env)
                  ;;   cont)
                  (set! exp letrec-body)
                  (set! env
                        (extend-env-rec p-name b-var p-body env))
                  (value-of/k))
      (zero?-exp (exp1)
                 ;; (value-of/k exp1 env (zero1-cont cont))
                 (set! cont (zero1-cont cont))
                 (set! exp exp1)
                 (value-of/k))
      (let-exp (var exp1 body) 
               ;; (value-of/k rhs env (let-exp-cont id body env cont)) 
               (set! cont (let-exp-cont var body env cont))
               (set! exp exp1)
               (value-of/k))
      (if-exp (exp1 exp2 exp3)
              ;; (value-of/k exp0 env (if-test-cont exp2 exp3 env cont))
              (set! cont (if-test-cont exp2 exp3 env cont))
              (set! exp exp1)
              (value-of/k))
      (diff-exp (exp1 exp2)
                ;; (value-of/k exp1 env (diff1-cont exp2 env cont))              
                (set! cont (diff1-cont exp2 env cont))
                (set! exp exp1)
                ;; env is unchanged          
                (value-of/k))
      (call-exp (rator rand)
                ;; (value-of/k rator env (rator-cont rand env cont))
                (set! cont (rator-cont rand env cont))
                (set! exp rator)
                (value-of/k))
      )))

;; apply-cont : Cont * ExpVal -> FinalAnswer
;; usage : reads registers
;;     cont : Cont
;;     val  : ExpVal
;; Page 169 and 170
(define apply-cont
  (lambda ()                          
    (cases continuation cont
      
      (end-cont ()
                (when (instrument-end)
                  (eopl:printf "End of computation.~%"))
                val)
      ;; or (logged-print val)  ; if you use drscheme-init-cps.scm
      (zero1-cont (saved-cont)
                  ;; (apply-cont cont
                  ;;   (bool-val
                  ;;     (zero? (expval->num val))))
                  (set! cont saved-cont)
                  (set! val (bool-val (zero? (expval->num val))))
                  (apply-cont))
      (let-exp-cont (var body saved-env saved-cont)
                    ;; (value-of/k body (extend-env id val env) cont)                     
                    (set! cont saved-cont)
                    (set! exp body)
                    (set! env (extend-env var val saved-env))
                    (value-of/k))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
                    (set! cont saved-cont)
                    (if (expval->bool val)
                        (set! exp exp2)
                        (set! exp exp3))
                    (set! env saved-env)
                    (value-of/k))
      (diff1-cont (exp2 saved-env saved-cont)
                  ;; (value-of/k exp2 env (diff2-cont val cont)))
                  (set! cont (diff2-cont val saved-cont))
                  (set! exp exp2)
                  (set! env saved-env)
                  (value-of/k))
      (diff2-cont (val1 saved-cont)
                  ;; (apply-cont cont (num-val (- num1 num2)))))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val)))
                    (set! cont saved-cont)
                    (set! val (num-val (- num1 num2)))
                    (apply-cont)))
      (rator-cont (rand saved-env saved-cont)
                  ;; (value-of/k rand env (rand-cont val cont))
                  (set! cont (rand-cont val saved-cont))
                  (set! exp rand)
                  (set! env saved-env)
                  (value-of/k))
      (rand-cont (rator-val saved-cont)
                 (let ((rator-proc (expval->proc rator-val)))
                   ;; (apply-procedure rator-proc rator-val cont)
                   (set! cont saved-cont)
                   (set! proc1 rator-proc)
                   (set! val val)
                   (apply-procedure/k)))
      )))

;; apply-procedure : Proc * ExpVal -> ExpVal
;; apply-procedure/k : () -> FinalAnswer}
;; usage : relies on registers
;;     proc1 : Proc
;;       val : ExpVal
;;      cont : Cont
;; Page 170
(define apply-procedure/k
  (lambda ()                          
    (cases proc proc1
      (procedure (var body saved-env)
                 (set! exp body)
                 (set! env (extend-env var val saved-env))
                 (value-of/k)))))

;; instrumented version
;; (define apply-procedure/k
;;   (lambda ()                       ; (proc1 val cont)
;;     (if (trace-apply-procedure)
;;       (begin
;;         (eopl:printf
;;           "~%entering apply-procedure:~%proc1=~s~%val=~s~%cont=~s~%" 
;;           proc1 val cont)))
;;     (cases proc proc1
;;       (procedure (var body saved-env)
;;         (set! exp body)
;;         (set! env (extend-env var val saved-env))
;;         (value-of/k)))))


