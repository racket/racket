#lang eopl

(require "lang.rkt")

(provide type-of type-of-program)

;; check-equal-type! : Type * Type * Exp -> Unspecified
;; Page: 242
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (unless (equal? ty1 ty2)
      (report-unequal-types ty1 ty2 exp))))

;; report-unequal-types : Type * Type * Exp -> Unspecified
;; Page: 243
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!  
                "Types didn't match: ~s != ~a in~%~a"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                exp)))

;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;

;; type-of-program : Program -> Type
;; Page: 244
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1) (type-of exp1 (init-tenv))))))

;; type-of : Exp * Tenv -> Type
;; Page 244--246
(define type-of
  (lambda (exp tenv)
    (cases expression exp
      
      ;; \commentbox{\hastype{\tenv}{\mv{num}}{\mathtt{int}}}
      (const-exp (num) (int-type))
      
      ;; \commentbox{\hastype{\tenv}{\var{}}{\tenv{}(\var{})}}
      (var-exp (var) (apply-tenv tenv var))
      
      ;; \commentbox{\diffrule}
      (diff-exp (exp1 exp2)
                (let ((ty1 (type-of exp1 tenv))
                      (ty2 (type-of exp2 tenv)))
                  (check-equal-type! ty1 (int-type) exp1)
                  (check-equal-type! ty2 (int-type) exp2)
                  (int-type)))
      
      ;; \commentbox{\zerorule}
      (zero?-exp (exp1)
                 (let ((ty1 (type-of exp1 tenv)))
                   (check-equal-type! ty1 (int-type) exp1)
                   (bool-type)))
      
      ;; \commentbox{\condrule}
      (if-exp (exp1 exp2 exp3)
              (let ((ty1 (type-of exp1 tenv))
                    (ty2 (type-of exp2 tenv))
                    (ty3 (type-of exp3 tenv)))
                (check-equal-type! ty1 (bool-type) exp1)
                (check-equal-type! ty2 ty3 exp)
                ty2))
      
      ;; \commentbox{\letrule}
      (let-exp (var exp1 body)
               (let ((exp1-type (type-of exp1 tenv)))
                 (type-of body
                          (extend-tenv var exp1-type tenv))))
      
      ;; \commentbox{\procrulechurch}
      (proc-exp (var var-type body)
                (let ((result-type
                       (type-of body
                                (extend-tenv var var-type tenv))))
                  (proc-type var-type result-type)))
      
      ;; \commentbox{\apprule}
      (call-exp (rator rand) 
                (let ((rator-type (type-of rator tenv))
                      (rand-type  (type-of rand tenv)))
                  (cases type rator-type
                    (proc-type (arg-type result-type)
                               (begin
                                 (check-equal-type! arg-type rand-type rand)
                                 result-type))
                    (else
                     (report-rator-not-a-proc-type rator-type rator)))))
      
      ;; \commentbox{\letrecrule}
      (letrec-exp (p-result-type p-name b-var b-var-type p-body
                                 letrec-body)
                  (let ((tenv-for-letrec-body
                         (extend-tenv p-name
                                      (proc-type b-var-type p-result-type)
                                      tenv)))
                    (let ((p-body-type 
                           (type-of p-body
                                    (extend-tenv b-var b-var-type
                                                 tenv-for-letrec-body)))) 
                      (check-equal-type!
                       p-body-type p-result-type p-body)
                      (type-of letrec-body tenv-for-letrec-body)))))))

(define report-rator-not-a-proc-type
  (lambda (rator-type rator)
    (eopl:error 'type-of-expression
                "Rator not a proc type:~%~s~%had rator type ~s"   
                rator 
                (type-to-external-form rator-type))))

;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
   (sym symbol?)
   (type type?)
   (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
                         (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (sym1 val1 old-env)
                            (if (eqv? sym sym1) 
                                val1
                                (apply-tenv old-env sym))))))

(define init-tenv
  (lambda ()
    (extend-tenv 'x (int-type) 
                 (extend-tenv 'v (int-type)
                              (extend-tenv 'i (int-type)
                                           (empty-tenv))))))

