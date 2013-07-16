#lang eopl

(require "lang.rkt")
(require "static-data-structures.rkt")
(require "expand-type.rkt")

(provide type-of)

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

;; moved to check-modules.scm
;; type-of-program : Program -> Type
;; Page: 244 
;;   (define type-of-program
;;     (lambda (pgm)
;;       (cases program pgm
;;         (a-program (exp1) 
;;           (type-of exp1 (init-tenv))))))


;; type-of : Exp * Tenv -> Type
;; Page 244--246.  See also page 285.
(define type-of
  (lambda (exp tenv)
    (cases expression exp
      (const-exp (num) (int-type))
      
      (diff-exp (exp1 exp2)
                (let ((type1 (type-of exp1 tenv))
                      (type2 (type-of exp2 tenv)))
                  (check-equal-type! type1 (int-type) exp1)
                  (check-equal-type! type2 (int-type) exp2)
                  (int-type)))
      
      (zero?-exp (exp1)
                 (let ((type1 (type-of exp1 tenv)))
                   (check-equal-type! type1 (int-type) exp1)
                   (bool-type)))
      
      (if-exp (exp1 exp2 exp3)
              (let ((ty1 (type-of exp1 tenv))
                    (ty2 (type-of exp2 tenv))
                    (ty3 (type-of exp3 tenv)))
                (check-equal-type! ty1 (bool-type) exp1)
                (check-equal-type! ty2 ty3 exp)
                ty2))
      
      (var-exp (var) (apply-tenv tenv var))
      
      ;; lookup-qualified-var-in-tenv defined on page 285.
      (qualified-var-exp (m-name var-name) 
                         (lookup-qualified-var-in-tenv m-name var-name tenv))
      
      (let-exp (var exp1 body)
               (let ((rhs-type (type-of exp1 tenv)))
                 (type-of body (extend-tenv var rhs-type tenv))))
      
      (proc-exp (bvar bvar-type body)
                (let ((expanded-bvar-type
                       (expand-type bvar-type tenv)))
                  (let ((result-type
                         (type-of body
                                  (extend-tenv
                                   bvar
                                   expanded-bvar-type
                                   tenv))))
                    (proc-type expanded-bvar-type result-type))))
      
      (call-exp (rator rand) 
                (let ((rator-type (type-of rator tenv))
                      (rand-type  (type-of rand tenv)))
                  (cases type rator-type
                    (proc-type (arg-type result-type)
                               (begin
                                 (check-equal-type! arg-type rand-type rand)
                                 result-type))
                    (else
                     (eopl:error 'type-of
                                 "Rator not a proc type:~%~s~%had rator type ~s"   
                                 rator (type-to-external-form rator-type))))))
      
      (letrec-exp (proc-result-type proc-name 
                                    bvar bvar-type 
                                    proc-body
                                    letrec-body)
                  (let ((tenv-for-letrec-body
                         (extend-tenv 
                          proc-name
                          (expand-type
                           (proc-type bvar-type proc-result-type)
                           tenv)
                          tenv)))
                    (let ((proc-result-type
                           (expand-type proc-result-type tenv))
                          (proc-body-type
                           (type-of proc-body
                                    (extend-tenv
                                     bvar
                                     (expand-type bvar-type tenv)
                                     tenv-for-letrec-body))))
                      (check-equal-type!
                       proc-body-type proc-result-type proc-body)
                      (type-of letrec-body tenv-for-letrec-body))))
      
      )))

;; type environments are now in static-data-structures.scm .
