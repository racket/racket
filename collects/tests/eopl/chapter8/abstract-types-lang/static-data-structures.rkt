#lang eopl

(require "lang.rkt")                  ; for expression?, type?, etc.

(provide (all-defined-out))           ; too many things to list

(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv 
   (bvar symbol?)
   (bval type?)
   (saved-tenv type-environment?))
  (extend-tenv-with-module
   (name symbol?)
   (interface interface?)
   (saved-tenv type-environment?))
  (extend-tenv-with-type
   (t-name symbol?)
   (t-type type?)                    ; invariant: this must always
   ; be expanded
   (saved-tenv type-environment?))
  )

;;;;;;;;;;;;;;;; procedures for looking things up tenvs ;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; lookup or die

;; lookup-qualified-var-in-tenv : Sym * Sym * Tenv -> Type
;; Page: 285
(define lookup-qualified-var-in-tenv
  (lambda (m-name var-name tenv)
    (let ((iface (lookup-module-name-in-tenv tenv m-name)))
      (cases interface iface
        (simple-iface (decls)
                      (lookup-variable-name-in-decls var-name decls)) ))))

(define lookup-variable-name-in-tenv
  (lambda (tenv search-sym)
    (let ((maybe-answer
           (variable-name->maybe-binding-in-tenv tenv search-sym)))
      (if maybe-answer maybe-answer
          (raise-tenv-lookup-failure-error 'variable search-sym tenv)))))

(define lookup-module-name-in-tenv
  (lambda (tenv search-sym)
    (let ((maybe-answer
           (module-name->maybe-binding-in-tenv tenv search-sym)))
      (if maybe-answer maybe-answer
          (raise-tenv-lookup-failure-error 'module search-sym tenv)))))

(define lookup-type-name-in-tenv
  (lambda (tenv search-sym)
    (let ((maybe-answer
           (type-name->maybe-binding-in-tenv tenv search-sym)))
      (if maybe-answer maybe-answer
          (raise-tenv-lookup-failure-error 'type search-sym tenv)))))

(define lookup-qualified-type-in-tenv
  (lambda (m-name t-name tenv)
    (let ((iface (lookup-module-name-in-tenv tenv m-name)))
      (cases interface iface
        (simple-iface (decls)
                      ;; this is not right, because it doesn't distinguish
                      ;; between type and variable declarations.  Exercise: fix
                      ;; this so that it raises an error if t-name is declared
                      ;; in a val-decl.
                      (lookup-variable-name-in-decls t-name decls))
        ))))

(define apply-tenv lookup-variable-name-in-tenv)

(define raise-tenv-lookup-failure-error
  (lambda (kind var tenv)
    (eopl:pretty-print
     (list 'tenv-lookup-failure: (list 'missing: kind var) 'in:
           tenv))
    (eopl:error 'lookup-variable-name-in-tenv)))


;; this is not right, because it doesn't distinguish
;; between type and variable declarations.  But it will do
;; for now.  Exercise: refine this do that it raises an error if
;; var-name is declared as something other than a val-decl.

(define lookup-variable-name-in-decls
  (lambda (var-name decls0)
    (let loop ((decls decls0))
      (cond
        ((null? decls)
         (raise-lookup-variable-in-decls-error! var-name decls0))
        ((eqv? var-name (decl->name (car decls)))
         (decl->type (car decls)))
        (else (loop (cdr decls)))))))

(define raise-lookup-variable-in-decls-error!
  (lambda (var-name decls)
    (eopl:pretty-print
     (list 'lookup-variable-decls-failure:
           (list 'missing-variable var-name)
           'in:
           decls))))

;;;;;;;;;;;;;;;; lookup or return #f.

;; variable-name->maybe-binding-in-tenv : Tenv * Sym -> Maybe(Type)
(define variable-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let recur ((tenv tenv)) 
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv (name ty saved-tenv)
                     (if (eqv? name search-sym) 
                         ty
                         (recur saved-tenv)))
        (else (recur (tenv->saved-tenv tenv)))))))

;; module-name->maybe-binding-in-tenv : Tenv * Sym -> Maybe(Iface)
(define module-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let recur ((tenv tenv)) 
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv-with-module (name m-type saved-tenv)
                                 (if (eqv? name search-sym) 
                                     m-type
                                     (recur saved-tenv)))
        (else (recur (tenv->saved-tenv tenv)))))))

;; type-name->maybe-binding-in-tenv : Tenv * Sym -> Maybe(Iface)
(define type-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let recur ((tenv tenv)) 
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv-with-type (name type saved-tenv)
                               (if (eqv? name search-sym) 
                                   type
                                   (recur saved-tenv)))
        (else (recur (tenv->saved-tenv tenv)))))))

;; assumes tenv is non-empty.  
(define tenv->saved-tenv
  (lambda (tenv)
    (cases type-environment tenv
      (empty-tenv () 
                  (eopl:error 'tenv->saved-tenv
                              "tenv->saved-tenv called on empty tenv"))
      (extend-tenv (name ty saved-tenv) saved-tenv)
      (extend-tenv-with-module (name m-type saved-tenv) saved-tenv)
      (extend-tenv-with-type (name ty saved-tenv) saved-tenv)
      )))

