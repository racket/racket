#lang eopl

(require "lang.rkt")
(require "static-data-structures.rkt")
(require "checker.rkt")
(require "subtyping.rkt")

(require (only-in racket pretty-print))

(provide type-of-program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; type-of-program : Program -> Type
;; Page: 286
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (module-defs body)
                 (type-of body
                          (add-module-defns-to-tenv module-defs (empty-tenv)))))))

;; add-module-defns-to-tenv : Listof(ModuleDefn) * Tenv -> Tenv
;; Page: 286
(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (null? defns)
        tenv
        (cases module-definition (car defns)
          (a-module-definition (m-name expected-iface m-body)
                               (let ((actual-iface (interface-of m-body tenv)))
                                 (if (<:-iface actual-iface expected-iface tenv)
                                     (let ((new-tenv
                                            (extend-tenv-with-module
                                             m-name
                                             expected-iface
                                             tenv)))
                                       (add-module-defns-to-tenv
                                        (cdr defns) new-tenv))
                                     (report-module-doesnt-satisfy-iface
                                      m-name expected-iface actual-iface))))))))

;; interface-of : ModuleBody * Tenv -> Iface
;; Page: 288
(define interface-of
  (lambda (m-body tenv)
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-iface
         (defns-to-decls defns tenv))) )))

;; defns-to-decls : Listof(Defn) * Tenv -> Listof(Decl)
;; Page: 288
;; 
;; Convert defns to a set of declarations for just the names defined
;; in defns.  Do this in the context of tenv.  The tenv is extended
;; at every step, so we get the correct let* scoping
;;
(define defns-to-decls
  (lambda (defns tenv)
    (if (null? defns)
        '()
        (cases definition (car defns)
          (val-defn (var-name exp)
                    (let ((ty (type-of exp tenv)))
                      (let ((new-env (extend-tenv var-name ty tenv)))
                        (cons
                         (val-decl var-name ty)
                         (defns-to-decls (cdr defns) new-env)))))))))

(define report-module-doesnt-satisfy-iface
  (lambda (m-name expected-type actual-type)
    (pretty-print 
     (list 'error-in-defn-of-module: m-name
           'expected-type: expected-type
           'actual-type: actual-type))
    (eopl:error 'type-of-module-defn)))
