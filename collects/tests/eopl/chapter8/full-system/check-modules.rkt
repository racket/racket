#lang eopl

(require "lang.rkt")
(require "static-data-structures.rkt")
(require "expand-type.rkt")
(require "checker.rkt")
(require "renaming.rkt")
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
;; Page: 286, 305
(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (null? defns)
        tenv
        (cases module-definition (car defns)
          (a-module-definition (m-name expected-iface m-body)
                               (let ((actual-iface (interface-of m-body tenv)))
                                 (if (<:-iface actual-iface expected-iface tenv)
                                     ;; ok, continue in extended tenv
                                     (let ((new-env (extend-tenv-with-module
                                                     m-name
                                                     (expand-iface m-name expected-iface tenv)
                                                     tenv)))
                                       (add-module-defns-to-tenv (cdr defns) new-env))
                                     ;; no, raise error
                                     (report-module-doesnt-satisfy-iface m-name
                                                                         expected-iface actual-iface))))))))

;; interface-of : ModuleBody * Tenv -> Iface
;; Page: 322
(define interface-of
  (lambda (m-body tenv)
    (cases module-body m-body
      (var-module-body (m-name)
                       (lookup-module-name-in-tenv tenv m-name))
      (defns-module-body (defns)
        (simple-iface
         (defns-to-decls defns tenv)))
      (app-module-body (rator-id rand-id)
                       (let ((rator-iface (lookup-module-name-in-tenv tenv rator-id))
                             (rand-iface  (lookup-module-name-in-tenv tenv rand-id)))
                         (cases interface rator-iface
                           (simple-iface (decls)
                                         (eopl:error 'interface-of
                                                     "attempt to apply non-parameterized module ~s"
                                                     rator-id))
                           (proc-iface (param-name param-iface result-iface)
                                       (if (<:-iface
                                            rand-iface
                                            param-iface tenv)
                                           (rename-in-iface
                                            result-iface param-name rand-id)
                                           (raise-bad-module-application-error! param-iface
                                                                                rand-iface m-body)))
                           (else (eopl:error 'interface-of
                                             "unknown module type ~s"
                                             rator-iface))
                           )))
      (proc-module-body (rand-name rand-iface m-body)
                        ;; add the formal parameter to the tenv as if it had been a
                        ;; top-level module.
                        (let ((body-iface
                               (interface-of m-body
                                             (extend-tenv-with-module rand-name 
                                                                      (expand-iface rand-name rand-iface tenv)
                                                                      tenv))))
                          (proc-iface rand-name rand-iface body-iface)))
      )))

;; defns-to-decls : Listof(Defn) * Tenv -> Listof(Decl)
;; Page: 288, 305
;; Convert defns to a set of declarations for just the names defined
;; in defns.  Do this in the context of tenv.  The tenv is extended
;; at every step, so we get the correct let* scoping
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
                         (defns-to-decls (cdr defns) new-env)))))
          (type-defn (name ty)
                     (let ((new-env (extend-tenv-with-type
                                     name
                                     (expand-type ty tenv)
                                     tenv)))
                       (cons
                        (transparent-type-decl name ty)
                        (defns-to-decls (cdr defns) new-env))))))))

(define raise-bad-module-application-error!
  (lambda (expected-type rand-type body)
    (pretty-print
     (list 'bad-module-application body
           'actual-rand-interface: rand-type
           'expected-rand-interface: expected-type))
    (eopl:error 'interface-of
                "Bad module application ~s" body)))

(define report-module-doesnt-satisfy-iface
  (lambda (m-name expected-type actual-type)
    (pretty-print 
     (list 'error-in-defn-of-module: m-name
           'expected-type: expected-type
           'actual-type: actual-type))
    (eopl:error 'type-of-module-defn)))
