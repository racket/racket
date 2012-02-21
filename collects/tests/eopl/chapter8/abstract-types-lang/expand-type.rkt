#lang eopl

(require "lang.rkt")
(require "static-data-structures.rkt")
; (require "renaming.scm")              ; for fresh-module-name

(provide expand-type)
(provide expand-iface)

;;;;;;;;;;;;;;;; expand-type ;;;;;;;;;;;;;;;;

;; expand-type expands a type so that it contains no type
;; abbreviations. 

;; For example, if tenv contains a declaration for a module

;;   module m1
;;    interface
;;     [abstract-type t
;;      type-abbrev u = int
;;      type-abbrev v = (t -> u)]

;; then calling expand-type on from m1 take v should return
;; (from m1 take t -> int)  

;; this relies on the invariant that every type returned by
;; lookup-type-name-in-tenv is already expanded.


;; expand-type : Type * Tenv -> ExpandedType
(define expand-type
  (lambda (ty tenv)
    (cases type ty
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (arg-type result-type)
                 (proc-type
                  (expand-type arg-type tenv)
                  (expand-type result-type tenv)))
      (named-type (name)
                  (lookup-type-name-in-tenv tenv name))
      (qualified-type (m-name t-name)
                      (lookup-qualified-type-in-tenv m-name t-name tenv))
      )))


;; creates new interface with all types expanded
;; expand-iface : Sym * Iface * Tenv -> Iface
;; Page: 307
(define expand-iface
  (lambda (m-name iface tenv)
    (cases interface iface
      (simple-iface (decls) 
                    (simple-iface
                     (expand-decls m-name decls tenv))) )))


;; like defns->decls, this creates only transparent type
;; declarations. 

;; expand-decls : Sym * Listof(Decl) * Tenv -> Listof(Decl)
;; Page: 307
(define expand-decls
  (lambda (m-name decls internal-tenv) 
    (if (null? decls) '()
        (cases declaration (car decls)
          (opaque-type-decl (t-name)
                            ;; here the expanded type is m.t
                            (let ((expanded-type (qualified-type m-name t-name)))
                              (let ((new-env (extend-tenv-with-type
                                              t-name
                                              expanded-type
                                              internal-tenv)))
                                (cons 
                                 (transparent-type-decl t-name expanded-type)
                                 (expand-decls m-name (cdr decls) new-env)))))
          (transparent-type-decl (t-name ty)
                                 (let ((expanded-type (expand-type ty internal-tenv)))
                                   (let ((new-env (extend-tenv-with-type 
                                                   t-name
                                                   expanded-type
                                                   internal-tenv)))
                                     (cons
                                      (transparent-type-decl t-name expanded-type)
                                      (expand-decls m-name (cdr decls) new-env)))))
          (val-decl (var-name ty)
                    (let ((expanded-type
                           (expand-type ty internal-tenv)))
                      (cons
                       (val-decl var-name expanded-type)
                       (expand-decls m-name (cdr decls) internal-tenv))))))))


