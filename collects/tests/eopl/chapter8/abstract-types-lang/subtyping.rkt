#lang eopl

(require "lang.rkt")
(require "static-data-structures.rkt")
(require "expand-type.rkt")
(require "renaming.rkt")

(provide <:-iface)

;; <:-iface : Iface * Iface * Tenv -> Bool
;; Page: 289
(define <:-iface
  (lambda (iface1 iface2 tenv)
    (cases interface iface1
      (simple-iface (decls1)
                    (cases interface iface2
                      (simple-iface (decls2)
                                    (<:-decls decls1 decls2 tenv)))))))

;; s1 <: s2 iff s1 has at least as much stuff as s2, and in the same
;; order.  We walk down s1 until we find a declaration that declares
;; the same name as the first component of s2.  If we run off the
;; end of s1, then we fail.  As we walk down s1, we record any type
;; bindings in the tenv

;; <:-decls : Listof(Decl) * Listof(Decl) * Tenv -> Bool
;; Page: 289, 305
(define <:-decls
  (lambda (decls1 decls2 tenv)
    (cond
      ;; if nothing in decls2, any decls1 will do
      ((null? decls2) #t)
      ;; nothing in decls1 to match, so false
      ((null? decls1) #f)
      (else
       ;; at this point we know both decls1 and decls2 are non-empty.
       (let ((name1 (decl->name (car decls1)))
             (name2 (decl->name (car decls2))))
         (if (eqv? name1 name2)
             ;; same name.  They'd better match
             (and
              (<:-decl (car decls1) (car decls2) tenv)
              (<:-decls (cdr decls1) (cdr decls2)
                        (extend-tenv-with-decl (car decls1) tenv)))
             ;; different names.  OK to skip, but record decl1 in the tenv. 
             (<:-decls (cdr decls1) decls2
                       (extend-tenv-with-decl (car decls1) tenv))))))))

;; extend-tenv-with-decl : Decl * Tenv -> Tenv
;; Page: 309
(define extend-tenv-with-decl
  (lambda (decl tenv)
    (cases declaration decl
      ;; don't need to record val declarations
      (val-decl (name ty) tenv)
      (transparent-type-decl (name ty)
                             (extend-tenv-with-type
                              name
                              (expand-type ty tenv)
                              tenv))
      (opaque-type-decl (name)
                        (extend-tenv-with-type
                         name
                         ;; the module name doesn't matter, since the only
                         ;; operation on qualified types is equal?
                         (qualified-type (fresh-module-name '%abstype) name)
                         tenv)))))

;; decl1 and decl2 are known to declare the same name.  There are
;; exactly four combinations that can succeed.

;; <:-decl : Decl * Decl * Tenv -> Bool
;; Page: 311
(define <:-decl 
  (lambda (decl1 decl2 tenv)
    (or
     (and
      (val-decl? decl1)
      (val-decl? decl2)
      (equiv-type? (decl->type decl1) (decl->type decl2) tenv))
     (and
      (transparent-type-decl? decl1)
      (transparent-type-decl? decl2)
      (equiv-type? (decl->type decl1) (decl->type decl2) tenv))
     (and 
      (transparent-type-decl? decl1)
      (opaque-type-decl? decl2))
     (and
      (opaque-type-decl? decl1)
      (opaque-type-decl? decl2))
     )))

;; equiv-type? : Type * Type * Tenv -> Bool
;; Page: 311
(define equiv-type?
  (lambda (ty1 ty2 tenv)
    (equal?
     (expand-type ty1 tenv)
     (expand-type ty2 tenv))))
