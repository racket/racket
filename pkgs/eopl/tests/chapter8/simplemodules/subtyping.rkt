#lang eopl

(require "lang.rkt")

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

;; <:-decls : Listof(Decl) * Listof(Decl) * Tenv -> Bool
;; Page: 289
;;
;; s1 <: s2 iff s1 has at least as much stuff as s2, and in the same
;; order.  We walk down s1 until we find a declaration that declares
;; the same name as the first component of s2.  If we run off the
;; end of s1, then we fail.  As we walk down s1, we record any type
;; bindings in the tenv
;;
(define <:-decls
  (lambda (decls1 decls2 tenv)
    (cond
      ((null? decls2) #t)
      ((null? decls1) #f)
      (else
       (let ((name1 (decl->name (car decls1)))
             (name2 (decl->name (car decls2))))
         (if (eqv? name1 name2)
             (and
              (equal?
               (decl->type (car decls1))
               (decl->type (car decls2)))
              (<:-decls (cdr decls1) (cdr decls2) tenv))
             (<:-decls (cdr decls1) decls2 tenv)))))))
