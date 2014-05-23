#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer infer-in]))
(require racket/match
         unstable/list
         (contract-req)
         (infer-in infer)
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         (types resolve subtype remove-intersect union)
         (only-in (env type-env-structs lexical-env)
                  env? update-type/lexical env-map env-props replace-props)
         (rename-in (types abbrev)
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (typecheck tc-metafunctions))

;(trace replace-nth)

(define/cond-contract (update t ft pos? lo)
  (Type/c Type/c boolean? (listof PathElem?) . -> . Type/c)
  (match* ((resolve t) lo)
    ;; pair ops
    [((Pair: t s) (list rst ... (CarPE:)))
     (-pair (update t ft pos? rst) s)]
    [((Pair: t s) (list rst ... (CdrPE:)))
     (-pair t (update s ft pos? rst))]

    ;; syntax ops
    [((Syntax: t) (list rst ... (SyntaxPE:)))
     (-Syntax (update t ft pos? rst))]

    ;; promise op
    [((Promise: t) (list rst ... (ForcePE:)))
     (-Promise (update t ft pos? rst))]

    ;; struct ops
    [((Struct: nm par flds proc poly pred)
      (list rst ... (StructPE: (? (lambda (s) (subtype t s)) s) idx)))
     (make-Struct nm par
                  (list-update flds idx (match-lambda
                                          [(fld: e acc-id #f)
                                           (make-fld (update e ft pos? rst) acc-id #f)]
                                          [_ (int-err "update on mutable struct field")]))
                  proc poly pred)]

    ;; otherwise
    [(t (list))
     (if pos?
         (restrict t ft)
         (remove t ft))]
    [((Union: ts) lo)
     (apply Un (map (lambda (t) (update t ft pos? lo)) ts))]
    [(t* lo)
     ;; This likely comes up with (-lst t) and we need to improve the system to make sure this case
     ;; dosen't happen
     #;
     (int-err "update along ill-typed path: ~a ~a ~a" t t* lo)
     t]))

;; sets the flag box to #f if anything becomes (U)
(define/cond-contract (env+ env fs flag)
  (([e env?] [fs (listof Filter/c)] [bx (box/c boolean?)])
   #:pre (bx) (unbox bx) . ->i . [_ env?])
  (define-values (props atoms) (combine-props fs (env-props env) flag))
  (for/fold ([Γ (replace-props env (append atoms props))]) ([f (in-list atoms)])
    (match f
      [(Bot:) (set-box! flag #f) (env-map (lambda (k v) (Un)) Γ)]
      [(or (TypeFilter: ft lo x) (NotTypeFilter: ft lo x))
       (update-type/lexical (lambda (x t) (let ([new-t (update t ft (TypeFilter? f) lo)])
                                            (when (type-equal? new-t (Un))
                                              (set-box! flag #f))
                                            new-t))
                            x Γ)]
      [_ Γ])))

(provide/cond-contract
 [env+ (([e env?] [fs (listof Filter/c)] [bx (box/c boolean?)])
        #:pre (bx) (unbox bx) . ->i . [_ env?])])
