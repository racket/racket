#lang racket/unit

(require 
  "../utils/utils.rkt"
  (typecheck tc-expr-unit signatures tc-app-helper)
  (types tc-result tc-error utils)
  (rep type-rep)
  (utils tc-utils)
  (env index-env tvar-env)
  (private syntax-properties type-annotation parse-type)
  unstable/syntax
  racket/match
  syntax/stx
  syntax/parse)


(import tc-expr^)
(export tc-expression^)

;; Typecheck an (#%expression e) form
(define (tc/expression form expected)
  (syntax-parse form
    [((~and exp #%expression) e)
     #:when (type-inst-property #'exp)
     (match (tc-expr #'e)
       [(tc-results: ts fs os)
        ;; do the instantiation
        (ret (do-inst ts (type-inst-property #'exp)) fs os)])]
    [((~and exp #%expression) e)
     #:when (type-ascription #'exp)
     (tc-expr/check #'e (type-ascription #'exp))]
    [((~and exp #%expression) e)
     #:when (external-check-property #'exp)
      ((external-check-property #'exp) #'e)
      (if expected
         (tc-expr/check #'e expected)
         (tc-expr #'e))]

    [(_ e)
     (if expected
         (tc-expr/check #'e expected)
         (tc-expr #'e))]))


;; do-inst : syntax? (listof Type/c) -> (listof Type/c)
(define (do-inst ty inst)
  (when (and inst (not (syntax? inst)))
    (int-err "Bad type-inst property ~a" inst))
  (match ty
    [(list ty)
     (list
       (cond
         [(not inst) ty]
         [(not (or (Poly? ty) (PolyDots? ty)))
          (tc-error/expr "Cannot instantiate non-polymorphic type ~a"
                         (cleanup-type ty))]
         [(and (Poly? ty)
               (not (= (syntax-length inst) (Poly-n ty))))
          (tc-error/expr "Wrong number of type arguments to polymorphic type ~a:\nexpected: ~a\ngot: ~a"
                         (cleanup-type ty) (Poly-n ty) (syntax-length inst))]
         [(and (PolyDots? ty) (not (>= (syntax-length inst) (sub1 (PolyDots-n ty)))))
          ;; we can provide 0 arguments for the ... var
          (tc-error/expr "Wrong number of type arguments to polymorphic type ~a:\nexpected at least: ~a\ngot: ~a"
                         (cleanup-type ty) (sub1 (PolyDots-n ty)) (syntax-length inst))]
         [(PolyDots? ty)
          ;; In this case, we need to check the last thing.  If it's a dotted var, then we need to
          ;; use instantiate-poly-dotted, otherwise we do the normal thing.
          ;; In the case that the list is empty we also do the normal thing
          (let ((stx-list (syntax->list inst)))
            (if (null? stx-list)
                (instantiate-poly ty null)
                (match stx-list
                  [(list ty-stxs ... (app syntax-e (cons bound-ty-stx (? identifier? bound-id))))
                   (unless (bound-index? (syntax-e bound-id))
                     (tc-error/stx bound-id "~a is not a type variable bound with ..." (syntax-e bound-id)))
                   (if (= (length ty-stxs) (sub1 (PolyDots-n ty)))
                       (let* ([last-id (syntax-e bound-id)]
                              [last-ty (extend-tvars (list last-id) (parse-type bound-ty-stx))])
                         (instantiate-poly-dotted ty (map parse-type ty-stxs) last-ty last-id))
                       (tc-error/expr "Wrong number of fixed type arguments to polymorphic type ~a:\nexpected: ~a\ngot: ~a"
                                      ty (sub1 (PolyDots-n ty)) (length ty-stxs)))]
                  [_
                   (instantiate-poly ty (map parse-type stx-list))])))]
         [else
          (instantiate-poly ty (stx-map parse-type inst))]))]
    [_ (if inst
           (tc-error/expr "Cannot instantiate expression that produces ~a values"
                          (if (null? ty) 0 "multiple"))
           ty)]))

