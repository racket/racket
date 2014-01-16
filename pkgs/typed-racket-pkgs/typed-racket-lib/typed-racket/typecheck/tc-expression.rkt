#lang racket/unit

(require 
  "../utils/utils.rkt"
  (typecheck tc-expr-unit signatures tc-app-helper)
  (types tc-result tc-error utils abbrev)
  (rep type-rep)
  (utils tc-utils)
  (env index-env tvar-env scoped-tvar-env)
  (private syntax-properties type-annotation parse-type)
  unstable/syntax
  racket/match
  syntax/stx
  syntax/parse
  (for-syntax racket/base syntax/parse syntax/parse/experimental/template))


(import tc-expr^)
(export tc-expression^)

(define-syntax define-expression-annotations
  (syntax-parser
    ((_ (name:id (attr:id acc:expr) ...) ...)
     (template
       (begin
         (define-syntax-class name
           #:literal-sets (kernel-literals)
           (pattern (~and exp #%expression)
             (?@ #:attr attr (acc #'exp)) ...
             (?@ #:when (attribute attr)) ...)) ...)))))

(define-expression-annotations
  (type-inst (vars type-inst-property))
  (external-check (check external-check-property))
  (type-ascrip
    (ty type-ascription)
    (scoped-vars ascribed-scoped-type-vars)))


;; Typecheck an (#%expression e) form
(define (tc/#%expression form expected)
  (syntax-parse form
    [(exp:type-inst e)
     (do-inst (tc-expr #'e) (attribute exp.vars))]
    [(exp:type-ascrip e)
     (add-scoped-tvars #'e (attribute exp.scoped-vars))
     (tc-expr/check #'e (attribute exp.ty))]
    [(exp:external-check e)
      ((attribute exp.check) #'e)
      (if expected
         (tc-expr/check #'e expected)
         (tc-expr #'e))]
    [(_ e)
     (if expected
         (tc-expr/check #'e expected)
         (tc-expr #'e))]))


;; do-inst : syntax? (or/c tc-results/c #f) -> tc-results/c
(define (do-inst results inst)
  (when (and inst (not (syntax? inst)))
    (int-err "Bad type-inst property ~a" inst))
  (match results
    [(tc-result1: ty f o)
     (define new-ty
       (cond
         [(not inst) ty]
         [(not (or (Poly? ty) (PolyDots? ty)))
          (tc-error/expr "Cannot instantiate non-polymorphic type ~a"
                         (cleanup-type ty))]
         [(and (Poly? ty)
               (not (= (syntax-length inst) (Poly-n ty))))
          (tc-error/expr "Wrong number of type arguments to polymorphic type ~a:\nexpected: ~a\ngot: ~a"
                         (cleanup-type ty) (Poly-n ty) (syntax-length inst))]
         [(and (PolyDots? ty) (< (syntax-length inst) (sub1 (PolyDots-n ty))))
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
          (instantiate-poly ty (stx-map parse-type inst))]))
     (ret new-ty f o)]
    [(tc-results: ts)
     (if inst
         (tc-error/expr #:return (ret -Bottom)
                        "Cannot instantiate expression that produces ~a values"
                        (if (null? ts) 0 "multiple"))
         results)]))

