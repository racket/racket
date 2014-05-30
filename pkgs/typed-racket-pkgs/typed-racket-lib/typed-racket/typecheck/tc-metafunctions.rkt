#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/list
         (except-in (types abbrev union utils filter-ops)
                    -> ->* one-of/c)
         (rep type-rep filter-rep object-rep rep-utils)
         (typecheck tc-subst check-below)
         (contract-req))

(provide abstract-results
         combine-props
         tc-results->values)


;; abstract-results
;;
;; Given results from the range of a lambda, abstract any
;; identifier objects into index (numeric) objects. This is effectively
;; doing a kind of De Bruijn indexing for objects.
;;
;; When the body of a lambda is type-checked, its filters and object
;; may refer to variables that are in scope in that body. Since these
;; names are not in scope outside of the lambda, the type of the function
;; will instead store their De Bruijn indices.
;;
;; For example, given the function
;;
;;   (λ (x) (number? x))
;;
;; the typechecker will check the body and return
;;
;;   Boolean ; N_x | !N_x ; ∅
;;
;; but the `x`s have to be converted to indices to get the type
;;
;;      N_(0,0) | !N_(0,0)
;; Any -------------------> Boolean
;;              ∅
;;
;; where the index (0,0) indicates the first argument of
;; the current function
;;
;; Comparatively, a curried predicate like
;;
;;   (λ (x) (λ (y)(number? x)))
;;
;; gets the type
;;
;;             N_(1,0) | !N_(1,0)
;; Any -> Any -------------------> Boolean
;;                     ∅
;;
;; (ignoring filters on the first arrow)
;; where the index (1,0) indicates the first argument of
;; the enclosing lambda.
;;
;; The paper "Logical Types for Untyped Languages" takes a different
;; approach where all function types carry their names, so that the first
;; example would have the type:
;;
;;        N_x | !N_x
;; x:Any ------------> Boolean
;;            ∅
;;
;; See tc-subst.rkt for the functions that take an abstracted function
;; type and substitute in a concrete object.
;;
(define/cond-contract (abstract-results results arg-names)
  (tc-results/c (listof identifier?) . -> . SomeValues/c)
  (tc-results->values
    (replace-names
      (for/list ([(nm k) (in-indexed arg-names)]) (list nm (make-Path null (list 0 k))))
      results)))

(define (tc-results->values tc)
  (match (fix-results tc)
    [(tc-any-results: f)
     (-AnyValues f)]
    [(tc-results: ts fs os)
     (make-Values (map -result ts fs os))]
    [(tc-results: ts fs os dty dbound)
     (make-ValuesDots (map -result ts fs os) dty dbound)]))

(define/cond-contract (resolve atoms prop)
  ((listof Filter/c)
   Filter/c
   . -> .
   Filter/c)
  (for/fold ([prop prop])
    ([a (in-list atoms)])
    (match prop
      [(AndFilter: ps)
       (let loop ([ps ps] [result null])
         (if (null? ps)
             (apply -and result)
             (let ([p (car ps)])
               (cond [(contradictory? a p) -bot]
                     [(implied-atomic? p a) (loop (cdr ps) result)]
                     [else (loop (cdr ps) (cons p result))]))))]
      [_ prop])))

(define (flatten-props ps)
  (let loop ([ps ps])
    (match ps
      [(list) null]
      [(cons (AndFilter: ps*) ps) (loop (append ps* ps))]
      [(cons p ps) (cons p (loop ps))])))

(define/cond-contract (combine-props new-props old-props flag)
  ((listof Filter/c) (listof Filter/c) (box/c boolean?)
   . -> .
   (values (listof (or/c ImpFilter? OrFilter?)) (listof (or/c TypeFilter? NotTypeFilter?))))
  (define (atomic-prop? p) (or (TypeFilter? p) (NotTypeFilter? p)))
  (define-values (new-atoms new-formulas) (partition atomic-prop? (flatten-props new-props)))
  (let loop ([derived-formulas null]
             [derived-atoms new-atoms]
             [worklist (append old-props new-formulas)])
    (if (null? worklist)
        (values derived-formulas derived-atoms)
        (let* ([p (car worklist)]
               [p (resolve derived-atoms p)])
          (match p
            [(ImpFilter: a c)
             (if (for/or ([p (in-list (append derived-formulas derived-atoms))])
                   (implied-atomic? a p))
                 (loop derived-formulas derived-atoms (cons c (cdr worklist)))
                 (loop (cons p derived-formulas) derived-atoms (cdr worklist)))]
            [(OrFilter: ps)
             (let ([new-or
                    (let or-loop ([ps ps] [result null])
                      (cond
                        [(null? ps) (apply -or result)]
                        [(for/or ([other-p (in-list (append derived-formulas derived-atoms))])
                             (complementary? (car ps) other-p))
                         (or-loop (cdr ps) result)]
                        [(for/or ([other-p (in-list derived-atoms)])
                             (implied-atomic? (car ps) other-p))
                         -top]
                        [else (or-loop (cdr ps) (cons (car ps) result))]))])
               (if (OrFilter? new-or)
                   (loop (cons new-or derived-formulas) derived-atoms (cdr worklist))
                   (loop derived-formulas derived-atoms (cons new-or (cdr worklist)))))]
            [(TypeFilter: _ _ _) (loop derived-formulas (cons p derived-atoms) (cdr worklist))]
            [(NotTypeFilter: _ _ _) (loop derived-formulas (cons p derived-atoms) (cdr worklist))]

            [(AndFilter: ps) (loop derived-formulas derived-atoms (append ps (cdr worklist)))]
            [(Top:) (loop derived-formulas derived-atoms (cdr worklist))]
            [(Bot:) (set-box! flag #f) (values derived-formulas derived-atoms)])))))

