#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/list
         (except-in (types abbrev union utils filter-ops tc-result)
                    -> ->* one-of/c)
         (rep type-rep filter-rep object-rep rep-utils)
         (typecheck tc-subst check-below)
         (contract-req))

(provide abstract-results
         combine-props
         merge-tc-results
         tc-results->values)


(define/cond-contract (abstract-results results arg-names #:rest-id [rest-id #f])
  ((tc-results/c (listof identifier?)) (#:rest-id (or/c #f identifier?))
   . ->* . SomeValues/c)
  (define arg-names* (append arg-names (if rest-id (list rest-id) null)))
  (tc-results->values
    (replace-names
      (for/list ([(nm k) (in-indexed (in-list arg-names*))])
        (list nm (make-Path null (list 0 k))))
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

(define/cond-contract (combine-props new-props old-props exit)
  ((listof Filter/c) (listof Filter/c) (-> none/c)
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
                           (contradictory? (car ps) other-p))
                         (or-loop (cdr ps) result)]
                        [(for/or ([other-p (in-list derived-atoms)])
                           (implied-atomic? (car ps) other-p))
                         -top]
                        [else (or-loop (cdr ps) (cons (car ps) result))]))])
               (if (OrFilter? new-or)
                   (loop (cons new-or derived-formulas) derived-atoms (cdr worklist))
                   (loop derived-formulas derived-atoms (cons new-or (cdr worklist)))))]
            [(or (? TypeFilter?) (? NotTypeFilter?)) (loop derived-formulas (cons p derived-atoms) (cdr worklist))]

            [(AndFilter: ps) (loop derived-formulas derived-atoms (append ps (cdr worklist)))]
            [(Top:) (loop derived-formulas derived-atoms (cdr worklist))]
            [(Bot:) (exit)])))))


(define (unconditional-prop res)
  (match res
    [(tc-any-results: f) f]
    [(tc-results (list (tc-result: _ (FilterSet: f+ f-) _) ...) _)
     (apply -and (map -or f+ f-))]))

(define (merge-tc-results results)
  (define/match (merge-tc-result r1 r2)
    [((tc-result: t1 (FilterSet: f1+ f1-) o1)
      (tc-result: t2 (FilterSet: f2+ f2-) o2))
     (tc-result
       (Un t1 t2)
       (-FS (-or f1+ f2+) (-or f1- f2-))
       (if (equal? o1 o2) o1 -empty-obj))])

  (define/match (same-dty? r1 r2)
    [(#f #f) #t]
    [((cons t1 dbound) (cons t2 dbound)) #t]
    [(_ _) #f])
  (define/match (merge-dty r1 r2)
    [(#f #f) #f]
    [((cons t1 dbound) (cons t2 dbound))
     (cons (Un t1 t2) dbound)])

  (define/match (number-of-values res)
    [((tc-results rs #f))
     (length rs)]
    [((tc-results rs (cons _ dbound)))
     (format "~a and ... ~a" (length rs) dbound)])


  (define/match (merge-two-results res1 res2)
    [((tc-result1: (== -Bottom)) res2) res2]
    [(res1 (tc-result1: (== -Bottom))) res1]
    [((tc-any-results: f1) res2)
     (tc-any-results (-or f1 (unconditional-prop res2)))]
    [(res1 (tc-any-results: f2))
     (tc-any-results (-or (unconditional-prop res1) f2))]
    [((tc-results results1 dty1) (tc-results results2 dty2))
     ;; if we have the same number of values in both cases
     (cond
       [(and (= (length results1) (length results2))
             (same-dty? dty1 dty2))
        (tc-results (map merge-tc-result results1 results2)
                    (merge-dty dty1 dty2))]
       ;; otherwise, error
       [else
        (tc-error/expr "Expected the same number of values, but got ~a and ~a"
                         (length results1) (length results2))])])

  (for/fold ([res (ret -Bottom)]) ([res2 (in-list results)])
    (merge-two-results res res2)))
