#lang scheme/base

(require "../utils/utils.ss")
(require (rename-in (types subtype convenience remove-intersect union utils)                   
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (rep type-rep filter-rep rep-utils) scheme/list
         scheme/contract scheme/match unstable/match
         (for-syntax scheme/base))

;(provide (all-defined-out))

(define-syntax-rule (d/c/p (name . args) c . body)
  (begin (d/c (name . args) c . body)
         (p/c [name c])))

;; this implements the sequence invariant described on the first page relating to Bot

(define (combine l1 l2)
  (match* (l1 l2) 
          [(_ (Bot:)) (-FS -top -bot)]
          [((Bot:) _) (-FS -bot -top)]
          [(_ _) (-FS l1 l2)]))

(provide combine)


(d/c/p (abstract-results results arg-names)
     (tc-results? (listof identifier?) . -> . (or/c Values? ValuesDots?))
     (define keys (for/list ([(nm k) (in-indexed arg-names)]) k))
     (match results
       [(tc-results: ts fs os dty dbound)
        (make-ValuesDots 
         (for/list ([t ts] [f fs] [o os])
           (make-Result t (abstract-filter arg-names keys f) (abstract-object arg-names keys o)))
         dty dbound)]
       [(tc-results: ts fs os)
        (make-Values
         (for/list ([t ts] [f fs] [o os])
           (make-Result t (abstract-filter arg-names keys f) (abstract-object arg-names keys o))))]))


(d/c (abstract-object ids keys o)
  (-> (listof identifier?) (listof name-ref/c) Object? Object?)
  (define (lookup y)
    (for/first ([x ids] [i keys] #:when (free-identifier=? x y)) i))
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i) (app lookup (? values i))]))
  (match o    
    [(Path: p (lookup: idx)) (make-Path p idx)]
    [_ (make-Empty)]))


(d/c (abstract-filter ids keys fs)
  (-> (listof identifier?) (listof name-ref/c) FilterSet/c FilterSet/c)
  (match fs
    [(FilterSet: f+ f-)
     (combine (abo ids keys f+) (abo ids keys f-))]
    [(NoFilter:) (combine -top -top)]))

(d/c (abo xs idxs f)
  ((listof identifier?) (listof name-ref/c) Filter/c . -> . Filter/c)
  (d/c (lookup y)
       (identifier? . -> . (or/c #f integer?))
       (for/first ([x xs] [i idxs] #:when (free-identifier=? x y)) i))
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i) (or (? identifier? (app lookup (? values i)))
                 i)]))
  (define (rec f) (abo xs idxs f))
  (define (sb-t t) t)
  (filter-case (#:Type sb-t #:Filter rec) f
               [#:TypeFilter 
                t p (lookup: idx)
                (make-TypeFilter t p idx)]
               [#:NotTypeFilter
                t p (lookup: idx)
                (make-NotTypeFilter t p idx)]))

(define (merge-filter-sets fs)
  (match fs
    [(list (FilterSet: f+ f-) ...)
     (make-FilterSet (make-AndFilter f+) (make-AndFilter f-))]))

(define (tc-results->values tc)
  (match tc
    [(tc-results: ts) (-values ts)]))

(provide combine-props tc-results->values)

(define (combine-props new-props old-props)
  (define (atomic-prop? p) (or (TypeFilter? p) (NotTypeFilter? p)))
  (define-values (new-atoms new-formulas) (partition atomic-prop? new-props))
  (let loop ([derived-props null] 
             [derived-atoms new-atoms]
             [worklist (append old-props new-formulas)])
    (if (null? worklist)
        (values derived-props derived-atoms)
        (let ([p (car worklist)])
          (match p      
            [(AndFilter: ps) (loop derived-props derived-atoms (append ps (cdr worklist)))]
            [(TypeFilter: _ _ _) (loop derived-props (cons p derived-atoms) (cdr worklist))]
            [(NotTypeFilter: _ _ _) (loop derived-props (cons p derived-atoms) (cdr worklist))]
            [_ (loop (cons p derived-props) derived-atoms (cdr worklist))])))))
