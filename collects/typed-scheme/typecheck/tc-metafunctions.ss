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


(d/c/p (abstract-filters results)
     (tc-results? . -> . (or/c Values? ValuesDots?))     
     (match results
       [(tc-results: ts fs os dty dbound)
        (make-ValuesDots 
         (for/list ([t ts] [f fs] [o os])
           (make-Result t f o))
         dty dbound)]
       [(tc-results: ts fs os)
        (make-Values
         (for/list ([t ts] [f fs] [o os])
           (make-Result t f o)))]))


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

(d/c (abo xs idxs f [inc 0])
  ((listof identifier?) (listof name-ref/c) Filter/c . -> . Filter/c)
  (define (lookup y)
    (for/first ([x xs] [i idxs] #:when (free-identifier=? x y)) (+ inc i)))
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i) (app lookup (? values i))]))
  (define (rec f) (abo xs idxs f inc))
  (define (sb-t t) t)
  (filter-case (#:Type sb-t #:Filter rec) f
               [#:TypeFilter t p (lookup: idx)
                             (make-TypeFilter t p idx)]
               [#:NotTypeFilter t p (lookup: idx)
                                (make-NotTypeFilter t p idx)]))

(define (merge-filter-sets fs)
  (match fs
    [(list (FilterSet: f+ f-) ...)
     (make-FilterSet (make-AndFilter f+) (make-AndFilter f-))]))



#|
#;
(define/contract (split-lfilters lf idx)  
  (LatentFilterSet/c index/c . -> . LatentFilterSet/c)
  (define (idx= lf)
    (match lf
      [(LBot:) #t]
      [(LNotTypeFilter: _ _ idx*) (= idx* idx)]
      [(LTypeFilter: _ _ idx*) (= idx* idx)]))
  (match lf
    [(LFilterSet: lf+ lf-)
     (make-LFilterSet (filter idx= lf+) (filter idx= lf-))]))

(define-match-expander T-FS:
  (lambda (stx) #'(FilterSet: _ (list (Bot:)))))
(define-match-expander F-FS:
  (lambda (stx) #'(FilterSet: (list (Bot:)) _)))

#;
(d/c (combine-filter f1 f2 f3 t2 t3 o2 o3)
  (FilterSet/c FilterSet/c FilterSet/c Type? Type? Object? Object? . -> . tc-results?)
  (define (mk f) (ret (Un t2 t3) f (make-Empty)))
  (match* (f1 f2 f3)
    [((T-FS:) f _) (ret t2 f o2)]
    [((F-FS:) _ f) (ret t3 f o3)]
    ;; the student expansion
    [(f (T-FS:) (F-FS:)) (mk f)]
    ;; skipping the general or/predicate rule because it's really complicated
    ;; or/predicate special case for one elem lists
    ;; note that we are relying on equal? on identifiers here
    [((FilterSet: (list (TypeFilter: t pi x)) (list (NotTypeFilter: t pi x)))
      (T-FS:)
      (FilterSet: (list (TypeFilter: s pi x)) (list (NotTypeFilter: s pi x))))
     (mk (make-FilterSet (list (make-TypeFilter (Un t s) pi x)) (list (make-NotTypeFilter (Un t s) pi x))))]
    ;; or
    [((FilterSet: f1+ f1-) (T-FS:) (FilterSet: f3+ f3-)) (mk (combine null (append f1- f3-)))]
    ;; and
    [((FilterSet: f1+ f1-) (FilterSet: f2+ f2-) (F-FS:)) 
     (mk (combine (append f1+ f2+)		  
                  (append (for/list ([f f1-]
                                     #:when (not (null? f2+)))
			    (make-ImpFilter f2+ (list f)))
			  (for/list ([f f2-]
                                     #:when (not (null? f1+)))
			    (make-ImpFilter f1+ (list f))))))]
    [(f f* f*) (mk f*)]
    [(_ _ _)
     ;; could intersect f2 and f3 here
     (mk (make-FilterSet null null))]))
|#

;; (or/c Values? ValuesDots?) listof[identifier] -> tc-results?
(d/c/p (values->tc-results tc)
  ((or/c Values? ValuesDots?) . -> . tc-results?)
  (match tc
    [(ValuesDots: (list (Result: ts fs os) ...) dty dbound)
     (ret ts fs os dty dbound)]
    [(Values: (list (Result: ts fs os) ...))
     (ret ts fs os)]))

(define (tc-results->values tc)
  (match tc
    [(tc-results: ts) (-values ts)]))

(provide combine-props tc-results->values)

(define (combine-props new-props old-props)
  (define-values (new-atoms new-formulas) 
    (partition (lambda (e) (or (TypeFilter? e) (NotTypeFilter? e))) new-props))
  (values new-formulas new-atoms)
  #;#;
  (define-values (derived-imps derived-atoms)
    (for/fold 
        ([derived-imps null]
         [derived-atoms null])
      ([o old-props])
      (match o
        [(ImpFilter: as cs)
         (let ([as* (remove* new-atoms as filter-equal?)])
           (if (null? as*)
               (values derived-imps (append cs new-atoms))
               (values (cons (make-ImpFilter as* cs) derived-imps) derived-atoms)))])))
  (values (append new-imps derived-imps) (append new-atoms derived-atoms)))
