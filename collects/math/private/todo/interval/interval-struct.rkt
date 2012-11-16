#lang typed/racket/base

(require racket/match
         racket/performance-hint
         (for-syntax racket/base)
         "../../flonum.rkt")

(provide
 ;; Types
 (rename-out [-Empty-Interval Empty-Interval]
             [-Nonempty-Interval Nonempty-Interval])
 Interval
 ;; Constructors
 empty-interval
 nonempty-interval
 interval
 ;; Predicates
 empty-interval?
 nonempty-interval?
 interval?
 ;; Accessors
 interval-min
 interval-max
 interval-min?
 interval-max?
 ;; Common intervals
 real-interval
 nonnegative-interval
 positive-interval
 negative-interval
 nonpositive-interval
 ;; Basic interval ops
 interval-member?
 interval-subset?
 interval-disjoint?
 interval-intersect
 interval-join
 interval-union
 interval-subtract
 )

;; ===================================================================================================
;; Empty interval type

(struct: Empty-Interval () #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (λ (_ port write?) (fprintf port "empty-interval")))

(define-type -Empty-Interval Empty-Interval)

(define empty-interval (Empty-Interval))
(define empty-interval? Empty-Interval?)

;; ===================================================================================================
;; Nonempty interval type

(: print-nonempty-interval (Nonempty-Interval Output-Port (U #t #f 0 1) -> Any))
(define (print-nonempty-interval ivl port write?)
  (match-define (nonempty-interval a b a? b?) ivl)
  (cond [(and a? b?)  (fprintf port "(interval ~v ~v)" a b)]
        [else  (fprintf port "(interval ~v ~v ~a ~a)" a b a? b?)]))

(struct: Nonempty-Interval ([min : Float] [max : Float] [min? : Boolean] [max? : Boolean])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write print-nonempty-interval)

(define-type -Nonempty-Interval Nonempty-Interval)

(define nonempty-interval? Nonempty-Interval?)
(define nonempty-interval-min Nonempty-Interval-min)
(define nonempty-interval-max Nonempty-Interval-max)
(define nonempty-interval-min? Nonempty-Interval-min?)
(define nonempty-interval-max? Nonempty-Interval-max?)

(: valid-nonempty-interval? (Float Float Any Any -> Any))
(define (valid-nonempty-interval? a b a? b?)
  (cond [(= a b)  (cond [(= a -inf.0)  #f]
                        [(= b +inf.0)  #f]
                        [else  (and a? b?)])]
        [else  (and (-inf.0 . <= . a)
                    (a . < . b)
                    (b . <= . +inf.0))]))

(: unsafe-nonempty-interval (Float Float Boolean Boolean -> Nonempty-Interval))
(define (unsafe-nonempty-interval a b a? b?)
  (Nonempty-Interval (if (equal? a -0.0) 0.0 a)
                     (if (= b 0.0) -0.0 b)
                     (if (= -inf.0 a) #f a?)
                     (if (= +inf.0 b) #f b?)))

(: make-nonempty-interval (case-> (Float Float -> Nonempty-Interval)
                                  (Float Float Any -> Nonempty-Interval)
                                  (Float Float Any Any -> Nonempty-Interval)))
(define (make-nonempty-interval a b [a? #t] [b? #t])
  (cond [(valid-nonempty-interval? a b a? b?)
         (unsafe-nonempty-interval a b (and a? #t) (and b? #t))]
        [else
         (raise-result-error 'nonempty-interval "Nonempty-Interval" empty-interval)]))

(define-match-expander nonempty-interval
  (λ (stx)
    (syntax-case stx ()
      [(_ a b a? b?)  (syntax/loc stx (Nonempty-Interval a b a? b?))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ . args)  (syntax/loc stx (make-nonempty-interval . args))]
      [_  (syntax/loc stx make-nonempty-interval)])))

(define real-interval (unsafe-nonempty-interval -inf.0 +inf.0 #f #f))
(define nonnegative-interval (unsafe-nonempty-interval 0.0 +inf.0 #t #f))
(define positive-interval (unsafe-nonempty-interval 0.0 +inf.0 #f #f))
(define nonpositive-interval (unsafe-nonempty-interval -inf.0 0.0 #f #t))
(define negative-interval (unsafe-nonempty-interval -inf.0 0.0 #f #f))

;; ===================================================================================================

(define-type Interval (U Empty-Interval Nonempty-Interval))
(define (interval? v) (or (empty-interval? v) (nonempty-interval? v)))

(: interval (case-> (Float Float -> Interval)
                    (Float Float Any -> Interval)
                    (Float Float Any Any -> Interval)))
(define (interval a b [a? #t] [b? #t])
  (cond [(valid-nonempty-interval? a b a? b?)
         (unsafe-nonempty-interval a b (and a? #t) (and b? #t))]
        [else
         empty-interval]))

(define-syntax-rule (define-interval-wrapper name f empty-value)
  (define name
    (λ: ([ivl : Interval])
      (if (empty-interval? ivl) empty-value (f ivl)))))

(begin-encourage-inline

  (: interval-min (Interval -> Float))
  (define-interval-wrapper interval-min nonempty-interval-min +nan.0)
  
  (: interval-max (Interval -> Float))
  (define-interval-wrapper interval-max nonempty-interval-max +nan.0)
  
  (: interval-min? (Interval -> Boolean))
  (define-interval-wrapper interval-min? nonempty-interval-min? #f)
  
  (: interval-max? (Interval -> Boolean))
  (define-interval-wrapper interval-max? nonempty-interval-max? #f)
  
  )

;; ===================================================================================================
;; Membership

(: nonempty-interval-member? (Nonempty-Interval Float -> Boolean))
(define (nonempty-interval-member? I x)
  (match-define (nonempty-interval a b a? b?) I)
  (cond [(and (a . < . x) (x . < . b))  #t]
        [(or (x . < . a) (b . < . x))  #f]
        [(and (= x a) a?)  #t]
        [(and (= x b) b?)  #t]
        [else  #f]))

;; ===================================================================================================
;; Subset test

(: nonempty-interval-subset? (Nonempty-Interval Nonempty-Interval -> Boolean))
(define (nonempty-interval-subset? I1 I2)
  (match-define (nonempty-interval a1 b1 a1? b1?) I1)
  (match-define (nonempty-interval a2 b2 a2? b2?) I2)
  (cond [(or (a1 . >= . b2) (b1 . <= . a2))  #f]
        [else  (and (cond [(a1 . > . a2)  #t]
                          [(a1 . < . a2)  #f]
                          [else  (or (not a1?) a2?)])
                    (cond [(b1 . < . b2)  #t]
                          [(b1 . > . b2)  #f]
                          [else  (or (not b1?) b2?)]))]))

;; ===================================================================================================
;; Disjointness test

(: nonempty-interval-disjoint? (Nonempty-Interval Nonempty-Interval -> Boolean))
(define (nonempty-interval-disjoint? I1 I2)
  (match-define (nonempty-interval a1 b1 a1? b1?) I1)
  (match-define (nonempty-interval a2 b2 a2? b2?) I2)
  (define a (max a1 a2))
  (define b (min b1 b2))
  (cond [(< a b)  #f]
        [(> a b)  #t]
        [else  (define a?
                 (cond [(a1 . > . a2)  a1?]
                       [(a1 . < . a2)  a2?]
                       [else  (and a1? a2?)]))
               (define b?
                 (cond [(b1 . > . b2)  b2?]
                       [(b1 . < . b2)  b1?]
                       [else  (and b1? b2?)]))
               (not (and a? b?))]))

;; ===================================================================================================
;; Intersection

(: nonempty-interval-intersect (Nonempty-Interval Nonempty-Interval -> Interval))
(define (nonempty-interval-intersect I1 I2)
  (match-define (nonempty-interval a1 b1 a1? b1?) I1)
  (match-define (nonempty-interval a2 b2 a2? b2?) I2)
  (define-values (a a?)
    (cond [(a1 . > . a2)  (values a1 a1?)]
          [(a1 . < . a2)  (values a2 a2?)]
          [else           (values a1 (and a1? a2?))]))
  (define-values (b b?)
    (cond [(b1 . > . b2)  (values b2 b2?)]
          [(b1 . < . b2)  (values b1 b1?)]
          [else           (values b1 (and b1? b2?))]))
  (cond [(valid-nonempty-interval? a b a? b?)
         (Nonempty-Interval a b a? b?)]
        [else
         empty-interval]))

;; ===================================================================================================
;; Join

(begin-encourage-inline
  
  (: inline-nonempty-interval-join (Float Float Boolean Boolean Float Float Boolean Boolean
                                          -> Nonempty-Interval))
  (define (inline-nonempty-interval-join a1 b1 a1? b1? a2 b2 a2? b2?)
    (define-values (a a?)
      (cond [(a1 . < . a2)  (values a1 a1?)]
            [(a1 . > . a2)  (values a2 a2?)]
            [else           (values a1 (or a1? a2?))]))
    (define-values (b b?)
      (cond [(b1 . > . b2)  (values b1 b1?)]
            [(b1 . < . b2)  (values b2 b2?)]
            [else           (values b1 (or b1? b2?))]))
    (Nonempty-Interval a b a? b?))
  
  )

(: nonempty-interval-join (Nonempty-Interval Nonempty-Interval -> Nonempty-Interval))
(define (nonempty-interval-join I1 I2)
  (match-define (nonempty-interval a1 b1 a1? b1?) I1)
  (match-define (nonempty-interval a2 b2 a2? b2?) I2)
  (inline-nonempty-interval-join a1 b1 a1? b1? a2 b2 a2? b2?))

;; ===================================================================================================
;; Union

(: nonempty-interval-union (Nonempty-Interval Nonempty-Interval
                                              -> (Values Interval Nonempty-Interval)))
(define (nonempty-interval-union I1 I2)
  (match-define (nonempty-interval a1 b1 a1? b1?) I1)
  (match-define (nonempty-interval a2 b2 a2? b2?) I2)
  (cond
    ;;        |-----|  or        (-----|
    ;; |-----|             |-----)
    [(or (a1 . > . b2) (and (= a1 b2) (not (or a1? b2?))))  (values I2 I1)]
    ;; |-----|         or  |-----)
    ;;        |-----|            (-----|
    [(or (b1 . < . a2) (and (= b1 a2) (not (or b1? a2?))))  (values I1 I2)]
     ;; Overlapping
    [else  (values empty-interval (inline-nonempty-interval-join a1 b1 a1? b1? a2 b2 a2? b2?))]))

;; ===================================================================================================
;; Difference

(: nonempty-interval-subtract (Nonempty-Interval Nonempty-Interval
                                                 -> (Values Interval Interval)))
(define (nonempty-interval-subtract I1 I2)
  (match-define (nonempty-interval a1 b1 a1? b1?) I1)
  (match-define (nonempty-interval a2 b2 a2? b2?) I2)
  (cond
    ;;        |-----|  or        |-----|
    ;; |-----|             |-----|
    [(a1 . >= . b2)  (if (and (= a1 b2) a1? b2?)
                         (values empty-interval (interval a1 b1 #f b1?))
                         (values empty-interval I1))]
    ;; |-----|         or  |-----|
    ;;        |-----|            |-----|
    [(b1 . <= . a2)  (if (and (= b1 a2) b1? a2?)
                         (values empty-interval (interval a1 b1 a1? #f))
                         (values empty-interval I1))]
    [else
     (define I3
       (cond
         ;;   |------->
         ;; |--------->
         [(a1 . > . a2)  empty-interval]
         ;; |--------->
         ;; |--------->
         [(a1 . = . a2)  (if (or (not a1?) a2?) empty-interval (interval a1 a1 #t #t))]
         ;; |--------->
         ;;   |------->
         [else  (interval a1 a2 a1? (not a2?))]))
     (define I4
       (cond
         ;; <-------|
         ;; <---------|
         [(b1 . < . b2)  empty-interval]
         ;; <---------|
         ;; <---------|
         [(b1 . = . b2)  (if (or (not b1?) b2?) empty-interval (interval b1 b1 #t #t))]
         ;; <---------|
         ;; <-------|
         [else  (interval b2 b1 (not b2?) b1?)]))
     (cond [(empty-interval? I4)  (values I4 I3)]
           [else  (values I3 I4)])]))

;; ===================================================================================================

(begin-encourage-inline
  
  (: interval-member? (case-> (Empty-Interval Float -> #f)
                              (Interval Float -> Boolean)))
  (define (interval-member? I x)
    (cond [(empty-interval? I)  #f]
          [else  (nonempty-interval-member? I x)]))
  
  (: interval-subset? (case-> (Interval Empty-Interval -> #t)
                              (Empty-Interval Nonempty-Interval -> #f)
                              (Interval Interval -> Boolean)))
  (define (interval-subset? I1 I2)
    (cond [(empty-interval? I2)  #t]
          [(empty-interval? I1)  #f]
          [else  (nonempty-interval-subset? I1 I2)]))
  
  (: interval-disjoint? (case-> (Empty-Interval Interval -> #t)
                                (Interval Empty-Interval -> #t)
                                (Interval Interval -> Boolean)))
  (define (interval-disjoint? I1 I2)
    (cond [(empty-interval? I1)  #t]
          [(empty-interval? I2)  #t]
          [else  (nonempty-interval-disjoint? I1 I2)]))
  
  (: interval-intersect (case-> (Empty-Interval Interval -> Empty-Interval)
                                (Interval Empty-Interval -> Empty-Interval)
                                (Interval Interval -> Interval)))
  (define (interval-intersect I1 I2)
    (cond [(empty-interval? I1)  I1]
          [(empty-interval? I2)  I2]
          [else  (nonempty-interval-intersect I1 I2)]))
  
  (: interval-join (case-> (Empty-Interval Empty-Interval -> Empty-Interval)
                           (Interval Nonempty-Interval -> Nonempty-Interval)
                           (Nonempty-Interval Interval -> Nonempty-Interval)
                           (Interval Interval -> Interval)))
  (define (interval-join I1 I2)
    (cond [(empty-interval? I1)  I2]
          [(empty-interval? I2)  I1]
          [else  (nonempty-interval-join I1 I2)]))
  
  (: interval-union
     (case-> (Empty-Interval Empty-Interval -> (Values Empty-Interval Empty-Interval))
             (Empty-Interval Nonempty-Interval -> (Values Empty-Interval Nonempty-Interval))
             (Nonempty-Interval Empty-Interval -> (Values Empty-Interval Nonempty-Interval))
             (Nonempty-Interval Nonempty-Interval -> (Values Interval Nonempty-Interval))
             (Interval Interval -> (Values Interval Interval))))
  (define (interval-union I1 I2)
    (cond [(empty-interval? I1)  (values I1 I2)]
          [(empty-interval? I2)  (values I2 I1)]
          [else  (nonempty-interval-union I1 I2)]))
  
  (: interval-subtract
     (case-> (Empty-Interval Interval -> (Values Empty-Interval Empty-Interval))
             (Nonempty-Interval Empty-Interval -> (Values Empty-Interval Nonempty-Interval))
             (Interval Interval -> (Values Interval Interval))))
  (define (interval-subtract I1 I2)
    (cond [(empty-interval? I1)  (values I1 I1)]
          [(empty-interval? I2)  (values I2 I1)]
          [else  (nonempty-interval-subtract I1 I2)]))
  
  )  ; begin-encourage-inline
