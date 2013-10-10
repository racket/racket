#lang typed/racket/base

(require "flonum-constants.rkt"
         "flonum-functions.rkt"
         "flonum-bits.rkt")

(provide find-least-flonum flfind-least-integer)

(define +inf-ordinal (flonum->ordinal +inf.0))

(: find-least-flonum (case-> ((Flonum -> Any) Flonum -> (U Flonum #f))
                             ((Flonum -> Any) Flonum Flonum -> (U Flonum #f))))

(define find-least-flonum
  (case-lambda
    [(pred? x-start)
     (when (eqv? +nan.0 x-start)
       (raise-argument-error 'find-least-flonum "non-NaN Flonum" 1 pred? x-start))
     (let loop ([n-end  (flonum->ordinal x-start)] [step 1])
       (define x-end (ordinal->flonum n-end))
       (cond [(pred? x-end)  (find-least-flonum pred? x-start x-end)]
             [(fl= x-end +inf.0)  #f]
             [else  (loop (min +inf-ordinal (+ n-end step)) (* step 2))]))]
    [(pred? x-start x-end)
     (when (eqv? x-start +nan.0)
       (raise-argument-error 'find-least-flonum "non-NaN Flonum" 1 pred? x-start x-end))
     (when (eqv? x-end +nan.0)
       (raise-argument-error 'find-least-flonum "non-NaN Flonum" 2 pred? x-start x-end))
     (cond [(pred? x-start)  x-start]
           [(not (pred? x-end))  #f]
           [else
            (let loop ([n-start  (flonum->ordinal x-start)] [n-end  (flonum->ordinal x-end)])
              (cond [(= n-start n-end)  (define x (ordinal->flonum n-end))
                                        (if (pred? x) x #f)]
                    [else
                     (define n-mid (quotient (+ n-start n-end) 2))
                     (cond [(pred? (ordinal->flonum n-mid))
                            (loop n-start n-mid)]
                           [else
                            (loop (+ n-mid 1) n-end)])]))])]))

(: sub-or-prev (Flonum Flonum -> Flonum))
(define (sub-or-prev k i)
  (define prev-k (fl- k i))
  (if (fl= prev-k k) (flprev* k) prev-k))

(: add-or-next (Flonum Flonum -> Flonum))
(define (add-or-next k i)
  (define next-k (fl+ k i))
  (if (fl= next-k k) (flnext* k) next-k))

(: flmidpoint (Flonum Flonum -> Flonum))
(define (flmidpoint x y)
  (let ([x  (flmin x y)]
        [y  (flmax x y)])
    (cond [(fl= x -inf.0)  (cond [(fl= y +inf.0)  0.0]
                                 [(fl= y -inf.0)  -inf.0]
                                 [else  (+ (* 0.5 -max.0) (* 0.5 y))])]
          [(fl= y +inf.0)  (cond [(fl= x +inf.0)  +inf.0]
                                 [else  (+ (* 0.5 x) (* 0.5 +max.0))])]
          [else  (+ (* 0.5 x) (* 0.5 y))])))

(: flfind-least-integer (case-> ((Flonum -> Any) -> Flonum)
                                ((Flonum -> Any) Flonum -> Flonum)
                                ((Flonum -> Any) Flonum Flonum -> Flonum)
                                ((Flonum -> Any) Flonum Flonum Flonum -> Flonum)))
;; Finds the least integer k such that (pred? k) is #t, given optional bounds and an optional
;; initial estimate. If the predicate is not monotone in the bounds, the result of this function is
;; indeterminate, and depends in an unspecified way on the initial estimate.
;; Formally, to get a unique answer, one of the following cases must be true.
;;  1. Exists k, forall mn <= i < k, (pred? i) is #f /\ forall k <= j <= mx, (pred? j) is #t
;;  2. Forall k, (pred? k) is #f
;;  3. Forall k, (pred? k) is #t
;; where mn0 <= k <= mx0. For case #1, this function returns k. For case #2, it returns +nan.0. For
;; case #3, it returns mn0.
(define (flfind-least-integer pred? [mn0 -inf.0] [mx0 +inf.0] [k0 +nan.0])
  (let ([mn  (flceiling (flmin mn0 mx0))]
        [mx  (flfloor (flmax mn0 mx0))])
    ;; Make sure the initial estimate is in-bounds
    (define k (cond [(and (k0 . >= . mn) (k0 . <= . mx))  (flfloor k0)]
                    [else  (flfloor (flmidpoint mn mx))]))
    (define k? (pred? k))
    ;; Find an integer k-min <= k for which (pred? k-min) is #f; increment exponentially
    (define-values (k-min k-min?)
      (let: loop : (Values Flonum Any) ([k-min : Flonum  k] [k-min? : Any  k?] [i : Flonum  1.0])
        ;(printf "min: ~v~n" k-min)
        (cond [(k-min . fl<= . mn)  (cond [(fl= k-min mn)  (values k-min k-min?)]
                                          [else  (values mn (pred? mn))])]
              [k-min?  (define prev-k-min (sub-or-prev k-min i))
                       (loop prev-k-min (pred? prev-k-min) (* 2.0 (- k-min prev-k-min)))]
              [else  (values k-min #f)])))
    ;; Find an integer k-max >= k0 for which (pred? k-max) is #t; increment exponentially
    (define-values (k-max k-max?)
      (let: loop : (Values Flonum Any) ([k-max : Flonum  k] [k-max? : Any  k?] [i : Flonum  1.0])
        ;(printf "max: ~v~n" k-max)
        (cond [(k-max . fl>= . mx)  (cond [(fl= k-max mx)  (values k-max k-max?)]
                                          [else  (values mx (pred? mx))])]
              [k-max?  (values k-max #t)]
              [else  (define next-k-max (add-or-next k-max i))
                     (loop next-k-max (pred? next-k-max) (* 2.0 (- next-k-max k-max)))])))
    ;; Quickly check cases #2 and #3; if case #1, do a binary search
    (cond [(not k-max?)  +nan.0]
          [k-min?  mn]
          [else
           ;; Loop invariant: (pred? k-max) is #t and (pred? k-min) is #f
           (let loop ([k-min k-min] [k-max k-max])
             ;(printf "~v ~v~n" k-min k-max)
             (define k (flfloor (flmidpoint k-min k-max)))
             ;; Check whether k-min + 1 = k-max or (flnext k-min) = k-max
             (cond [(or (= k k-min) (= k k-max))  k-max]
                   [(pred? k)  (loop k-min k)]
                   [else  (loop k k-max)]))])))
