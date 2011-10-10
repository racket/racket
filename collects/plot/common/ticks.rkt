#lang racket/base

;; Data structure that represents a tick, and functions that produce ticks.

(require racket/string racket/list racket/contract racket/pretty
         "math.rkt"
         "format.rkt"
         "utils.rkt"
         "contract.rkt" "contract-doc.rkt"
         "parameters.rkt")

(provide (all-defined-out))

(define-struct/contract tick
  ([p real?] [label string?] [major? boolean?])
  #:transparent)

(define (tick-ps->majors ps major-skip)
  (define zero-idx (list-index 0 ps =))
  (define zero-idx-rem (if (zero-idx . < . 0) 0 (remainder zero-idx major-skip)))
  (for/list ([n  (in-range (length ps))])
    (= (remainder n major-skip) zero-idx-rem)))

(define (linear-ticks major-skip x-min x-max)
  (when (x-min . >= . x-max)
    (error 'default-range->ticks "expected x-min < x-max; got x-min = ~e and x-max = ~e" x-min x-max))
  (let ([x-min  (inexact->exact x-min)]
        [x-max  (inexact->exact x-max)])
    (define e (floor-log10 (- x-max x-min)))
    (define mag (expt 10 e))
    (define step (let ([y  (/ (- x-max x-min) mag)])
                   (cond [(y . < . 2)  (* 1/5 mag)]
                         [(y . < . 5)  (* 1/2 mag)]
                         [else         mag])))
    (define start (* (ceiling (/ x-min step)) step))
    (define stop (* (floor (/ x-max step)) step))
    (define num (+ 1 (round (/ (- stop start) step))))
    (define ps (linear-seq start stop num))
    (define digits (digits-for-range x-min x-max))
    (define labels (map (λ (p) (real->plot-label p digits)) ps))
    (define majors (tick-ps->majors ps major-skip))
    (map tick ps labels majors)))

(defproc (default-ticks-fun [x-min real?] [x-max real?]) (listof tick?)
  (linear-ticks (plot-tick-skip) x-min x-max))

(defproc (auto-contour-zs [z-min real?] [z-max real?]) (listof real?)
  (let* ([zs  (map tick-p (default-ticks-fun z-min z-max))]
         [zs  (if (= (first zs) z-min) (rest zs) zs)]
         [zs  (if (= (last zs) z-max) (take zs (sub1 (length zs))) zs)])
    zs))
