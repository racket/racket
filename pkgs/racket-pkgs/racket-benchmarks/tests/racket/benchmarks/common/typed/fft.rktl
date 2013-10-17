;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         fft.cl
; Description:  FFT benchmark from the Gabriel tests.
; Author:       Harry Barrow
; Created:      8-Apr-85
; Modified:     6-May-85 09:29:22 (Bob Shaw)
;               11-Aug-87 (Will Clinger)
;               4-May-10 (Vincent St-Amour)
; Language:     Typed Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: pi Float)
(define pi (assert (atan 0 -1) flonum?))

;;; FFT -- This is an FFT benchmark written by Harry Barrow.
;;; It tests a variety of floating point operations,
;;; including array references.

(: *re* (Vectorof Float))
(define *re* (make-vector 1025 0.0))

(: *im* (Vectorof Float))
(define *im* (make-vector 1025 0.0))

(: fft ((Vectorof Float) (Vectorof Float) -> Boolean))
(define (fft areal aimag)
  (let: ((ar : (Vectorof Float) (vector))
         (ai : (Vectorof Float) (vector))
         (i : Integer 0)
         (j : Integer 0)
         (k : Integer 0)
         (m : Integer 0)
         (n : Integer 0)
         (le : Integer 0)
         (le1 : Integer 0)
         (ip : Integer 0)
         (nv2 : Integer 0)
         (nm1 : Integer 0)
         (ur : Float 0.0)
         (ui : Float 0.0)
         (wr : Float 0.0)
         (wi : Float 0.0)
         (tr : Float 0.0)
         (ti : Float 0.0))
        ;; initialize
        (set! ar areal)
        (set! ai aimag)
        (set! n (vector-length ar))
        (set! n (- n 1))
        (set! nv2 (quotient n 2))
        (set! nm1 (- n 1))
        (set! m 0)                                  ;compute m = log(n)
        (set! i 1)
        (let loop ()
          (if (< i n)
              (begin (set! m (+ m 1))
                     (set! i (+ i i))
                     (loop))
              #t))
        (cond ((not (= n (expt 2 m)))
               (error "array size not a power of two.")))
        ;; interchange elements in bit-reversed order
        (set! j 1)
        (set! i 1)
        (let l3 ()
          (cond ((< i j)
                 (set! tr (vector-ref ar j))
                 (set! ti (vector-ref ai j))
                 (vector-set! ar j (vector-ref ar i))
                 (vector-set! ai j (vector-ref ai i))
                 (vector-set! ar i tr)
                 (vector-set! ai i ti)))
          (set! k nv2)
          (let l6 ()
            (cond ((< k j)
                   (set! j (- j k))
                   (set! k (quotient k 2))
                   (l6))))
          (set! j (+ j k))
          (set! i (+ i 1))
          (cond ((< i n)
                 (l3))))
        (do: : Void
             ((l : Natural 1 (+ l 1)))     ;loop thru stages (syntax converted
             ((> l m))                     ; from old MACLISP style \bs)
             (set! le (expt 2 l))
             (set! le1 (quotient le 2))
             (set! ur 1.0)
             (set! ui 0.)
             (set! wr (cos (/ pi le1)))
             (set! wi (sin (/ pi le1)))
             ;; loop thru butterflies
             (do: : Void
                  ((j : Integer 1 (+ j 1)))
                  ((> j le1))
                  ;; do a butterfly
                  (do: : Void
                       ((i : Integer j (+ i le)))
                       ((> i n))
                       (set! ip (+ i le1))
                       (set! tr (- (* (vector-ref ar ip) ur)
                                   (* (vector-ref ai ip) ui)))
                       (set! ti (+ (* (vector-ref ar ip) ui)
                                   (* (vector-ref ai ip) ur)))
                       (vector-set! ar ip (- (vector-ref ar i) tr))
                       (vector-set! ai ip (- (vector-ref ai i) ti))
                       (vector-set! ar i (+ (vector-ref ar i) tr))
                       (vector-set! ai i (+ (vector-ref ai i) ti))))
             (set! tr (- (* ur wr) (* ui wi)))
             (set! ti (+ (* ur wi) (* ui wr)))
             (set! ur tr)
             (set! ui ti))
        #t))

;;; the timer which does 10 calls on fft

(: fft-bench ( -> Void))
(define (fft-bench)
  (do: : Void
       ((ntimes : Integer 0 (+ ntimes 1)))
       ((= ntimes 5000))
       (fft *re* *im*)))

;;; call:  (fft-bench)

(time (fft-bench))
