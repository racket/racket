#lang typed/racket/base

(require racket/match
         (only-in racket/unsafe/ops unsafe-flvector-ref)
         "flonum.rkt"
         "flomap-struct.rkt")

(provide flomap-gradient-x flomap-gradient-y flomap-gradient flomap-gradient-normal)

;; ===================================================================================================
;; Derivatives (Scharr operator)

(: flomap-gradient-x (flomap -> flomap))
(define (flomap-gradient-x fm)
  (match-define (flomap vs c w h) fm)
  (define +x (fx* c 1))
  (define -x+y (fx* c (fx- w 1)))
  (define +x+y (fx* c (fx+ w 1)))
  (define w-1 (fx- w 1))
  (define h-1 (fx- h 1))
  (inline-build-flomap
   c w h
   (λ (k x y i)
     (cond [(and (x . fx> . 0) (x . fx< . w-1)
                 (y . fx> . 0) (y . fx< . h-1))
            (+ (- (* 0.1875 (unsafe-flvector-ref vs (fx- i -x+y)))
                  (* 0.1875 (unsafe-flvector-ref vs (fx- i +x+y))))
               (- (* 0.6250 (unsafe-flvector-ref vs (fx+ i +x)))
                  (* 0.6250 (unsafe-flvector-ref vs (fx- i +x))))
               (- (* 0.1875 (unsafe-flvector-ref vs (fx+ i +x+y)))
                  (* 0.1875 (unsafe-flvector-ref vs (fx+ i -x+y)))))]
           [else
            (+ (- (* 0.1875 (flomap-ref fm k (+ x 1) (- y 1)))
                  (* 0.1875 (flomap-ref fm k (- x 1) (- y 1))))
               (- (* 0.6250 (flomap-ref fm k (+ x 1) y))
                  (* 0.6250 (flomap-ref fm k (- x 1) y)))
               (- (* 0.1875 (flomap-ref fm k (+ x 1) (+ y 1)))
                  (* 0.1875 (flomap-ref fm k (- x 1) (+ y 1)))))]))))

(: flomap-gradient-y (flomap -> flomap))
(define (flomap-gradient-y fm)
  (match-define (flomap vs c w h) fm)
  (define +y (fx* c w))
  (define -x+y (fx* c (fx- w 1)))
  (define +x+y (fx* c (fx+ w 1)))
  (define w-1 (fx- w 1))
  (define h-1 (fx- h 1))
  (inline-build-flomap
   c w h
   (λ (k x y i)
     (cond [(and (x . fx> . 0) (x . fx< . w-1)
                 (y . fx> . 0) (y . fx< . h-1))
            (+ (- (* 0.1875 (unsafe-flvector-ref vs (fx+ i -x+y)))
                  (* 0.1875 (unsafe-flvector-ref vs (fx- i +x+y))))
               (- (* 0.6250 (unsafe-flvector-ref vs (fx+ i +y)))
                  (* 0.6250 (unsafe-flvector-ref vs (fx- i +y))))
               (- (* 0.1875 (unsafe-flvector-ref vs (fx+ i +x+y)))
                  (* 0.1875 (unsafe-flvector-ref vs (fx- i -x+y)))))]
           [else
            (+ (- (* 0.1875 (flomap-ref fm k (- x 1) (+ y 1)))
                  (* 0.1875 (flomap-ref fm k (- x 1) (- y 1))))
               (- (* 0.6250 (flomap-ref fm k x (+ y 1)))
                  (* 0.6250 (flomap-ref fm k x (- y 1))))
               (- (* 0.1875 (flomap-ref fm k (+ x 1) (+ y 1)))
                  (* 0.1875 (flomap-ref fm k (+ x 1) (- y 1)))))]))))

(: flomap-gradient (flomap -> (values flomap flomap)))
(define (flomap-gradient fm)
  (values (flomap-gradient-x fm) (flomap-gradient-y fm)))

(: flomap-gradient-normal (flomap -> flomap))
(define (flomap-gradient-normal z-fm)
  (unless (= 1 (flomap-components z-fm))
    (raise-type-error 'flomap-gradient-normal "flomap with 1 component" z-fm))
  (define-values (dx-fm dy-fm) (flomap-gradient z-fm))
  (match-define (flomap dx-vs 1 w h) dx-fm)
  (match-define (flomap dy-vs 1 _w _h) dy-fm)
  (define normal-vs (make-flvector (* 3 w h)))
  (for ([i  (in-range (* w h))])
    (define dx (flvector-ref dx-vs i))
    (define dy (flvector-ref dy-vs i))
    (define-values (nx ny nz) (fl3normalize (- dx) (- dy) 2.0))
    (define j (fx* 3 i))
    (flvector-set! normal-vs j nx)
    (flvector-set! normal-vs (fx+ j 1) ny)
    (flvector-set! normal-vs (fx+ j 2) nz))
  (flomap normal-vs 3 w h))
