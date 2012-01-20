#lang typed/racket/base

(require racket/flonum
         (except-in racket/fixnum fl->fx fx->fl)
         racket/match
         "flonum.rkt"
         "flomap-struct.rkt")

(provide flomap-gradient-x flomap-gradient-y flomap-gradient flomap-gradient-normal)

;; ===================================================================================================
;; Derivatives (Schurr operator)

(: flomap-gradient-x (flomap -> flomap))
(define (flomap-gradient-x fm)
  (match-define (flomap vs c w h) fm)
  (define cw (fx* c w))
  (define d20 (fx- 1 cw))
  (define d22 (fx+ cw 1))
  (define w-1 (fx- w 1))
  (define h-1 (fx- h 1))
  (inline-build-flomap
   c w h
   (λ (_k x y i)
     (cond [(and (x . fx> . 0) (x . fx< . w-1)
                 (y . fx> . 0) (y . fx< . h-1))
            (+ (- (* 0.1875 (unsafe-flvector-ref vs (fx+ i d20)))
                  (* 0.1875 (unsafe-flvector-ref vs (fx- i d22))))
               (- (* 0.6250 (unsafe-flvector-ref vs (fx+ i 1)))
                  (* 0.6250 (unsafe-flvector-ref vs (fx- i 1))))
               (- (* 0.1875 (unsafe-flvector-ref vs (fx+ i d22)))
                  (* 0.1875 (unsafe-flvector-ref vs (fx- i d20)))))]
           [else  0.0]))))

(: flomap-gradient-y (flomap -> flomap))
(define (flomap-gradient-y fm)
  (match-define (flomap vs c w h) fm)
  (define cw (fx* c w))
  (define d02 (fx- cw 1))
  (define d22 (fx+ cw 1))
  (define w-1 (fx- w 1))
  (define h-1 (fx- h 1))
  (inline-build-flomap
   c w h
   (λ (_k x y i)
     (cond [(and (x . fx> . 0) (x . fx< . w-1)
                 (y . fx> . 0) (y . fx< . h-1))
            (+ (- (* 0.1875 (unsafe-flvector-ref vs (fx+ i d02)))
                  (* 0.1875 (unsafe-flvector-ref vs (fx- i d22))))
               (- (* 0.6250 (unsafe-flvector-ref vs (fx+ i cw)))
                  (* 0.6250 (unsafe-flvector-ref vs (fx- i cw))))
               (- (* 0.1875 (unsafe-flvector-ref vs (fx+ i d22)))
                  (* 0.1875 (unsafe-flvector-ref vs (fx- i d02)))))]
           [else  0.0]))))

(: flomap-gradient (flomap -> (values flomap flomap)))
(define (flomap-gradient fm)
  (values (flomap-gradient-x fm) (flomap-gradient-y fm)))

(: flomap-gradient-normal (flomap -> flomap))
(define (flomap-gradient-normal z-fm)
  (define-values (dx-fm dy-fm) (flomap-gradient z-fm))
  (match-define (flomap dx-vs 1 w h) dx-fm)
  (match-define (flomap dy-vs 1 _w _h) dy-fm)
  (define normal-vs (make-flvector (* 3 w h)))
  (for ([i  (in-range (* w h))])
    (define dx (unsafe-flvector-ref dx-vs i))
    (define dy (unsafe-flvector-ref dy-vs i))
    (define-values (nx ny nz) (fl3normalize (- dx) (- dy) 2.0))
    (define j (fx* 3 i))
    (unsafe-flvector-set! normal-vs j nx)
    (unsafe-flvector-set! normal-vs (fx+ j 1) ny)
    (unsafe-flvector-set! normal-vs (fx+ j 2) nz))
  (flomap normal-vs 3 w h))
