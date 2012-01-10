#lang typed/racket/base

(require racket/flonum
         (except-in racket/fixnum fl->fx fx->fl)
         racket/match
         "flonum.rkt"
         "flomap-struct.rkt")

(provide flomap-flip-horizontal flomap-flip-vertical flomap-transpose
         flomap-cw-rotate flomap-ccw-rotate)

(: flomap-flip-horizontal (flomap -> flomap))
(define (flomap-flip-horizontal fm)
  (match-define (flomap vs c w h) fm)
  (define w-1 (fx- w 1))
  (inline-build-flomap c w h (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k (fx- w-1 x) y)))))

(define (flomap-flip-vertical fm)
  (match-define (flomap vs c w h) fm)
  (define h-1 (fx- h 1))
  (inline-build-flomap c w h (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k x (fx- h-1 y))))))

(define (flomap-transpose fm)
  (match-define (flomap vs c w h) fm)
  (inline-build-flomap c h w (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k y x)))))

(define (flomap-cw-rotate fm)
  (match-define (flomap vs c w h) fm)
  (define h-1 (fx- h 1))
  (inline-build-flomap c h w (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k (fx- h-1 y) x)))))

(define (flomap-ccw-rotate fm)
  (match-define (flomap vs c w h) fm)
  (define w-1 (fx- w 1))
  (inline-build-flomap c h w (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k y (fx- w-1 x))))))
