#lang racket/base

;; Small library for clipping points, rectangles, lines and polygons against axial planes.

(require racket/match racket/list
         "../common/vector.rkt")

(provide point-in-bounds? clip-line clip-rectangle clip-lines clip-polygon)

;; ===================================================================================================
;; Point clipping

(define (point-in-bounds? v x-min x-max y-min y-max)
  (match-define (vector x y) v)
  (and (x . >= . x-min) (x . <= . x-max) (y . >= . y-min) (y . <= . y-max)))

;; ===================================================================================================
;; Rectangle clipping

(define (clip-rectangle v1 v2 x-min x-max y-min y-max)
  (let/ec return
    ; early accept: both endpoints in bounds
    (when (and (point-in-bounds? v1 x-min x-max y-min y-max)
               (point-in-bounds? v2 x-min x-max y-min y-max))
      (return v1 v2))
    ; early reject: both endpoints on the outside of the same plane
    (match-define (vector x1 y1) v1)
    (match-define (vector x2 y2) v2)
    (when (or (and (x1 . < . x-min) (x2 . < . x-min)) (and (x1 . > . x-max) (x2 . > . x-max))
              (and (y1 . < . y-min) (y2 . < . y-min)) (and (y1 . > . y-max) (y2 . > . y-max)))
      (return #f #f))
    (let ([x1  (max (min x1 x-max) x-min)]
          [x2  (max (min x2 x-max) x-min)]
          [y1  (max (min y1 y-max) y-min)]
          [y2  (max (min y2 y-max) y-min)])
      (values (vector x1 y1) (vector x2 y2)))))

;; ===================================================================================================
;; Line clipping

(define (clip-line-x start-in-bounds? x x1 y1 x2 y2)
  (define t (/ (- x x1) (- x2 x1)))
  (cond [start-in-bounds?  (values x1 y1 x (+ y1 (* t (- y2 y1))))]
        [else              (values x (+ y1 (* t (- y2 y1))) x2 y2)]))

(define (clip-line-y start-in-bounds? y x1 y1 x2 y2)
  (define t (/ (- y y1) (- y2 y1)))
  (cond [start-in-bounds?  (values x1 y1 (+ x1 (* t (- x2 x1))) y)]
        [else              (values (+ x1 (* t (- x2 x1))) y x2 y2)]))

(define (clip-line-x-min x-min x1 y1 x2 y2)
  (cond [(and (x1 . >= . x-min) (x2 . < . x-min))  (clip-line-x #t x-min x1 y1 x2 y2)]
        [(and (x2 . >= . x-min) (x1 . < . x-min))  (clip-line-x #f x-min x1 y1 x2 y2)]
        [else                                      (values x1 y1 x2 y2)]))

(define (clip-line-x-max x-max x1 y1 x2 y2)
  (cond [(and (x1 . <= . x-max) (x2 . > . x-max))  (clip-line-x #t x-max x1 y1 x2 y2)]
        [(and (x2 . <= . x-max) (x1 . > . x-max))  (clip-line-x #f x-max x1 y1 x2 y2)]
        [else                                      (values x1 y1 x2 y2)]))

(define (clip-line-y-min y-min x1 y1 x2 y2)
  (cond [(and (y1 . >= . y-min) (y2 . < . y-min))  (clip-line-y #t y-min x1 y1 x2 y2)]
        [(and (y2 . >= . y-min) (y1 . < . y-min))  (clip-line-y #f y-min x1 y1 x2 y2)]
        [else                                      (values x1 y1 x2 y2)]))

(define (clip-line-y-max y-max x1 y1 x2 y2)
  (cond [(and (y1 . <= . y-max) (y2 . > . y-max))  (clip-line-y #t y-max x1 y1 x2 y2)]
        [(and (y2 . <= . y-max) (y1 . > . y-max))  (clip-line-y #f y-max x1 y1 x2 y2)]
        [else                                      (values x1 y1 x2 y2)]))

(define (clip-line v1 v2 x-min x-max y-min y-max)
  (let/ec return
    ; early accept: both endpoints in bounds
    (when (and (point-in-bounds? v1 x-min x-max y-min y-max)
               (point-in-bounds? v2 x-min x-max y-min y-max))
      (return v1 v2))
    ; early reject: both endpoints on the outside of the same plane
    (match-define (vector x1 y1) v1)
    (match-define (vector x2 y2) v2)
    (when (or (and (x1 . < . x-min) (x2 . < . x-min)) (and (x1 . > . x-max) (x2 . > . x-max))
              (and (y1 . < . y-min) (y2 . < . y-min)) (and (y1 . > . y-max) (y2 . > . y-max)))
      (return #f #f))
    (let*-values ([(x1 y1 x2 y2)  (clip-line-x-min x-min x1 y1 x2 y2)]
                  [(x1 y1 x2 y2)  (clip-line-x-max x-max x1 y1 x2 y2)]
                  [(x1 y1 x2 y2)  (clip-line-y-min y-min x1 y1 x2 y2)]
                  [(x1 y1 x2 y2)  (clip-line-y-max y-max x1 y1 x2 y2)])
      (values (vector x1 y1) (vector x2 y2)))))

;; ===================================================================================================
;; Polygon clipping

(define ((make-clip-polygon idx test? clip-line) val vs)
  (reverse
   (for/fold ([res empty]) ([v1  (in-list (cons (last vs) vs))] [v2  (in-list vs)])
     (define v1-in-bounds? (test? (vector-ref v1 idx) val))
     (define v2-in-bounds? (test? (vector-ref v2 idx) val))
     (cond [(and v1-in-bounds? v2-in-bounds?)              (cons v2 res)]
           [(and (not v1-in-bounds?) (not v2-in-bounds?))  res]
           [else  (match-define (vector x1 y1) v1)
                  (match-define (vector x2 y2) v2)
                  (let-values ([(x1 y1 x2 y2)  (clip-line v1-in-bounds? val x1 y1 x2 y2)])
                    (cond [v2-in-bounds?  (list* (vector x2 y2) (vector x1 y1) res)]
                          [else           (cons (vector x2 y2) res)]))]))))

(define clip-polygon-x-min (make-clip-polygon 0 >= clip-line-x))
(define clip-polygon-x-max (make-clip-polygon 0 <= clip-line-x))
(define clip-polygon-y-min (make-clip-polygon 1 >= clip-line-y))
(define clip-polygon-y-max (make-clip-polygon 1 <= clip-line-y))

(define (clip-polygon vs x-min x-max y-min y-max)
  (let/ec return
    ; early reject: no polygon
    (when (empty? vs) (return empty))
    ; early accept: all endpoints in bounds
    (when (andmap (λ (v) (point-in-bounds? v x-min x-max y-min y-max)) vs)
      (return vs))
    (match-define (list (vector xs ys) ...) vs)
    ; early reject: all endpoints on the outside of the same plane
    (when (or (andmap (λ (x) (x . < . x-min)) xs) (andmap (λ (x) (x . > . x-max)) xs)
              (andmap (λ (y) (y . < . y-min)) ys) (andmap (λ (y) (y . > . y-max)) ys))
      (return empty))
    (let* ([vs  (clip-polygon-x-min x-min vs)]
           [_   (when (empty? vs) (return empty))]
           [vs  (clip-polygon-x-max x-max vs)]
           [_   (when (empty? vs) (return empty))]
           [vs  (clip-polygon-y-min y-min vs)]
           [_   (when (empty? vs) (return empty))]
           [vs  (clip-polygon-y-max y-max vs)])
      vs)))

;; =============================================================================
;; Lines clipping

;; This could be done a lot faster...

(define (join-lines lines [current-line empty])
  (cond [(empty? lines)         (list (reverse current-line))]
        [(empty? current-line)  (join-lines (rest lines)
                                            (reverse (first lines)))]
        [else
         (match-define v (first current-line))
         (match-define (list v1 v2) (first lines))
         (cond [(equal? v v1)  (join-lines (rest lines) (cons v2 current-line))]
               [else (cons (reverse current-line)
                           (join-lines lines empty))])]))

#;
(join-lines
 '((#(1 2) #(3 4))
   (#(3 4) #(5 6))
   (#(5 7) #(7 8))
   (#(7 8) #(9 10))))

(define (clip-lines vs x-min x-max y-min y-max)
  (if (empty? vs)
      empty
      (join-lines
       (reverse
        (for/fold ([res empty]) ([v1  (in-list vs)] [v2  (in-list (rest vs))])
          (let-values ([(v1 v2)  (clip-line v1 v2 x-min x-max y-min y-max)])
            (if (and v1 v2)
                (cons (list v1 v2) res)
                res)))))))
