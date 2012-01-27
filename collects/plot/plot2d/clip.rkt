#lang racket/base

;; Small library for clipping points, rectangles, lines and polygons against axial planes.

(require racket/match racket/list
         "../common/utils.rkt")

(provide point-in-bounds? clip-line clip-lines clip-polygon)

;; ===================================================================================================
;; Point clipping

(define (point-in-bounds? v x-min x-max y-min y-max)
  (match-define (vector x y) v)
  (and (<= x-min x x-max) (<= y-min y y-max)))

;; ===================================================================================================
;; Line clipping

(define (clip-line-x start-in-bounds? x x1 y1 x2 y2)
  (let-map
   (x x1 y1 x2 y2) inexact->exact
   (define t (/ (- x x1) (- x2 x1)))
   (cond [start-in-bounds?  (values x1 y1 x (+ y1 (* t (- y2 y1))))]
         [else              (values x (+ y1 (* t (- y2 y1))) x2 y2)])))

(define (clip-line-y start-in-bounds? y x1 y1 x2 y2)
  (let-map
   (y x1 y1 x2 y2) inexact->exact
   (define t (/ (- y y1) (- y2 y1)))
   (cond [start-in-bounds?  (values x1 y1 (+ x1 (* t (- x2 x1))) y)]
         [else              (values (+ x1 (* t (- x2 x1))) y x2 y2)])))

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
    (match-define (vector x1 y1) v1)
    (match-define (vector x2 y2) v2)
    ;; early accept: both endpoints in bounds
    (when (and (<= x-min x1 x-max) (<= y-min y1 y-max)
               (<= x-min x2 x-max) (<= y-min y2 y-max))
      (return v1 v2))
    ;; early reject: both endpoints on the outside of the same plane
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

(define-syntax-rule (make-clip-polygon in-bounds? clip-line)
  (λ (val xs ys)
    (cond [(empty? xs)  (values empty empty)]
          [else
           (for/fold ([res-xs empty] [res-ys empty]) ([x1  (in-list (cons (last xs) xs))]
                                                      [x2  (in-list xs)]
                                                      [y1  (in-list (cons (last ys) ys))]
                                                      [y2  (in-list ys)])
             (define v1-in-bounds? (in-bounds? x1 y1 val))
             (define v2-in-bounds? (in-bounds? x2 y2 val))
             (cond [(and v1-in-bounds? v2-in-bounds?)              (values (cons x2 res-xs)
                                                                           (cons y2 res-ys))]
                   [(and (not v1-in-bounds?) (not v2-in-bounds?))  (values res-xs res-ys)]
                   [else  (let-values ([(x1 y1 x2 y2)  (clip-line v1-in-bounds? val x1 y1 x2 y2)])
                            (cond [v2-in-bounds?  (values (list* x2 x1 res-xs)
                                                          (list* y2 y1 res-ys))]
                                  [else           (values (cons x2 res-xs) (cons y2 res-ys))]))]))])))

(define-syntax-rule (x-min-in-bounds? x y x-min) (x . >= . x-min)) 
(define-syntax-rule (x-max-in-bounds? x y x-max) (x . <= . x-max))
(define-syntax-rule (y-min-in-bounds? x y y-min) (y . >= . y-min))
(define-syntax-rule (y-max-in-bounds? x y y-max) (y . <= . y-max))

(define clip-polygon-x-min (make-clip-polygon x-min-in-bounds? clip-line-x))
(define clip-polygon-x-max (make-clip-polygon x-max-in-bounds? clip-line-x))
(define clip-polygon-y-min (make-clip-polygon y-min-in-bounds? clip-line-y))
(define clip-polygon-y-max (make-clip-polygon y-max-in-bounds? clip-line-y))

(define (clip-polygon vs x-min x-max y-min y-max)
  (let/ec return
    ;; early reject: no polygon
    (when (empty? vs) (return empty))
    (match-define (list (vector xs ys) ...) vs)
    ;; early accept: all endpoints in bounds
    (when (and (andmap (λ (x) (<= x-min x x-max)) xs)
               (andmap (λ (y) (<= y-min y y-max)) ys))
      (return vs))
    ;; early reject: all endpoints on the outside of the same plane
    (when (or (andmap (λ (x) (x . < . x-min)) xs) (andmap (λ (x) (x . > . x-max)) xs)
              (andmap (λ (y) (y . < . y-min)) ys) (andmap (λ (y) (y . > . y-max)) ys))
      (return empty))
    (let*-values ([(xs ys)  (clip-polygon-x-min x-min xs ys)]
                  [(xs ys)  (clip-polygon-x-max x-max xs ys)]
                  [(xs ys)  (clip-polygon-y-min y-min xs ys)]
                  [(xs ys)  (clip-polygon-y-max y-max xs ys)])
      (map vector xs ys))))

;; ===================================================================================================
;; Lines clipping

(define (join-lines lines)
  (let loop ([lines lines] [current-line empty])
    (cond [(empty? lines)         (list (reverse current-line))]
          [(empty? current-line)  (loop (rest lines) (reverse (first lines)))]
          [else
           (match-define v (first current-line))
           (match-define (list v1 v2) (first lines))
           (cond [(equal? v v1)  (loop (rest lines) (cons v2 current-line))]
                 [else           (cons (reverse current-line) (loop lines empty))])])))

#;
(join-lines
 '((#(1 2) #(3 4))
   (#(3 4) #(5 6))
   (#(5 7) #(7 8))
   (#(7 8) #(9 10))))

(define (clip-lines vs x-min x-max y-min y-max)
  (let/ec return
    ;; early reject: no lines
    (when (empty? vs) (return empty))
    (match-define (list (vector xs ys) ...) vs)
    ;; early accept: all endpoints in bounds
    (when (and (andmap (λ (x) (<= x-min x x-max)) xs) (andmap (λ (y) (<= y-min y y-max)) ys))
      (return (list vs)))
    ;; early reject: all endpoints on the outside of the same plane
    (when (or (andmap (λ (x) (x . < . x-min)) xs) (andmap (λ (x) (x . > . x-max)) xs)
              (andmap (λ (y) (y . < . y-min)) ys) (andmap (λ (y) (y . > . y-max)) ys))
      (return empty))
    (join-lines
     (reverse
      (for/fold ([res empty]) ([v1  (in-list vs)] [v2  (in-list (rest vs))])
        (let-values ([(v1 v2)  (clip-line v1 v2 x-min x-max y-min y-max)])
          (cond [(and v1 v2)  (cons (list v1 v2) res)]
                [else  res])))))))
