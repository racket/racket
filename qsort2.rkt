#lang racket/base

(require racket/future
         racket/future/visualizer
         rackunit
         racket/fixnum
         racket/unsafe/ops
         (only-in racket/list empty?)
         racket/vector
         rackunit)

(define (qsort v)
  (define length (vector-length v))
  (qsort-h v 0 (sub1 length)))

(define (qsort-h v l r)
  (when (unsafe-fx> (unsafe-fx- r l) 0)
    (define m (partition v l r))
    (qsort-h v l (unsafe-fx- m 1))
    (qsort-h v (unsafe-fx+ 1 m) r)))

(define (swap! v i j)
  (define temp (unsafe-vector-ref v j))
  (unsafe-vector-set! v j (unsafe-vector-ref v i))
  (unsafe-vector-set! v i temp))

(define (partition v l r)
  (swap! v l r)
  (define p (unsafe-vector-ref v r))
  ;moving this inline eliminates jit copilations
  ;but slows things down and makes things less parallel (factor of ~1.5)
  (let recur
     ([i l]
      [p-i l])
    (cond
      [(unsafe-fx= i r)
       (swap! v r p-i)
       p-i]
      [(unsafe-fx< (unsafe-vector-ref v i) p)
       (swap! v i p-i)
       (recur (unsafe-fx+ 1 i) (unsafe-fx+ p-i 1))]
      [else
       (recur (unsafe-fx+ 1 i) p-i)])))

;(define (partition v l r)
;  (let recur ([i (fx- l 1)]
;              [j l])
;    (cond
;      [(fx= j (fx- r 1))
;       (swap! v (fx+ i 1) r)
;       (fx+ i 1)]
;      [(fx< (unsafe-vector-ref v j) (unsafe-vector-ref v r))
;       (swap! v (fx+ i 1) j)
;       (recur (fx+ i 1) (fx+ j 1))]
;      [else
;       (recur i (fx+ j 1))])))

(define (qsort-f v)
  (define length (vector-length v))
  (qsort-f-h v 0 (sub1 length) 6))

(define (qsort-f-h v l r d)
  (when (unsafe-fx> (unsafe-fx- r l) 0)
    ;(define m (partition v l r))
    (if (unsafe-fx= d 0)
        (let ([m (partition v l r)])
          (qsort-h v l (unsafe-fx- m 1))
          (qsort-h v (unsafe-fx+ 1 m) r))
        (let* ([m (partition v l r)]
               [f1 (future (λ () (qsort-f-h v l (unsafe-fx- m 1) (unsafe-fx- d 1))))]
               [f2 (future (λ () (qsort-f-h v (unsafe-fx+ 1 m) r (unsafe-fx- d 1))))])
          (touch f1)
          (touch f2)))))

(define (check-sorting v)
  (define l (vector->list v))
  (define v1 (list->vector (sort l <)))
  (qsort-f v)
  (check-equal? v1 v))

(define (rand-v size) (build-vector size (λ (_) (random 30))))

;(check-sorting (rand-v 100))

(random-seed 2)

(define v1 (rand-v 50000))
(define v2 (vector-copy v1))

#;(time (begin (sort (vector->list v1) <) 
             'a))
#;(time (begin (qsort v1)
             'b))
(let ()
  (start-performance-tracking!)
  (time (qsort-f v2))
  (show-visualizer)
  'c)
