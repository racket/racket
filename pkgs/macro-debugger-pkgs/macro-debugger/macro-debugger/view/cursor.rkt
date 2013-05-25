#lang racket/base
(provide cursor?
         cursor-position
         cursor:new
         cursor:add-to-end!
         cursor:remove-current!

         cursor:next
         cursor:prev

         cursor:at-start?
         cursor:at-end?

         cursor:has-next?
         cursor:has-prev?

         cursor:move-next
         cursor:move-prev
         cursor:move-to-start
         cursor:move-to-end
         cursor:skip-to

         cursor->list
         cursor:prefix->list
         cursor:suffix->list
         cursor-count)

(define-struct cursor (vector count position)
  #:mutable)

(define (cursor:ensure-capacity c capacity)
  (define v (cursor-vector c))
  (when (< (vector-length v) capacity)
    (let* ([new-capacity (ceiling (* capacity 3/2))]
           [new-v (make-vector new-capacity)])
      (vector-copy! new-v 0 v 0)
      (set-cursor-vector! c new-v))))

(define (cursor:new items)
  (define v (list->vector items))
  (make-cursor v (vector-length v) 0))

(define (cursor:add-to-end! c items)
  (define count0 (cursor-count c))
  (define items-vector (list->vector items))
  (cursor:ensure-capacity c (+ (cursor-count c) (length items)))
  (vector-copy! (cursor-vector c) count0 items-vector)
  (set-cursor-count! c (+ (cursor-count c) (vector-length items-vector))))

(define (cursor:remove-current! c)
  (cursor:remove-at! c (cursor-position c)))

(define (cursor:remove-at! c p)
  (define count (cursor-count c))
  (define v (cursor-vector c))
  (vector-copy! v p v (add1 p))
  (vector-set! v (sub1 count) #f)
  (set-cursor-count! c (sub1 count)))

(define (cursor:next c)
  (define p (cursor-position c))
  (define count (cursor-count c))
  (and (< p count)
       (vector-ref (cursor-vector c) p)))

(define (cursor:prev c)
  (define p (cursor-position c))
  (define count (cursor-count c))
  (and (< 0 p)
       (vector-ref (cursor-vector c) (sub1 p))))


(define (cursor:move-next c)
  (define p (cursor-position c))
  (define count (cursor-count c))
  (when (< p count)
    (set-cursor-position! c (add1 p))))

(define (cursor:move-prev c)
  (define p (cursor-position c))
  (define count (cursor-count c))
  (when (< 0 p)
    (set-cursor-position! c (sub1 p))))

(define (cursor:at-start? c)
  (= (cursor-position c) 0))

(define (cursor:at-end? c)
  (= (cursor-position c) (cursor-count c)))

(define (cursor:has-next? c)
  (not (cursor:at-end? c)))

(define (cursor:has-prev? c)
  (not (cursor:at-start? c)))

(define (cursor:move-to-start c)
  (set-cursor-position! c 0))

(define (cursor:move-to-end c)
  (set-cursor-position! c (cursor-count c)))

(define (cursor:skip-to c i)
  (when (<= 0 i (cursor-count c))
    (set-cursor-position! c i)))

(define (cursor->list c)
  (define count (cursor-count c))
  (define v (cursor-vector c))
  (let loop ([i 0])
    (if (< i count)
        (cons (vector-ref v i)
              (loop (add1 i)))
        null)))

(define (cursor:prefix->list c)
  (define position (cursor-position c))
  (define v (cursor-vector c))
  (let loop ([i 0])
    (if (< i position)
        (cons (vector-ref v i)
              (loop (add1 i)))
        null)))

(define (cursor:suffix->list c)
  (define position (cursor-position c))
  (define count (cursor-count c))
  (define v (cursor-vector c))
  (let loop ([i position])
    (if (< i count)
        (cons (vector-ref v i)
              (loop (add1 i)))
        null)))
