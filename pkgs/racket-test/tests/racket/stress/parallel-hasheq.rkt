#lang racket/base

(define shared-ht (make-hasheq))

(define (check pool)
  (for-each
   thread-wait
   (for/list ([j (in-range 8)])
     (thread #:pool pool
             (lambda ()
               (for ([i (in-range 1000)])
                 (define items
                   (for/list ([i (in-range 10)])
                     (cons 1 2)))
                 (for ([item (in-list items)])
                   (hash-set! shared-ht item item))
                 (for ([item (in-list items)])
                   (unless (eq? item (hash-ref shared-ht item #f))
                     (error "missing")))
                 (for ([item (in-list items)])
                   (hash-remove! shared-ht item))
                 (for ([item (in-list items)])
                   (when (hash-ref shared-ht item #f)
                     (error "still there")))))))))

(check #f)
(check 'own)

(define (atomicity-check)
  (define progress 0)
  (define ts
    (for/list ([j (in-range 8)])
      (define r (current-pseudo-random-generator))
      (thread (lambda ()
                (for ([i (in-range 1000)])
                  (define items
                    (for/list ([i (in-range 10)])
                      (cons 1 2)))
                  (for ([item (in-list items)])
                    (hash-set! shared-ht item item))
                  (for ([item (in-list items)])
                    (unless (eq? item (hash-ref shared-ht item #f))
                      (error "missing")))
                  (for ([i (in-range (random 100 r))])
                    (set! progress (add1 progress)))
                  (for ([item (in-list items)])
                    (hash-remove! shared-ht item))
                  (for ([item (in-list items)])
                    (when (hash-ref shared-ht item #f)
                      (error "still there"))))))))
  (let loop ()
    (unless (progress . > . 100)
      (loop)))
  (define suspend-is '(1 3 5))
  (for ([suspend-i (in-list suspend-is)])
    (thread-suspend (list-ref ts suspend-i)))
  (for ([t (in-list ts)]
        [i (in-naturals)])
    (unless (memv i suspend-is)
      (thread-wait t)))
  (for ([suspend-i (in-list suspend-is)])
    (thread-resume (list-ref ts suspend-i))
    (thread-wait (list-ref ts suspend-i))))

(for ([i (in-range 100)])
  (atomicity-check))
