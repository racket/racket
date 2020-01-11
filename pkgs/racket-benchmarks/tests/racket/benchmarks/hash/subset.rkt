#lang racket/base
(require "config.rkt"
         racket/list)

(define elems
  (parameterize ([current-pseudo-random-generator
                  (make-pseudo-random-generator)])
    (random-seed 12745)
    (shuffle
     (hash-keys
      (for/fold ([ht #hasheqv()]) ([i 200])
        (let loop ()
          (define n (random 10000))
          (if (hash-ref ht n #f)
              (loop)
              (hash-set ht n #t))))))))
      
(define (gen n)
  (for/hasheq ([i (in-range n)]
               [e (in-list elems)])
    (values e e)))

(define (gen-more n ht)
  (for/fold ([ht ht]) ([i (in-range n)]
                       [e (in-list (list-tail elems (hash-count ht)))])
    (hash-set ht e e)))

(define (check-true v)
  (unless v
    (error "failed")))

(define (check-false v)
  (when v
    (error "failed")))

'subset-lil-shared:eq
(times
 (let* ([sub-ht (gen 6)]
        [ht (gen-more 3 sub-ht)])
   (for ([i (in-range H)])
     (check-true
      (for/and ([i (in-range M)])
        (hash-keys-subset? sub-ht ht))))))

'subset-lil-unshared:eq
(times
 (let ([ht (gen 6)]
       [sub-ht (gen 3)])
   (for ([i (in-range H)])
     (check-true
      (for/and ([i (in-range M)])
        (hash-keys-subset? sub-ht ht))))))

'subset-lil-not:eq
(times
 (let ([ht (gen 6)]
       [sub-ht (gen 3)])
   (for ([i (in-range H)])
     (check-false
      (for/or ([i (in-range L)])
        (hash-keys-subset? ht sub-ht))))))

'subset-med+lil-shared:eq
(times
 (let* ([sub-ht (gen 10)]
        [ht (gen-more 1 sub-ht)])
   (check-true
    (for/and ([i (in-range L)])
      (hash-keys-subset? sub-ht ht)))))

'subset-med+med-shared:eq
(times
 (let* ([sub-ht (gen 10)]
        [ht (gen-more 10 sub-ht)])
   (check-true
    (for/and ([i (in-range L)])
      (hash-keys-subset? sub-ht ht)))))

'subset-big-same:eq
(times
 (for ([i (in-range H)])
   (let* ([sub-ht (gen 100)])
     (check-true
      (for/and ([i (in-range L)])
        (hash-keys-subset? sub-ht sub-ht))))))

'subset-big+lil-shared:eq
(times
 (let* ([sub-ht (gen 100)]
        [ht (gen-more 3 sub-ht)])
   (check-true
    (for/and ([i (in-range L)])
      (hash-keys-subset? sub-ht ht)))))

'subset-big+med-shared:eq
(times
 (let* ([sub-ht (gen 100)]
        [ht (gen-more 10 sub-ht)])
   (check-true
    (for/and ([i (in-range M)])
      (hash-keys-subset? sub-ht ht)))))

;; This one amounts to a test of how fast the subset
;; operation iterates internally:
'subset-big-unshared:eq
(times
 (let* ([sub-ht (gen 100)]
        [ht (gen 100)])
   (check-true
    (for/and ([i (in-range M)])
      (hash-keys-subset? sub-ht ht)))))
