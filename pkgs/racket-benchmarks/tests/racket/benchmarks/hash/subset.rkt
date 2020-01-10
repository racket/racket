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

'eq:subset-shared-small
(times
 (let* ([sub-ht (gen 6)]
        [ht (gen-more 3 sub-ht)])
   (check-true
    (for/and ([i (in-range M)])
      (hash-keys-subset? sub-ht ht)))))

'eq:subset-unshared-small
(times
 (let ([ht (gen 6)]
       [sub-ht (gen 3)])
   (check-true
    (for/and ([i (in-range M)])
      (hash-keys-subset? sub-ht ht)))))

'eq:not-subset-unshared-small
(times
 (let ([ht (gen 6)]
       [sub-ht (gen 3)])
   (check-false
    (for/or ([i (in-range L)])
      (hash-keys-subset? ht sub-ht)))))

'eq:subset-shared-medium+small
(times
 (let* ([sub-ht (gen 10)]
        [ht (gen-more 1 sub-ht)])
   (check-true
    (for/and ([i (in-range L)])
      (hash-keys-subset? sub-ht ht)))))

'eq:subset-shared-medium+medium
(times
 (let* ([sub-ht (gen 10)]
        [ht (gen-more 10 sub-ht)])
   (check-true
    (for/and ([i (in-range L)])
      (hash-keys-subset? sub-ht ht)))))

'eq:subset-same-large
(times
 (let* ([sub-ht (gen 100)])
   (check-true
    (for/and ([i (in-range L)])
      (hash-keys-subset? sub-ht sub-ht)))))

'eq:subset-shared-large+small
(times
 (let* ([sub-ht (gen 100)]
        [ht (gen-more 3 sub-ht)])
   (check-true
    (for/and ([i (in-range L)])
      (hash-keys-subset? sub-ht ht)))))

'eq:subset-shared-large+medium
(times
 (let* ([sub-ht (gen 100)]
        [ht (gen-more 10 sub-ht)])
   (check-true
    (for/and ([i (in-range M)])
      (hash-keys-subset? sub-ht ht)))))

;; This one amounts to a test of how fast the subset
;; operation iterates internally:
'eq:subset-unshared-large
(times
 (let* ([sub-ht (gen 100)]
        [ht (gen 100)])
   (check-true
    (for/and ([i (in-range M)])
      (hash-keys-subset? sub-ht ht)))))
