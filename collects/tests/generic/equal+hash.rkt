#lang racket/base

;; vectors as method tables
(struct kons (kar kdr)
        #:methods gen:equal+hash
        [(define (equal-proc x y rec)
           (and (rec (kons-kar x) (kons-kar y))
                (rec (kons-kdr x) (kons-kdr y))))
         (define (hash-proc x rec)  12)
         (define (hash2-proc x rec) 13)])

(module+ test
  (require rackunit)

  (check-equal? (kons 1 2) (kons 1 2))
  (check-false (equal? (kons 1 2) 2))
  (check-false (equal? 2 (kons 1 2)))
  (check-false (equal? (kons 1 2) (kons 3 4)))
  (check-equal? (equal-hash-code (kons 1 2)) 60)
  )
