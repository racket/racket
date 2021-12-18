#lang racket/base

;; vectors as method tables
(struct kons (kar kdr)
        #:methods gen:equal+hash
        [(define (equal-proc x y rec)
           (and (rec (kons-kar x) (kons-kar y))
                (rec (kons-kdr x) (kons-kdr y))))
         (define (hash-proc x rec)  12)
         (define (hash2-proc x rec) 13)])

(struct mkons (kar kdr) #:mutable
  #:methods gen:equal+hash
  [(define (equal-proc x y rec)
     (and (rec (mkons-kar x) (mkons-kar y))
          (rec (mkons-kdr x) (mkons-kdr y))))
   (define (hash-proc x rec)  12)
   (define (hash2-proc x rec) 13)])

(module+ test
  (require rackunit)

  (test-case "kons as an immutable pair"
    (check-equal? (kons 1 2) (kons 1 2))
    (check-false (equal? (kons 1 2) 2))
    (check-false (equal? 2 (kons 1 2)))
    (check-false (equal? (kons 1 2) (kons 3 4)))
    (check-equal? (equal-hash-code (kons 1 2))
                  (equal-hash-code (kons 1 2)))
    (check equal-always? (kons 1 2) (kons 1 2))
    (check-false (equal-always? (kons 1 2) 2))
    (check-false (equal-always? 2 (kons 1 2)))
    (check-false (equal-always? (kons 1 2) (kons 3 4)))
    (check-equal? (equal-always-hash-code (kons 1 2))
                  (equal-always-hash-code (kons 1 2))))

  (test-case "mkons as a mutable pair"
    (check-equal? (mkons 1 2) (mkons 1 2))
    (check-false (equal-always? (mkons 1 2) (mkons 1 2)))
    (check-false (equal? (mkons 1 2) 2))
    (check-false (equal? 2 (mkons 1 2)))
    (check-false (equal? (mkons 1 2) (mkons 3 4)))
    (check-false (equal-always? (mkons 1 2) 2))
    (check-false (equal-always? 2 (mkons 1 2)))
    (check-false (equal-always? (mkons 1 2) (mkons 3 4)))
    (check-equal? (equal-hash-code (mkons 1 2))
                  (equal-hash-code (mkons 1 2)))
    (check-false (equal? (equal-always-hash-code (mkons 1 2))
                         (equal-always-hash-code (mkons 1 2)))))
  )
