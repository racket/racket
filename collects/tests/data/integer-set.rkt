#lang racket/base
(require data/integer-set
         racket/stream
         rackunit
         racket/sequence)

(test-equal? "integer-set"
             (integer-set-contents (make-integer-set '((-1 . 2) (4 . 10))))
             '((-1 . 2) (4 . 10)))

(test-true "integer-set?"
           (integer-set? (make-integer-set '((-1 . 2) (4 . 10)))))

(test-equal? "set-integer-set-contents!"
             (let ([s (make-integer-set '((-1 . 2) (4 . 10)))])
               (set-integer-set-contents! s '((1 . 1) (3 . 3)))
               (integer-set-contents s))
             '((1 . 1) (3 . 3)))

(test-true "well-formed-set? true"
           (well-formed-set? '((-1 . 2) (4 . 10))))

(test-false "well-formed-set? false"
            (well-formed-set? '((1 . 5) (6 . 7))))

(define s1 (make-integer-set '((-1 . 2) (4 . 10))))
(define s2 (make-integer-set '((1 . 1) (3 . 3))))

(check-true (member? 1 (intersect s1 s2)))
(check-false (member? 3 (intersect s1 s2)))

(check-true (member? 2 (union s1 s2)))
(check-false (member? 15 (union s1 s2)))

(check-true (member? 4 (subtract s1 s2)))
(check-false (member? 1 (subtract s1 s2)))

(check-true (member? 4 (symmetric-difference s1 s2)))
(check-true (member? 3 (symmetric-difference s1 s2)))
(check-false (member? 1 (symmetric-difference s1 s2)))

(check-equal? (count s1) 11)
(check-equal? (count s2) 2)

(check-true (member? (get-integer s1) s1))
(check-false (get-integer (make-integer-set '())))

(check-equal? (partition (list (make-integer-set '((1 . 2) (5 . 10)))
                               (make-integer-set '((2 . 2) (6 . 6) (12 . 12)))))
              (list (make-integer-set '((2 . 2) (6 . 6)))
                    (make-integer-set '((12 . 12)))
                    (make-integer-set '((1 . 1) (5 . 5) (7 . 10)))))

(check-true (subset? (make-integer-set '((1 . 1))) s2))

;; check gen:stream
(check-equal? (stream-first s1) -1)
(check-equal? (stream-first (stream-rest s1)) 0)
(check-equal? (stream-first (stream-rest s2)) 3)
(check-true (stream-empty? (make-integer-set '())))
(check-false (stream-empty? s1))
(check-equal? (stream->list (stream-map add1 s2)) '(2 4))

;; 2013-02-20: checks commit bd1141c670bfc7981761fbfb53f548c2abb1f12d
;; (previous version results in contract error)
(check-true (well-formed-set? (integer-set-contents (stream-rest (make-range 1 10)))))
(check-true (well-formed-set? (integer-set-contents (sequence-tail (make-range 1 10) 1))))
