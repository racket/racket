#lang typed/racket/base

(require "../../../flonum.rkt"
         "../../unsafe.rkt")

(provide Walker-Table make-walker-table walker-table-sample)

(define-type (Walker-Entry A) (U (List (Pair A Float)) (List (Pair A Float) (Pair A Float))))
(define-type (Walker-Table A) (Vectorof (Walker-Entry A)))

(: make-walker-table (All (A) ((Listof (Pair A Float)) -> (Walker-Table A))))
(define (make-walker-table xws)
  (define n (length xws))
  (define xs (map (ann car ((Pair A Float) -> A)) xws))
  (define ws (map (ann cdr ((Pair A Float) -> Float)) xws))
  (define total-weight (apply + ws))
  (define bin-weight (/ total-weight n))
  (define small-xws (filter (λ: ([xw : (cons A Float)]) ((cdr xw) . fl< . bin-weight)) xws))
  (define large-xws (filter (λ: ([xw : (cons A Float)]) ((cdr xw) . fl>= . bin-weight)) xws))
  (vector->immutable-vector
   (list->vector
    (let: loop : (Listof (Walker-Entry A))
      ([small-xws : (Listof (Pair A Float))  small-xws]
       [large-xws : (Listof (Pair A Float))  large-xws])
      (cond [(null? small-xws)  (map (λ: ([xws : (Pair A Float)]) (list xws)) large-xws)]
            [(null? large-xws)  (map (λ: ([xws : (Pair A Float)]) (list xws)) small-xws)]
            [else
             (define small-x (car (car small-xws)))
             (define small-w (cdr (car small-xws)))
             (define large-x (car (car large-xws)))
             (define large-w (cdr (car large-xws)))
             (define underweight (fl- bin-weight small-w))
             (define new-large-w (fl- large-w underweight))
             (cons (list (cons small-x small-w) (cons large-x underweight))
                   (if (new-large-w . fl< . bin-weight)
                       (loop (cons (cons large-x new-large-w) (cdr small-xws))
                             (cdr large-xws))
                       (loop (cdr small-xws)
                             (cons (cons large-x new-large-w)
                                   (cdr large-xws)))))])))))

(: walker-table-sample (All (A) ((Walker-Table A) -> A)))
(define (walker-table-sample vec)
  (define len (vector-length vec))
  (cond [(zero? len)  (raise-argument-error 'walker-table-sample "nonempty Walker-Table" vec)]
        [else
         (define i (random len))
         (define xws (unsafe-vector-ref vec i))
         (cond [(= 1 (length xws))  (unsafe-car (unsafe-car xws))]
               [else
                (define xw1 (unsafe-car xws))
                (define x1 (unsafe-car xw1))
                (define w1 (unsafe-cdr xw1))
                (define xw2 (unsafe-car (unsafe-cdr xws)))
                (define x2 (unsafe-car xw2))
                (define w2 (unsafe-cdr xw2))
                (define r (fl* (random) (fl+ w1 w2)))
                (if (r . fl< . w1) x1 x2)])]))

#|
(: sample/replace (All (A) ((Listof (Pair A Float)) Integer -> (Listof A))))
(define (sample/replace xws n)
  (when (negative? n)
    (raise-type-error 'sample/replace "nonnegative integer" n))
  (define t (build-walker-table xws))
  (build-list n (λ _ (walker-table-sample t))))

(: proportion-equal? (All (A) ((Listof A) A -> Exact-Rational)))
(define (proportion-equal? xs x0)
  (/ (length (filter (λ: ([x : A]) (equal? x x0)) xs)) (length xs)))

(define xs1 (time (sample/replace '((a . 1.0) (b . 1.0) (c . 1.0)) 10000)))
;; 21ms
(map (λ (x) (exact->inexact (proportion-equal? xs1 x))) '(a b c))

(define xs2 (time (sample/replace '((a . 1.0) (b . 1.0) (c . 4.0)) 10000)))
;; 32ms
(map (λ (x) (exact->inexact (proportion-equal? xs2 x))) '(a b c))

(define xs3 (time (sample/replace '((a . 1.0) (b . 1.0) (c . 1.0) (d . 5.0))
                                  10000)))
;; 33ms
(map (λ (x) (exact->inexact (proportion-equal? xs3 x))) '(a b c d))

(define xs4 (time (sample/replace '((a . 1.0) (b . 1.0) (c . 1.0) (d . 16.0))
                                  10000)))
;; 33ms
(map (λ (x) (exact->inexact (proportion-equal? xs4 x))) '(a b c d))

(define xs5 (time (sample/replace '((a . 1.0)
                                    (b . 2.0)
                                    (c . 4.0)
                                    (d . 8.0)
                                    (e . 16.0)
                                    (f . 32.0)
                                    (g . 64.0)
                                    (h . 128.0))
                                  100000)))
;; 584ms
(map (λ (x) (exact->inexact (proportion-equal? xs5 x))) '(a b c d e f g h))
|#
