#lang typed/racket/base

(require racket/list
         "../../../flonum.rkt")

(provide Walker-Table make-walker-table walker-table-sample)

(define-type (Walker-Entry A) (Pair (Pair A Float) (U Null (List (Pair A Float)))))
(define-type (Walker-Table A) (Vectorof (Walker-Entry A)))

(: make-walker-table (All (A) ((Listof A) (Listof Float) -> (Walker-Table A))))
(define (make-walker-table xs ws)
  (define n (length xs))
  (define m (length ws))
  (unless (= n m)
    (error 'make-walker-table "values and weights aren't the same length; given lengths ~e and ~e"
           n m))
  (when (zero? n)
    (raise-argument-error 'make-walker-table "nonempty (Listof A)" 0 xs ws))
  (define xws ((inst map (Pair A Flonum) A Flonum) cons xs ws))
  (define total-weight (flsum ws))
  (define bin-weight (/ total-weight n))
  (define small-xws (filter (λ: ([xw : (Pair A Float)]) ((cdr xw) . fl< . bin-weight)) xws))
  (define large-xws (filter (λ: ([xw : (Pair A Float)]) ((cdr xw) . fl>= . bin-weight)) xws))
  (list->vector
   (let: loop : (Listof (Walker-Entry A))
     ([small-xws : (Listof (Pair A Float))  small-xws]
      [large-xws : (Listof (Pair A Float))  large-xws])
     (cond [(null? small-xws)  (map (λ: ([xws : (Pair A Float)]) (list xws)) large-xws)]
           [(null? large-xws)  (map (λ: ([xws : (Pair A Float)]) (list xws)) small-xws)]
           [else
            (define small-x (car (first small-xws)))
            (define small-w (cdr (first small-xws)))
            (define large-x (car (first large-xws)))
            (define large-w (cdr (first large-xws)))
            (define underweight (fl- bin-weight small-w))
            (define new-large-w (fl- large-w underweight))
            (cons (list (cons small-x small-w) (cons large-x underweight))
                  (if (new-large-w . fl< . bin-weight)
                      (loop (cons (cons large-x new-large-w) (rest small-xws))
                            (rest large-xws))
                      (loop (rest small-xws)
                            (cons (cons large-x new-large-w)
                                  (rest large-xws)))))]))))

(: walker-table-sample (All (A) ((Walker-Table A) -> A)))
(define (walker-table-sample vec)
  (define len (vector-length vec))
  (cond [(zero? len)  (raise-argument-error 'walker-table-sample "nonempty Walker-Table" vec)]
        [else
         (define i (random len))
         (define xws (vector-ref vec i))
         (define rest-xws (rest xws))
         (cond [(empty? rest-xws)  (car (first xws))]
               [else
                (define xw1 (car xws))
                (define x1 (car xw1))
                (define w1 (cdr xw1))
                (define xw2 (car rest-xws))
                (define x2 (car xw2))
                (define w2 (cdr xw2))
                (define r (fl* (random) (fl+ w1 w2)))
                (if (r . fl< . w1) x1 x2)])]))
