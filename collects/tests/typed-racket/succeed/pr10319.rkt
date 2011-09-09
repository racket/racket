
#lang typed-scheme

(define-type-alias LSN (Rec LSN (U '() (cons Number LSN) (cons Symbol LSN))))

(: sum (LSN -> Number))
;; add all numbers in this lsn
(define (sum lsn)
  (cond
    [(null? lsn) 0]
    [(number? (car lsn)) (+ (car lsn) (sum (cdr lsn)))]
    [else (sum (cdr lsn))]))

(sum '(a b 2 3))
