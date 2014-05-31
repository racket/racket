#lang racket/base
(require racket/list
         racket/match
         racket/contract/base
         "status.rkt")

(define (log-divide l)
  (match l
    [(list)
     (list empty)]
    [(cons s l)
     (define rl (log-divide l))
     (match-define (cons trl rrl) rl)
     (match s
       [(? stdout?)
        (match trl
          [(or (list) (cons (? stdout?) _))
           (cons (cons s trl) rrl)]
          [_
           (cons (list s) rl)])]
       [(? stderr?)
        (cons (cons s trl) rrl)])]))

(module+ test
  (require rackunit)
  (check-equal?
   (log-divide empty)
   (list empty))
  (check-equal?
   (log-divide (list (stdout #"1")))
   (list (list (stdout #"1"))))
  (check-equal?
   (log-divide (list (stdout #"1") (stdout #"2")))
   (list (list (stdout #"1") (stdout #"2"))))
  (check-equal?
   (log-divide (list (stdout #"1") (stderr #"A") (stdout #"2")))
   (list (list (stdout #"1"))
         (list (stderr #"A") (stdout #"2"))))
  (check-equal?
   (log-divide (list (stdout #"1") (stderr #"A") (stderr #"B") (stdout #"2")))
   (list (list (stdout #"1"))
         (list (stderr #"A") (stderr #"B") (stdout #"2")))))

(provide
 (contract-out
  [log-divide
   (-> (listof event?)
       (listof (listof event?)))]))
