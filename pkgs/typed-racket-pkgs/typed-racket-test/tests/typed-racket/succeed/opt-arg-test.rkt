#lang typed/racket

(: f (case-> (-> Integer)
             (Integer -> Integer)))
(define (f [#{z : Integer} 0]) z)
#;
(define-values
  (f)
  (let-values (((#{core3 : (case-> (Integer True -> Integer)
                                   (Univ False -> Integer))})
                (lambda (z1 z2) (let-values (((#{z : Integer}) (if z2 z1 '0)))
                                  (let-values () z)))))
    (case-lambda (() (#%app core3 '#f '#f)) 
                 ((z1) (#%app core3 z1 '#t)))))


(add1 (f 0))
(add1 (f))
