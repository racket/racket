#lang typed-scheme

(: make-recursive : (All (S T) (((S -> T) -> (S -> T)) -> (S -> T))))
(define (make-recursive f)
  (define-type-alias Tau (Rec Tau (Tau -> (S -> T))))
  ((lambda: ([x : Tau]) (f (lambda: ([z : S]) ((x x) z))))
   (lambda: ([x : Tau]) (f (lambda: ([z : S]) ((x x) z))))))

(: fact : (Number -> Number))
(define fact (make-recursive
              (lambda: ([fact : (Number -> Number)])
                (lambda: ([n : Number])
                  (if (zero? n)
                      1
                      (* n (fact (- n 1))))))))
