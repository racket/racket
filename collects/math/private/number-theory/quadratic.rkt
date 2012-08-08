#lang typed/racket
(provide quadratic-solutions
         quadratic-integer-solutions
         quadratic-natural-solutions)

(define-predicate exact-integer? Integer)
(define-predicate natural? Natural)

(: quadratic-solutions : Real Real Real -> (Listof Real))
(define (quadratic-solutions a b c)
  ; return list of solutions to a a x^2 + b x + c = 0
  (let ([d (- (* b b) (* 4 a c))])
    (cond
      [(< d 0) '()]
      [(= d 0) (list (/ b (* -2 a)))]
      [else
       (let ([sqrt-d (sqrt d)])
         (list (/ (- (- b) sqrt-d) (* 2 a))
               (/ (+ (- b) sqrt-d) (* 2 a))))])))

(: filter-integer : (Listof Real) -> (Listof Integer))
(define (filter-integer xs)
  (cond [(empty? xs) '()]
        [else
         (define x (car xs))
         (cond [(exact-integer? x) (cons x (filter-integer ((inst rest Real Real) xs)))]
               [else (filter-integer ((inst rest Real Real) xs))])]))

(: quadratic-integer-solutions : Real Real Real -> (Listof Integer))
(define (quadratic-integer-solutions a b c)
  ; return list of integer solutions to a x^2 + b x + c = 0
  (filter-integer (quadratic-solutions a b c)))

(: filter-natural : (Listof Real) -> (Listof Natural))
(define (filter-natural xs)
  (cond [(empty? xs) '()]
        [else
         (define x (car xs))
         (cond [(natural? x) (cons x (filter-natural ((inst rest Real Real) xs)))]
               [else (filter-natural ((inst rest Real Real) xs))])]))

(: quadratic-natural-solutions : Real Real Real -> (Listof Natural))
(define (quadratic-natural-solutions a b c)
  ; return list of integer solutions to a x^2 + b x + c = 0
  (filter-natural (quadratic-solutions a b c)))
