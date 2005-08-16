(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

;; calc : AE -> number
;; to define a calculator for AEs

(define (calc an-ae)
  (type-case AE an-ae
             [num (n) n]
             [add (l r) (+ (calc l) (calc r))]
             [sub (l r) (- (calc l) (calc r))]))

(test (calc (num 3)) 3)
(test (calc (add (num 3) (num 4))) 7)
(test (calc (add (sub (num 3) (num 4)) (num 7))) 6)

;; parse : sexp -> AE
;; to convert s-expressions into AEs

(define (parse sexp)
  (cond
   [(number? sexp) (num sexp)]
   [(list? sexp)
    (case (first sexp)
      [(+) (add (parse (second sexp))
                     (parse (third sexp)))]
      [(-) (sub (parse (second sexp))
                     (parse (third sexp)))])]))

(test (calc (parse 3)) 3)
(test (calc (parse (list '+ 3 4))) 7)
(test (calc (parse (list '+ (list '- 3 4) 7))) 6)

