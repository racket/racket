#lang scheme
(require redex)

(define-language lang
  (e (binop e e)
     (sqrt e)
     number)
  (binop +
         -
         *
         /)
  
  (e-ctxt (binop e e-ctxt)
          (binop e-ctxt e)
          (sqrt e-ctxt)
          hole)
  (v number))

(define reductions
  (reduction-relation
   lang
   (c--> (+ number_1 number_2)
         ,(+ (term number_1) (term number_2))
         "add")
   (c--> (- number_1 number_2)
         ,(- (term number_1) (term number_2))
         "subtract")
   (c--> (* number_1 number_2)
         ,(* (term number_1) (term number_2))
         "multiply")
   (c--> (/ number_1 number_2)
         ,(/ (term number_1) (term number_2))
         "divide")
   (c-->(sqrt number_1)
        ,(sqrt (term number_1))
        "sqrt")
   with
   [(--> (in-hole e-ctxt_1 a) (in-hole e-ctxt_1 b))
    (c--> a b)]))

(define traces-file
  (make-temporary-file "traces~a.ps"))

(traces/ps reductions (term (- (* (sqrt 36) (/ 1 2)) (+ 1 2)))
           traces-file)

(printf "Traces are in ~a~n" traces-file)
