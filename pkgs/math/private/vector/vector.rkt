#lang typed/racket/base

(require racket/fixnum
         racket/performance-hint
         "../unsafe.rkt")

(provide for/vector: for*/vector: vector-ref!)

(begin-encourage-inline
  
  (: vector-ref!
     (All (A B) (case-> ((Vectorof (U A False)) Integer (-> A) -> A)
                        ((Vectorof (U A B)) Integer (-> A) (Any -> Boolean : B) -> A))))
  (define vector-ref!
    (case-lambda
      [(vs i thnk)  (vector-ref! vs i thnk not)]
      [(vs i thnk nothing?)
       (define n (vector-length vs))
       (cond [(and (fixnum? i) (i . fx>= . 0) (i . fx< . n))
              (define v (unsafe-vector-ref vs i))
              (if (nothing? v)
                  (let ([v  (thnk)])
                    (unsafe-vector-set! vs i v)
                    v)
                  v)]
             [else
              (raise-range-error 'vector-ref! "Vector" "" i vs 0 n #f)])]))
  
  )  ; begin-encourage-inline
