#lang typed/racket

(require "array-struct.rkt"
         "../unsafe.rkt")

(provide in-array)

(: in-array/proc : (All (A) ((Array A) -> (Sequenceof A))))
(define (in-array/proc a)
  (define v (strict-array-data (array-strict a)))
  (define n (vector-length v))
  (make-do-sequence
   (λ ()
     (values
      ; pos->element
      (λ: ([j : Index])
        (vector-ref v j))
      ; next-pos
      (λ: ([j : Index]) (assert (+ j 1) index?))
      ; initial-pos
      0
      ; continue-with-pos?
      (λ: ([j : Index ]) (< j n))
      #f #f))))

; (in-array a]
;     Returns a sequence of all elements of the array a.
(define-sequence-syntax in-array
  (λ () #'in-array/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ a-expr)]
       #'((x)
          (:do-in
           ([(a n d) 
             (let ([a1 a-expr])
               (define data (strict-array-data (array-strict a1)))
               (define n (vector-length data))
               (values a1 n data))])
           (begin 
             (unless (array? a) 
               (raise-type-error 'in-array "expected array, got ~a" a)))
           ([j 0])
           (< j n)
           ([(x) (vector-ref d j)])
           #true
           #true
           [(+ j 1)]))]
      ; for: does not support the indices in for:-clauses yet :-(
      #;[[(i x) (_ M-expr r-expr)]
         #'((i x)
          (:do-in
           ([(a n d) 
             (let ([a1 a-expr])
               (define data (strict-array-data (array-strict a1)))
               (define n (vector-length data))
               (values a1 n data))])
           (begin 
             (unless (array? a) 
               (raise-type-error 'in-array "expected array, got ~a" a)))
           ([j 0])
           (< j n)
           ([(x) (vector-ref d j)]
            [(i) j])
           #true
           #true
           [(+ j 1)]))]
      [[_ clause] (raise-syntax-error 
                   'in-array "expected (in-array <array>)" #'clause #'clause)])))

