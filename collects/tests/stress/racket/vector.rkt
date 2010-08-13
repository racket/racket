#lang racket
(require tests/stress/stress
         racket/unsafe/ops)

; vector-set*!
(local [(define vec (make-vector 8001 #t))
        (define-syntax (inlined-vector-set*! stx)
          (syntax-case stx ()
            [(_ vec 0) #'(void)]
            [(_ vec n) #`(begin (inlined-vector-set*! vec #,(sub1 (syntax->datum #'n))) (vector-set! vec n #f))]))
        (define-syntax (fun-vector-set*! stx)
          (syntax-case stx ()
            [(_ vec n)
             #`(vector-set*! vec #,@(apply append
                                       (for/list ([i (in-range (syntax->datum #'n))])
                                         (list i #f))))]))]
  (stress
   20
   ; XXX if there was an unsafe-vector-set!/bounds, we could test vector? once
   ["inlined vector-set*!"
    (inlined-vector-set*! vec 8000)]
   ["for, vector-set!"
    (for ([i (in-range 8000)])
      (vector-set! vec i #f))]
   ["for, unsafe-vector-set!"
    (for ([i (in-range 8000)])
      (unsafe-vector-set! vec i #f))]
   ["for, unsafe-vector*-set!"
    (for ([i (in-range 8000)])
      (unsafe-vector*-set! vec i #f))]
   ["vector-set*!"
    (fun-vector-set*! vec 8000)]))
