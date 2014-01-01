#lang racket/base

;; This test tries to stress module expansion, module invocation/visit,
;; and JIT compilation with stack overflows. It turns out to be a useful
;; GC test, too, since stack overflows trigger many minor GCs.

(module stress racket/load

  (module loopy racket/base
    (require (for-syntax racket/base))
    (provide loopy)
    
    (define-syntax (loopy stx)
      (printf "~s\n" (variable-reference->module-base-phase (#%variable-reference)))  
      (if (= 100 (variable-reference->module-base-phase (#%variable-reference)))
          #'(void)
          #'(begin
              (require (for-syntax 'loopy))
              (begin-for-syntax
               (loopy))))))

  (define results (make-vector 30))

  (void
   (let loop ([i 0])
     (vector-set-performance-stats! results)
     (if (zero? (vector-ref results 5))
         (let ([v (loop (add1 i))])
           (if (zero? v)
               (begin
                 (printf "at ~s\n" i) ; around 129000 for 32-bit w/JIT; around 16750 for 32-bit w/o JIT
                 (eval '(module overflow racket
                          (require 'loopy)
                          (loopy)))
                 -1)
               (sub1 v)))
         (if (eval-jit-enabled)
             503
           50)))))

(require 'stress)

(module+ test
  (module config info
    (define random? #t)))
