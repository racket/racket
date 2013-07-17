#lang racket/load
(module a racket
  (require unstable/temp-c/dsl
           unstable/match)
  
  (define memory%
    (class object%
      (super-new)
      (define/public (malloc) 1)
      (define/public (free n) (void))))
  
  (provide/contract
   [memory%
    (with-monitor
        (class/c [malloc (label 'malloc (->m number?))]
                 [free (label 'free (->m number? void))])
      (complement
       (seq (star _)
            (dseq (call 'free _ addr)
                  (seq
                   (star (not (ret 'malloc (== addr))))
                   (call 'free _ (== addr)))))))]))

(module b racket
  (require 'a tests/eli-tester)
  
  (define memory (new memory%))
  
  (define a (send memory malloc))
  (test
   (send memory free a)
   (send memory free a)
   =error> #rx"disallowed call"))

(require 'b)
