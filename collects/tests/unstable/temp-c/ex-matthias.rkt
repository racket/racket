#lang racket/load
(module a racket
  (require racket/require
           (path-up "temp-c/dsl.rkt")
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
 (require 'a)

 (define memory (new memory%))

 (define a (send memory malloc))
 (send memory free a) (displayln `(freeing ,a))
 (send memory free a) (displayln `(freeing ,a))
 (send memory free a) (displayln `(freeing ,a)))

(require 'b)