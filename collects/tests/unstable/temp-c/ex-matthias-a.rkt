#lang racket
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
                 (call 'free _ (== addr)))))))])
