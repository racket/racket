#lang scheme
(require plai/test-harness)

(provide count-errors)
(define (count-errors [results plai-all-test-results])
  (foldl (λ (r n) 
           (if (or (symbol=? (first r) 'bad) (symbol=? (first r) 'exception))
               (+ n 1) n))
         0 results))
