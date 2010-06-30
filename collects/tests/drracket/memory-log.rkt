#lang racket/base
(require "drracket-test-util.rkt"
         racket/gui/base)

;; mem-cnt returns the amount of memory used, iterating (collect-garbage)
;; until the delta is less than 10k or we've done it 20 times.
(define (mem-cnt)
  (let loop ([cmu (current-memory-use)]
             [n 20])
    (collect-garbage)
    (let ([new-cmu (current-memory-use)])
      (cond
        [(or (< n 0)
             (< (abs (- cmu new-cmu)) 
                (* 10 1024)))
         new-cmu]
        [else
         (loop new-cmu (- n 1))]))))
    
(fire-up-drscheme-and-run-tests 
 (λ ()
   (let ([drscheme-frame (wait-for-drscheme-frame)]
         [s (make-semaphore 0)])
     (queue-callback (λ () (queue-callback  (λ () (semaphore-post s)) #f)) #f)
     (yield s) ;; let two rounds of pending events be handled. 
     (let ([n (mem-cnt)])
       (printf "cpu time: ~a real time: ~a gc time: ~a\n"
               n n n)))))
