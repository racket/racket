#lang racket/base
(require "drracket-test-util.rkt"
         racket/gui/base
         racket/class
         framework/test)

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
    
(void (putenv "PLTDRXREFDELAY" "yes"))

(define (wait-and-print)
  (let ([s (make-semaphore 0)])
    ;; let two rounds of pending events be handled. 
    (queue-callback (λ () (queue-callback  (λ () (semaphore-post s)) #f)) #f)
    (yield s)
    
    ;; print out memory use in a fake form to be tracked by drdr
    (let ([n (mem-cnt)])
      (printf "cpu time: ~a real time: ~a gc time: ~a\n"
              n n n))))

(printf "The printouts below are designed to trick drdr into graphing them;\nthey aren't times, but memory usage.\n")
(fire-up-drscheme-and-run-tests 
 (λ ()
   (let ([drs-frame (wait-for-drscheme-frame)])
     
     (wait-and-print)
     
     (send (send drs-frame get-definitions-text) insert "#lang racket/base\n+")
     (set-module-language!)
     (test:run-one (lambda () (send (send drs-frame syncheck:get-button) command)))
     (wait-for-computation drs-frame)
     
     (wait-and-print))))     

