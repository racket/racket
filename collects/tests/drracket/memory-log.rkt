#lang racket/base
(require "private/drracket-test-util.rkt"
         racket/gui/base
         racket/class
         framework/test
         framework/preferences
         drracket/private/syncheck/local-member-names) ;; for the syncheck:arrows-visible? method

(printf "The printouts below are designed to trick drdr into graphing them;\n")
(printf "they aren't times, but memory usage. The first is starting up DrRacket,\n")
(printf "the second is after the documentation index has been loaded (via check\n")
(printf "syntax) and the third is after online check syntax has completed once (so\n")
(printf "a place was created and the docs loaded there.\n")

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
    
(void (putenv "PLTDRPLACEPRINT" "yes"))

(define (wait-and-print)
  (let ([s (make-semaphore 0)])
    ;; let two rounds of pending events be handled. 
    (queue-callback (λ () (queue-callback  (λ () (semaphore-post s)) #f)) #f)
    (yield s)
    
    ;; print out memory use in a fake form to be tracked by drdr
    (let ([n (mem-cnt)])
      (printf "cpu time: ~a real time: ~a gc time: ~a\n"
              n n n))))

(fire-up-drracket-and-run-tests 
 (λ ()
   (let ([drs-frame (wait-for-drracket-frame)])
     
     ;; initial startup memory use
     (wait-and-print)
     
     ;; figure out the memory use after running check syntax once (and so the docs 
     ;; have been loaded)
     (queue-callback
      (λ () (send (send drs-frame get-definitions-text) insert "#lang racket/base\n+")))
     (set-module-language!)
     (test:run-one (lambda () (send (send drs-frame syncheck:get-button) command)))
     (wait-for-computation drs-frame)
     (wait-and-print)
     
     ;; figure out the memory use after letting online check syntax run once
     ;; (so a place has been created and the docs loaded again (in the other place
     ;; this time))
     
     ; clear out the check synax results from before
     (queue-callback/res (λ () (send (send drs-frame get-definitions-text) insert "\n")))
     (poll-until
      (λ ()
        (not (send (send drs-frame get-definitions-text) syncheck:arrows-visible?))))
     
     ; enable online check syntax and wait for the results to appear
     (queue-callback/res (λ () (preferences:set 'drracket:online-compilation-default-on #t)))
     (poll-until
      (λ ()
        (send (send drs-frame get-definitions-text) syncheck:arrows-visible?)))
     (wait-and-print))))
