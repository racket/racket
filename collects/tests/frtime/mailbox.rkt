#lang scheme
(require scheme/package
         tests/eli-tester
         frtime/core/match
         (prefix-in ch: frtime/core/mailbox)
         (prefix-in sema: frtime/core/sema-mailbox))

(define (test-it! new-mailbox mailbox? mailbox-send! mailbox-receive)
  (define mb (new-mailbox))
  (define ch (make-channel))
  (define (error-timeout) (error 'never))
  (define (id-thnk v) (lambda () v))
  (define (want-thnk what) 
    (lambda (v) 
      (if (= what v)
          (lambda () v)
          match-fail)))
  (test
   (mailbox? mb) => #t
   (mailbox-send! mb 25) => (void)  
   ((mailbox-receive mb #f error-timeout id-thnk)) => 25
   ((mailbox-receive mb 10 error-timeout id-thnk)) =error> "never"
   ;(mailbox-send! mb #f) => (void)  
   ;((mailbox-receive mb #f error-timeout id-thnk)) =error> "never"
   (mailbox-send! mb 21) => (void)
   ((mailbox-receive mb 10 error-timeout (want-thnk 25))) =error> "never"
   ((mailbox-receive mb 10 error-timeout (want-thnk 21))) => 21
   (mailbox-send! mb 23) => (void)
   (mailbox-send! mb 24) => (void)
   ((mailbox-receive mb 10 error-timeout (want-thnk 23))) => 23
   ((mailbox-receive mb 10 error-timeout (want-thnk 24))) => 24
   (mailbox-send! mb 24) => (void)
   (mailbox-send! mb 23) => (void)
   ((mailbox-receive mb 10 error-timeout (want-thnk 23))) => 23
   ((mailbox-receive mb 10 error-timeout (want-thnk 24))) => 24  
   ))

(printf "Channel\n")
(test-it! ch:new-mailbox ch:mailbox? ch:mailbox-send! ch:mailbox-receive)
(printf "Semaphore\n")
(test-it! sema:new-mailbox sema:mailbox? sema:mailbox-send! sema:mailbox-receive)
