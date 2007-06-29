(module dispatch-stat mzscheme
  (require (lib "contract.ss"))
  (require "dispatch.ss"
           "../private/web-server-structs.ss"
           "../private/connection-manager.ss")
  (provide/contract
   [make-gc-thread (integer? . -> . thread?)]
   [interface-version dispatcher-interface-version?]
   [make (-> dispatcher?)])
  
  (define (bytes->mb b)
    (round (exact->inexact (/ b 1024 1024))))
  
  (define (print-memory-usage)
    (printf "Usage: ~aMB (of ~aMB)~n"
            (bytes->mb (current-memory-use (current-server-custodian)))
            (bytes->mb (current-memory-use))))
  
  (define (make-gc-thread t)
    (thread
     (lambda ()
       (let loop ()
         (sleep t) 
         (printf ".")         
         (collect-garbage)
         (loop)))))
  
  (define interface-version 'v1)
  (define ((make) conn req)
    #;(dump-memory-stats)
    (print-memory-usage)
    (next-dispatcher)))