#lang racket/load

#| This file shows that we can still track some things without an "interval"
|#

(module mem racket
  (require unstable/temp-c/monitor)
  (define heap%
    (class object%
      (define free-list empty)
      (define last-addr 0)
      (define/public (malloc)
        (if (empty? free-list)
            (begin0 last-addr
                    (set! last-addr (add1 last-addr)))
            (begin0 (first free-list)
                    (set! free-list (rest free-list)))))
      (define/public (free addr)
        (set! free-list (cons addr free-list)))
      
      (super-new)))
  
  (define allocated (make-weak-hasheq))
  (define (the-monitor evt)
    ; Only allow freeing of allocated things, disallow double frees
    ; and track addrs using malloc returns
    (match evt
      [(monitor:return 'malloc _ _ _ _ _ _ (list addr))
       (hash-set! allocated addr #t)
       #t]
      [(monitor:call 'free _ _  _ _ _ (list _ addr))
       (hash-has-key? allocated addr)]
      [(monitor:return 'free _ _ _ _ _ (list _ addr) _)
       (hash-remove! allocated addr)
       #t]
      [_
       #t]))
  
  (provide/contract
   [heap% 
    (class/c 
     [malloc (monitor/c the-monitor 'malloc (->m number?))]
     [free (monitor/c the-monitor 'free (->m number? void))])]))

(module mem-test racket
  (require tests/eli-tester
           'mem)
  (define h (new heap%))
  (test
   (send h malloc)
   
   (send h free (send h malloc))
   
   (send h free -1) =error> "disallow"
   
   (send h free (send h malloc))
   
   (let ([a (send h malloc)])
     (send h free a)
     (send h free a))
   =error>
   "disallow"))

(require 'mem-test)
