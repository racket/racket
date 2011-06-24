#lang racket/load

#| This file shows that we can still track some things without an "interval"
|#

(module mem racket
  (require unstable/temp-c/monitor)
  
  (define free-list empty)
  (define last-addr 0)
  (define (malloc)
    (if (empty? free-list)
        (begin0 last-addr
                (set! last-addr (add1 last-addr)))
        (begin0 (first free-list)
                (set! free-list (rest free-list)))))
  (define (free addr)
    (set! free-list (cons addr free-list)))
  
  (define allocated (make-weak-hasheq))
  (define (mem-monitor evt)
    ; Only allow freeing of allocated things, disallow double frees
    ; and track addrs using malloc returns
    (match evt
      [(monitor:return 'malloc _ _ _ _ _ _ (list addr))
       (hash-set! allocated addr #t)
       #t]
      [(monitor:call 'free _ _  _ _ _ (list addr))
       (hash-has-key? allocated addr)]
      [(monitor:return 'free _ _ _ _ _ (list addr) _)
       (hash-remove! allocated addr)
       #t]
      [_
       #t]))
  
  (provide/contract
   [malloc (monitor/c mem-monitor 'malloc (-> number?))]
   [free (monitor/c mem-monitor 'free (-> number? void))]))

(module mem-test racket
  (require tests/eli-tester
           'mem)
  (test
   (malloc)
   
   (free (malloc))
   
   (free -1) =error> "disallow"
   
   (free (malloc))
   
   (let ([a (malloc)])
     (free a)
     (free a))
   =error>
   "disallow"))

(require 'mem-test)
