#lang racket/load

#| This file is an attempt to show a different style of monitor
   that doesn't record the event trace, but rather records the
   pertinent information.
|#

(module lock racket
  (require unstable/temp-c/monitor)
  (define (use-resource f)
    (define (protect label g)
      (contract (monitor/c monitor label (-> void)) g
                'pos 'neg))
    
    (define locked? #f)
    (define returned? #f)
    (define (monitor evt)
      (match evt
        [(monitor:return 'user _ _ app _ _ _ _)
         (set! returned? #t)]
        [(monitor:return 'lock p  _ _ _ _ _ _)
         (set! locked? #t)]
        [(monitor:return 'unlock p _ _ _ _ _ _)
         (set! locked? #f)]
        [_
         (void)])
      (and
       (match evt
         ; Must not lock or unlock twice
         [(monitor:call 'lock p _ _ _ _ _)
          (not locked?)]
         [(monitor:call 'unlock p _ _ _ _ _)
          locked?]
         ; Must not use resource unless locked
         [(monitor:call 'use p _ _ _ _ _)
          locked?]
         ; Otherwise, okay
         [_
          #t])
       ; Must not use anything after return
       (match evt
         [(monitor:call 'lock p _ _ _ _ _)
          (not returned?)]
         [(monitor:call 'unlock p _ _ _ _ _)
          (not returned?)]
         [(monitor:call 'use p _ _ _ _ _)
          (not returned?)]
         ; Otherwise, okay
         [_
          #t])))
    
    ((contract (monitor/c monitor 'user any/c) f
               'pos 'neg)
     (protect 'lock (λ () (void)))
     (protect 'use (λ () (void)))
     (protect 'unlock (λ () (void)))))
  
  (provide/contract
   [use-resource
    (-> (-> (-> void) (-> void) (-> void)
            any/c)
        any/c)]))

(module tester racket
  (require tests/eli-tester
           'lock)
  (test
   (use-resource
    (λ (lock use unlock)
      (lock) (use) (unlock)
      (lock) (use) (use) (unlock)))
   =>
   (void)
   
   (use-resource
    (λ (lock use unlock)
      (lock) (use) (unlock)
      (use-resource
       (λ (lock1 use1 unlock1)
         ; Note out of order unlocking
         (lock1) (lock)
         (use) (use1)
         (unlock1) (unlock)))
      (lock) (use) (use) (unlock)))
   =>
   (void)
   
   (use-resource
    (λ (lock use unlock)
      (use)))
   =error>
   "disallowed"
   
   (use-resource
    (λ (lock use unlock)
      (lock) (use) (unlock) (unlock)))
   =error>
   "disallowed"
   
   (use-resource
    (λ (lock use unlock)
      (lock) (lock)))
   =error>
   "disallowed"
   
   (use-resource
    (λ (lock use unlock)
      (lock) (unlock) (use)))
   =error>
   "disallowed"
   
   ((use-resource (λ (lock use unlock) lock)))
   =error>
   "disallowed"
   
   ((use-resource (λ (lock use unlock) use)))
   =error>
   "disallowed"
   
   ((use-resource (λ (lock use unlock) unlock)))
   =error>
   "disallowed"
   ))

(require 'tester)
