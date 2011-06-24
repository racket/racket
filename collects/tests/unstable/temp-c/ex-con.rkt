#lang racket
(require racket/require
         (path-up "temp-c/dsl.rkt")
         tests/eli-tester)

(define count 0)
(define (evil? v)
  (define nc (add1 count))
  (sleep (random))
  (set! count nc)
  #t)

(define (test-spec spec)
  (define (f g) g)
  (define f/c (contract spec f 'pos 'neg))
  
  (define x #f)
  (define (body)
    (with-handlers ([exn? (λ (e) (set! x e))])
      (f/c 1)))
  (define t1
    (thread body))
  (define t2
    (thread body))
  
  (thread-wait t1)
  (thread-wait t2)
  (when x
    (raise x)))

(define-syntax-rule (dupe r c K T)
  (begin (define r (with-monitor K T))
         (define c (with-monitor K #:concurrent T))))

(dupe Race Concurrent
      (label 'f (number? . -> . number?))
      (seq (monitor:proj 'f _ _)
           (star (union (call 'f (? evil?))
                        (ret 'f _)))))

(test
 #:failure-prefix "Race"
 (test
  (set! count 0)
  (test-spec Race)
  count => 2)
 #:failure-prefix "Concurrent"
 (test
  (set! count 0)
  (test-spec Concurrent)
  count => 2))