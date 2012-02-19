#lang racket/base

(require tests/eli-tester)

;; tests for space safety, especially for `or' and `and'

(define (space-tests)
  (define (one-test first-class?)
    (collect-garbage)
    (define mem (current-memory-use))
    (define t
      (thread 
       (lambda () 
         (parameterize ([current-namespace (make-base-namespace)])
           (eval
            `(module loop lazy
               (let ()
                 (define (list-from n)
                   (if (= n 500000)
                       empty
                       (cons n (list-from (add1 n)))))
                 ,@(if first-class?
                       `((define my-or or)
                         (define my-and and))
                       '())
                 (define (has-negative? l)
                   (,(if first-class? 'my-and 'and)
                    (pair? l)
                    (,(if first-class? 'my-or 'or)
                     (negative? (car l))
                     (has-negative? (rest l)))))
                 (! (has-negative? (list-from 0))))))
           (eval `(require 'loop))))))
    (thread (lambda () (let loop ()
                         (sleep 0.2)
                         (unless ((current-memory-use) . < . (* 10 mem))
                           (eprintf "too much memory!")
                           (kill-thread t))
                         (when (thread-running? t)
                           (loop)))))
    (sync t)
    (void))
  (one-test #f)
  (one-test #t))
     
(provide space-tests)
