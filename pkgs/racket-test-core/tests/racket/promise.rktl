(load-relative "loadtest.rktl")

(require racket/promise)

(Section 'promise)

(test '(0 1)
  (for/list/concurrent ([i (in-range 3)])
    #:break (= i 1)
    i))

(test '((0 0) (0 1) (1 0) (1 1))
  (for*/list/concurrent ([i (in-range 2)]
                         [j (in-range 2)])
    (list i j)))

;; Test that the bodies run in different threads, but the results are
;; ordered.
(let ()
  (define-syntax (test-concurrent-result stx)
    (syntax-case stx ()
      [(_ for/list/concurrent-id)
       #'(let* ([group (make-thread-group)]
                [thds (make-hasheq)]
                [results (for/list/concurrent-id #:group group ([i (in-range 3)])
                           (test group (current-thread-group))
                           (hash-set! thds (current-thread) #t)
                           i)])
           (test '(0 1 2) results)
           (test 3 (hash-count thds)))]))

  (test-concurrent-result for/list/concurrent)
  (test-concurrent-result for*/list/concurrent))
