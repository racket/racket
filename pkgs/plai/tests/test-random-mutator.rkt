#lang racket/base
(require rackunit
         plai/random-mutator
         racket/runtime-path
         racket/file
         racket/pretty
         ;; test find-heap-values and save-random-mutator via the contract'd
         ;; interface, just in case they break their contracts
         (except-in plai/private/random-mutator find-heap-values save-random-mutator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  random mutator generation tests
;;

;; test-code : exp -> boolean
;; returns true if evaluating the example code (as a mutator)
;; returns one result at the top-level, namely the symbol 'passed.
(define (test-code exps)
  (let ([tmpfile (make-temporary-file "plai-random-mutator-test-~a")])
    (call-with-output-file tmpfile
      (λ (port)
        (fprintf port "#lang plai/mutator\n")
        (fprintf port "~s\n" `(allocator-setup plai/tests/gc/good-collectors/good-collector 100))
        (for-each (λ (exp) (pretty-write exp port)) exps))
      #:exists 'truncate)

    (let ([sp (open-output-string)])
      (parameterize ([current-output-port sp])
        (dynamic-require tmpfile #f))
      (delete-file tmpfile)
      (and (regexp-match #rx"Value at location [0-9]+:\n'passed\n"
                         (get-output-string sp))
           #t))))


(define (make-simple-obj-graph/code heap-value)
  (obj-graph->code (make-obj-graph
                    (let ([ht (make-hash)])
                      (hash-set! ht 0 (make-terminal heap-value))
                      ht)
                    '()
                    heap-value
                    0)
                   1
                   100))

(check-true (test-code (list ''passed)))
(check-true (test-code (make-simple-obj-graph/code 'z)))
(check-true (test-code (make-simple-obj-graph/code 111)))
(check-true (test-code (make-simple-obj-graph/code '())))
(check-true (test-code (make-simple-obj-graph/code #t)))
(check-true (test-code (make-simple-obj-graph/code #f)))
(check-true (test-code
             (obj-graph->code (make-obj-graph
                               (let ([ht (make-hash)])
                                 (hash-set! ht 2 (make-pair 2 1))
                                 (hash-set! ht 1 (make-pair 3 4))
                                 (hash-set! ht 0 (make-terminal 'z))
                                 (hash-set! ht 4 (make-pair 0 0))
                                 (hash-set! ht 3 (make-proc (list 0 1 2 0 1)))
                                 ht)
                               '(first 2 rest first 4 first 3)
                               'z
                               1)
                              1
                              100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  find-heap-values tests
;;

(check-equal?
 (find-heap-values
  (open-input-string
   "#lang plai/mutator\n'x"))
 (list 'x))

(check-equal?
 (find-heap-values
  (open-input-string
   "#lang plai/collector\ntrue"))
 (list #t))
(check-equal?
 (find-heap-values
  (open-input-string
   "#lang plai/collector\n1"))
 (list 1))

(check-equal?
 (find-heap-values
  (open-input-string
   "#lang plai/collector\n'(x y 1)"))
 (list 1 'x 'y))

(check-equal?
 (find-heap-values
  (open-input-string
   "#lang plai/collector\n(error 'x \"hm\")(test 'y 'z) (test/exn 'w 'q)"))
 (list))

(check-equal?
 (find-heap-values
  (open-input-string
   "#lang scheme/base\n(error 'x \"hm\")(test 'y 'z) (test/exn 'w 'q)"))
 (list 'q 'w 'x 'y 'z))

(check-equal?
 (find-heap-values
  (open-input-string
   "((error 'x \"hm\")(test 'y 'z) (test/exn 'w 'q))"))
 (list 'q 'w 'x 'y 'z))

(check-equal?
 (find-heap-values
  (open-input-string
   "(true false null)"))
 (list #f #t'()))

(check-equal?
 (find-heap-values
  (open-input-string
   "empty"))
 (list '()))

(check-equal?
 (find-heap-values
  (open-input-string
   "`x"))
 (list 'x))

(check-equal?
 (find-heap-values
  (open-input-string
   "`(())"))
 (list '()))
