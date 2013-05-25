#lang racket/base

(require rackunit rackunit/text-ui 
         data/queue
         racket/stream)

(run-tests
 (test-suite "queue.rkt"
             (test-suite "queue-empty?"
                         (test-case "make-queue"
                                    (check-true (queue-empty? (make-queue))))
                         (test-case "enqueue! once"
                                    (let* ([q (make-queue)])
                                      (enqueue! q 1)
                                      (check-false (queue-empty? q))))
                         (test-case "enqueue! once / dequeue! once"
                                    (let* ([q (make-queue)])
                                      (enqueue! q 1)
                                      (dequeue! q)
                                      (check-true (queue-empty? q))))
                         (test-case "enqueue! twice"
                                    (let* ([q (make-queue)])
                                      (enqueue! q 1)
                                      (enqueue! q 2)
                                      (check-false (queue-empty? q))))
                         (test-case "enqueue! twice / dequeue! once"
                                    (let* ([q (make-queue)])
                                      (enqueue! q 1)
                                      (enqueue! q 2)
                                      (dequeue! q)
                                      (check-false (queue-empty? q))))
                         (test-case "enqueue! twice / dequeue! twice"
                                    (let* ([q (make-queue)])
                                      (enqueue! q 1)
                                      (enqueue! q 2)
                                      (dequeue! q)
                                      (dequeue! q)
                                      (check-true (queue-empty? q)))))
             (test-suite "length"
                         (test-case "length empty"
                                    (let* ([queue (make-queue)])
                                      (check-equal? (queue-length queue) 0)))
                         (test-case "length enqueue once"
                                    (let* ([queue (make-queue)])
                                      (enqueue! queue 5)
                                      (check-equal? (queue-length queue) 1)))
                         (test-case "length enqueue thrice dequeue once"
                                    (let* ([queue (make-queue)])
                                      (enqueue! queue 5)
                                      (enqueue! queue 9)
                                      (enqueue! queue 12)
                                      (dequeue! queue)
                                      (check-equal? (queue-length queue) 2))))
             (test-suite "dequeue!"
                         (test-case "make-queue"
                                    (check-exn exn:fail? (lambda () (dequeue! (make-queue)))))
                         (test-case "enqueue! once"
                                    (let* ([q (make-queue)])
                                      (enqueue! q 1)
                                      (check-equal? (dequeue! q) 1)
                                      (check-exn exn:fail? (lambda () (dequeue! q)))))
                         (test-case "enqueue! twice"
                                    (let* ([q (make-queue)])
                                      (enqueue! q 1)
                                      (enqueue! q 2)
                                      (check-equal? (dequeue! q) 1)
                                      (check-equal? (dequeue! q) 2)
                                      (check-exn exn:fail? (lambda () (dequeue! q)))))
                         (test-case "don't leak last element"
                                    (let* ([thing (box 'box-that-queue-should-not-hold-onto)]
                                           [wb (make-weak-box thing)]
                                           [q (make-queue)])
                                      (enqueue! q thing)
                                      (set! thing #f)
                                      (dequeue! q)
                                      (collect-garbage)
                                      (check-false (weak-box-value wb))
                                      ;; need a reference to 'q' after looking in the
                                      ;; box or else the whole queue gets collected
                                      (check-true (queue? q)))))
             (test-suite "queue misc"
                         (test-case "queue as a sequence"
                                    (let ([queue (make-queue)])
                                      (enqueue! queue 1)
                                      (enqueue! queue 2)
                                      (enqueue! queue 3)
                                      (check-equal? '(1 2 3) (for/list ([item (in-queue queue)]) item)))
                                    (check-equal? '() (for/list ([item (in-queue (make-queue))]) item)))
                         (test-case "queue to empty list"
                                    (let ([queue (make-queue)])
                                      (check-equal? (queue->list queue) '())))
                         (test-case "queue length"
                                    (let ([queue (make-queue)])
                                      (enqueue! queue 1)
                                      (enqueue! queue 2)
                                      (enqueue! queue 3)
                                      (check-equal? (queue->list queue) '(1 2 3)))))))

;; try 1000 random tests
(for ([x (in-range 1000)])
  (define lst '())
  (define deq (make-queue))
  (define ops '())
  
  ;; try 30 random ops per test
  (for ([op-number (in-range 30)])
    
    (case (random 5)
      [(0) 
       (define ele (random 100000))
       (set! lst (cons ele lst))
       (enqueue-front! deq ele)
       (set! ops (cons `(add-front ,ele) ops))]
      [(1)
       (define ele (random 100000))
       (set! lst (reverse (cons ele (reverse lst))))
       (enqueue! deq ele)
       (set! ops (cons `(add-back ,ele) ops))]
      [(2)
       (unless (null? lst)
         (dequeue! deq)
         (set! lst (cdr lst))
         (set! ops (cons `(pop) ops)))]
      [(3)
       (set! lst (filter even? lst))
       (queue-filter! deq even?)
       (set! ops (cons `(filter even?) ops))]
      [(4)
       (set! lst (filter odd? lst))
       (queue-filter! deq odd?)
       (set! ops (cons `(filter odd?) ops))])
    
    ;; check to make sure the list
    ;; and queue version match up
    ;; after each of the ops
    (define qlst (queue->list deq))
    (unless (equal? lst qlst)
      (error 'queue.rkt
             "test failure, elements different: ~s\n  => ~s (queue)\n  => ~s (list)"
             ops 
             qlst lst))
    (unless (= (length lst) (queue-length deq))
      (error 'queue.rkt
             "test failure, lengths different: ~s\n  => ~s (queue)\n  => ~s (list)"
             ops 
             (length lst) (queue-length deq)))))
