#lang racket/base

(require rackunit rackunit/text-ui data/queue)

(run-tests
 (test-suite "queue.ss"
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
   (test-suite "count"
     (test-case "count empty"
       (let* ([queue (make-queue)])
         (check-equal? (queue-count queue) 0)))
     (test-case "count enqueue once"
       (let* ([queue (make-queue)])
         (enqueue! queue 5)
         (check-equal? (queue-count queue) 1)))
     (test-case "count enqueue thrice dequeue once"
       (let* ([queue (make-queue)])
         (enqueue! queue 5)
         (enqueue! queue 9)
         (enqueue! queue 12)
         (dequeue! queue)
         (check-equal? (queue-count queue) 2))))
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
         (check-exn exn:fail? (lambda () (dequeue! q))))))
   (test-suite "queue misc"
     (test-case "queue to empty list"
       (let ([queue (make-queue)])
         (check-equal? (queue->list queue) '())))
     (test-case "queue length"
       (let ([queue (make-queue)])
         (enqueue! queue 1)
         (enqueue! queue 2)
         (enqueue! queue 3)
         (check-equal? (queue->list queue) '(1 2 3)))))))
