#lang racket

(require rackunit rackunit/text-ui unstable/queue "helpers.rkt")

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
   (test-suite "dequeue!"
     (test-case "make-queue"
       (check-exn exn:fail:contract? (lambda () (dequeue! (make-queue)))))
     (test-case "enqueue! once"
       (let* ([q (make-queue)])
         (enqueue! q 1)
         (check-equal? (dequeue! q) 1)
         (check-exn exn:fail:contract?
           (lambda () (dequeue! q)))))
     (test-case "enqueue! twice"
       (let* ([q (make-queue)])
         (enqueue! q 1)
         (enqueue! q 2)
         (check-equal? (dequeue! q) 1)
         (check-equal? (dequeue! q) 2)
         (check-exn exn:fail:contract?
           (lambda () (dequeue! q))))))))
