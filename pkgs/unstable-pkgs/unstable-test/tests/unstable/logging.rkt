#lang racket/base

(require rackunit rackunit/text-ui unstable/logging)

(run-tests
 (test-suite "logging.rkt"
   (test-case "start/stop-recording"
     (let ([l (start-recording 'warning)])
       (log-warning "1")
       (log-warning "2")
       (log-warning "3")
       (log-info "4")
       (stop-recording l) ; stopping should be idempotent
       (let ([out (stop-recording l)])
         (check-equal? (map (lambda (l) (vector-ref l 1)) out)
                       '("1" "2" "3"))
         (check-true (andmap (lambda (l) (eq? (vector-ref l 0) 'warning))
                             out)))))))
