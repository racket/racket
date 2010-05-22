#lang scheme

(require "checks.ss"
         "../debug.ss")

(provide debug-suite)

(define debug-suite
  (test-suite "debug.ss"
    (test-suite "dprintf"
      (test
       (let ()
         (define logger (make-logger))
         (define receiver (make-log-receiver logger 'debug))
         (parameterize ([current-logger logger])
           (dprintf "Danger, ~a!" "Will Robinson"))
         (check-not-false
          (member
           "Danger, Will Robinson!"
           (let loop ()
             (match (sync/timeout 0 receiver)
               [(vector 'debug (? string? message) _)
                (cons message (loop))]
               [_ null])))))))))
