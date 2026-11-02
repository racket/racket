#lang racket/base

(require racket/place)

;; Regression test for a bug that could effectivly discard the ready
;; state of a pipe port as a place is started. The original test used
;; 100 places, but that's a lot of file descriptors, so we limit to 20
;; here and hope that running the test regularly makes up for shorter
;; runs.

(define-values (feeder pipe child-in _)
  (subprocess #f #f #f
              (find-executable-path (find-system-path 'exec-file))
              "-e" "(let loop () (write-byte 120) (flush-output) (sleep 0.001) (loop))"))
(close-output-port child-in)

(define reader
  (thread (lambda ()
            (let loop ()
              (unless (eof-object? (read-byte pipe))
                (loop))))))

;; Keep the list so idle places are not garbage-collected.
(define places
  (for/list ([_ (in-range 20)])
    (place _ (sync never-evt))))

;; Stop pipe input, then run CPU-only work.
(thread-suspend reader)
(subprocess-kill feeder #t)
(void (thread (lambda ()
                (sleep 3)
                (displayln "stalled")
                (exit 1))))
(displayln (for/sum ([n (in-range 1000000)]) n))
