#lang racket/base
(require racket/future
         racket/place)

;; Check shutdown of future threads by exiting a place and via
;; `custodian-shutdown-all`.

;; Since this test may hang on failure, run with a `--timeout` argument on `raco test`.

(define (go)
  (place pch (run)))

(define (run)
  (define c (make-custodian))
  (define f #f)

  (define loop? ((random) . < . 0.2))
  
  (parameterize ([current-custodian c])
    (void
     (thread (lambda ()
               (set! f (future (lambda ()
                                 (if loop?
                                     (let loop () (loop))
                                     10))))))))
  
  (sync (system-idle-evt))
  (sleep 0.1)

  (when (zero? (random 2))
    (custodian-shutdown-all c)))
  
(module+ main
  (for ([i 30])
    (printf "ok ~a\n" (current-seconds))
    (flush-output)
    (place-wait (go))))

(module+ test
  (require (submod ".." main)))
