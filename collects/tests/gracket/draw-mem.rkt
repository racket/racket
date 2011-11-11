#lang racket

;; Check for a leak via multiple `racket/draw' instantiations.

(define-values (incs m)
  (for/fold ([incs 0] [prev-mem 0]) ([i 10])
    (parameterize ([current-namespace (make-base-namespace)])
      (dynamic-require 'racket/draw #f))
    (collect-garbage)
    (sync (system-idle-evt))
    (collect-garbage)
    (let ([m (current-memory-use)])
      (if (m . > . (+ prev-mem (* 100 1024)))
          (values (add1 incs) m)
          (values incs m)))))

(unless (incs . < . 5)
  (error "multiple `racket/draw' instantiations seem to accumulate memory"))

