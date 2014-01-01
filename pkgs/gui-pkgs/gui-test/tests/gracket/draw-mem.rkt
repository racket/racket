#lang racket

;; Check for a leak via multiple `racket/draw' instantiations.

(define my-ns-channel (make-parameter #f))

(define N 20)

(define-values (incs m ns)
  (for/fold ([incs 0] [max-mem 0] [accum #f]) ([i N])
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns]
                   [my-ns-channel ns])
      (dynamic-require 'racket/draw #f))
    (collect-garbage)
    (sync (system-idle-evt))
    (collect-garbage)
    (let ([m (current-memory-use)])
      (printf "~s\n" m)
      (if (m . > . (* 1.1 max-mem))
          (values (add1 incs) m accum)
          (values incs max-mem accum)))))

(unless (incs . < . (/ N 3))
  (error "multiple `racket/draw' instantiations seem to accumulate memory"))

(module+ test
  (module config info
    (define random? #t)))
