#lang racket

;; Check for a leak via multiple `racket/draw' instantiations.

(define my-ns-channel (make-parameter #f))

(define-values (incs m ns)
  (for/fold ([incs 0] [max-mem 0] [ns #f]) ([i 10])
    (define ns (make-base-namespace))
    (parameterize ([current-namespace ns]
                   [my-ns-channel ns])
      (dynamic-require 'racket/draw #f))
    (collect-garbage)
    (sync (system-idle-evt))
    (collect-garbage)
    (let ([m (current-memory-use)])
      (printf "~s\n" m)
      (if (m . > . max-mem)
          (values (add1 incs) m 'ns)
          (values incs max-mem 'ns)))))

(unless (incs . < . 5)
  (error "multiple `racket/draw' instantiations seem to accumulate memory"))

