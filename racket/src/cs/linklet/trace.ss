;; Ideally this would just use the standard Racket logging facility,
;; but that is not accessible to all of the layers we're working with.
;;
;; Unfortunately we are faced with a somewhat unusual requirement to
;; produce a consistently ordered event trace that weaves together
;; messages from several different layers:
;;
;;   - `cs/linklet.sls`
;;   - `ChezScheme/s/**`
;;
;; To achieve this, we log to the error port, which testing has shown
;; fares better than the output port. Logging to the error port
;; preserves the expected ordering of events from different layers.

(define trace-times-on? (getenv "PLT_LINKLET_TRACE_TIMES"))

(define trace-printf-core
  (lambda (fmt . args)
    (apply #%fprintf (#%current-error-port) fmt args)))

(define trace-printf
  (lambda (fmt . args)
    ;; Currently undocumented, intended as an optional aid when
    ;; debugging trace event order.
    (when trace-times-on?
      (trace-printf-core "[~a] " (current-inexact-monotonic-milliseconds)))
    (apply trace-printf-core fmt args)))
