#lang racket/base
(require racket/fixnum
         "../common/class.rkt"
         "../common/internal-error.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "output-port.rkt"
         "count.rkt"
         "check.rkt")

(provide write-some-bytes)

(define (write-some-bytes who out bstr start end
                          #:copy-bstr? [copy-bstr? #t]
                          #:buffer-ok? [buffer-ok? #f]
                          #:zero-ok? [zero-ok? #f]
                          #:enable-break? [enable-break? #f])
  (let try-again ([out out] [extra-count-outs null])
    (start-atomic)
    (cond
      [(fx= start end)
       (check-not-closed who out)
       (end-atomic)
       0]
      [else
       (define buffer (core-port-buffer out))
       (define buf-pos (direct-pos buffer))
       (define buf-end (direct-end buffer))
       (cond
         [(buf-pos . fx< . buf-end)
          ;; Copy bytes directly to buffer
          (define v (fxmin (fx- buf-end buf-pos) (fx- end start)))
          (bytes-copy! (direct-bstr buffer) buf-pos bstr start (fx+ start v))
          (set-direct-pos! buffer (fx+ buf-pos v))
          (when (or (pair? extra-count-outs) (core-port-count out))
            (port-count-all! out extra-count-outs v bstr start))
          (end-atomic)
          v]
         [else
          (check-not-closed who out)
          (define write-out (method core-output-port out write-out))
          (cond
            [(procedure? write-out)
             (define v (write-out out bstr start end (not buffer-ok?) enable-break? copy-bstr?))
             (let result-loop ([v v])
               (cond
                 [(not v)
                  (end-atomic)
                  (if zero-ok?
                      0
                      (try-again out extra-count-outs))]
                 [(exact-positive-integer? v)
                  (port-count-all! out extra-count-outs v bstr start)
                  (end-atomic)
                  v]
                 [(evt? v)
                  (end-atomic)
                  (cond
                    [zero-ok? 0]
                    [else
                     (define new-v (if enable-break?
                                       (sync/enable-break v)
                                       (sync v)))
                     (start-atomic)
                     (result-loop new-v)])]
                 [else
                  (end-atomic)
                  (internal-error (format "write-some-bytes: weird result ~s for ~s ~s ~s at ~s" v bstr start end out))]))]
            [else
             (end-atomic)
             (try-again (->core-output-port write-out) (cons out extra-count-outs))])])])))
