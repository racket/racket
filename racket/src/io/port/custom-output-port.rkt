#lang racket/base
(require "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "output-port.rkt"
         "custom-port.rkt"
         "pipe.rkt"
         "count.rkt")

(provide make-output-port)

(define/who (make-output-port name
                              evt
                              user-write-out
                              user-close
                              [user-write-out-special #f]
                              [user-get-write-evt #f]
                              [user-get-write-special-evt #f]
                              [user-get-location #f]
                              [user-count-lines! void]
                              [user-init-position 1]
                              [user-buffer-mode #f])
  (check who evt? evt)
  (check who (lambda (p) (or (output-port? p)
                             (and (procedure? p)
                                  (procedure-arity-includes? p 5))))
         #:contract "(or/c output-port? (procedure-arity-includes/c 5))"
         user-write-out)
  (check who (procedure-arity-includes/c 0) user-close)
  (check who (lambda (p) (or (not p)
                             (output-port? p)
                             (and (procedure? p)
                                  (procedure-arity-includes? p 3))))
         #:contract "(or/c #f output-port? (procedure-arity-includes/c 3))"
         user-write-out-special)
  (check who #:or-false (procedure-arity-includes/c 3) user-get-write-evt)
  (check who #:or-false (procedure-arity-includes/c 1) user-get-write-special-evt)
  (check who #:or-false (procedure-arity-includes/c 0) user-get-location)
  (check who (procedure-arity-includes/c 0) user-count-lines!)
  (check-init-position who user-init-position)
  (check-buffer-mode who user-buffer-mode)

  (when (and (not user-write-out-special) user-get-write-special-evt)
    (raise-arguments-error who "write-special argument is #f, but get-write-special-evt argument is not"
                           "get-write-special-evt argument" user-get-write-special-evt))

  (when (and (not user-get-write-evt) user-get-write-special-evt)
    (raise-arguments-error who "get-write-evt argument is #f, but get-write-special-evt argument is not"
                           "get-write-special-evt argument" user-get-write-special-evt))

  (when (and (not user-get-write-special-evt) user-get-write-evt user-write-out-special)
    (raise-arguments-error who
                           "get-write-special-evt argument is #f, but get-write-evt argument is not, and write-special argument is not"
                           "get-write-evt argument" user-get-write-evt
                           "get-write-special-evt argument" user-get-write-special-evt))

  (define output-pipe #f)
  
  ;; in atomic mode
  (define (check-write-result who r start end non-block/buffer? #:as-evt? [as-evt? #f])
    (cond
      [(exact-nonnegative-integer? r)
       (if (eqv? r 0)
           (unless (= start end)
             (end-atomic)
             (raise-arguments-error who (string-append
                                         "bad result for non-flush write"
                                         (if as-evt? " event" ""))
                                    "result" r))
           (unless (r . <= . (- end start))
             (end-atomic)
             (raise-arguments-error who "result integer is larger than the supplied byte string"
                                    "result" r
                                    "byte string length" (- end start))))]
      [(not r) r]
      [(pipe-output-port? r)
       (when (= start end)
         (end-atomic)
         (raise-arguments-error who "bad result for a flushing write"
                                "result" r))
       (when non-block/buffer?
         (end-atomic)
         (raise-arguments-error who "bad result for a non-blocking write"
                                "result" r))
       (set! output-pipe r)]
      [(evt? r)
       (void)]
      [else
       (end-atomic)
       (raise-result-error who "(or/c exact-nonnegative-integer? #f evt?)" r)]))
  

  ;; possibly in atomic mode
  (define (wrap-check-write-evt-result who evt start end non-block/buffer?)
    (wrap-evt evt (lambda (r)
                    (start-atomic)
                    (check-write-result who r start end non-block/buffer? #:as-evt? #t)
                    (end-atomic)
                    (cond
                      [(pipe-output-port? r) 0]
                      [(evt? r)
                       (wrap-check-write-evt-result who r start end non-block/buffer?)]
                      [else r]))))

  ;; in atomic mode
  (define (write-out self bstr start end non-block/buffer? enable-break? copy?)
    (cond
      [output-pipe
       (cond
         [(or non-block/buffer?
              (= start end)
              (not (sync/timeout 0 output-pipe)))
          (set! output-pipe #f)
          (write-out self bstr start end non-block/buffer? enable-break? copy?)]
         [else
          (send core-output-port output-pipe write-out bstr start end non-block/buffer? enable-break? copy?)])]
      [else
       (define r
         ;; Always tell user port to re-enable breaks if it blocks, since
         ;; we always disable breaks:
         (let ([enable-break? (and (not non-block/buffer?) (break-enabled))])
           (parameterize-break #f
             (non-atomically
              (if copy?
                  (user-write-out (subbytes bstr start end) 0 (- end start) non-block/buffer? enable-break?)
                  (user-write-out bstr start end non-block/buffer? enable-break?))))))
       (check-write-result '|user port write| r start end non-block/buffer?)
       (cond
         [(pipe-output-port? r)
          (write-out self bstr start end non-block/buffer? enable-break? copy?)]
         [(evt? r)
          (wrap-check-write-evt-result '|user port write| r start end non-block/buffer?)]
         [else r])]))

  (define (get-write-evt self bstr start end)
    (end-atomic)
    (define r (user-get-write-evt bstr start end))
    (unless (evt? r)
      (raise-result-error '|user port get-write-evt| "evt?" r))
    (start-atomic)
    (wrap-check-write-evt-result '|user port write-evt| r start end #t))

  (define (write-out-special self v non-block/buffer? enable-break?)
    (let ([enable-break? (and (not non-block/buffer?) (break-enabled))])
      (parameterize-break #f
        (non-atomically
         (user-write-out-special v non-block/buffer? enable-break?)))))

  (define get-location
    (and user-get-location
         (make-get-location user-get-location)))

  (define count-lines!
    (and user-count-lines!
         (lambda (self) (end-atomic) (user-count-lines!) (start-atomic))))

  (define-values (init-offset file-position)
    (make-init-offset+file-position user-init-position))

  (define buffer-mode
    (and user-buffer-mode
         (make-buffer-mode user-buffer-mode #:output? #t)))

  ;; in atomic mode
  (define (close self)
    (end-atomic)
    (user-close)
    (start-atomic))

  (finish-port/count
   (new core-output-port
        #:field
        [name name]
        [evt evt]
        [offset init-offset]
        #:override
        [write-out (if (output-port? user-write-out)
                       user-write-out
                       write-out)]
        [close close]
        [write-out-special
         (if (output-port? user-write-out-special)
             user-write-out-special
             (and user-write-out-special write-out-special))]
        [get-write-evt (and user-get-write-evt get-write-evt)]
        [get-write-special-evt (and user-get-write-special-evt
                                    (lambda (self v)
                                      (user-get-write-special-evt v)))]
        [get-location get-location]
        [count-lines! count-lines!]
        [file-position file-position]
        [buffer-mode buffer-mode])))
