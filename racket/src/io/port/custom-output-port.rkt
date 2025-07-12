#lang racket/base
(require (only-in racket/unsafe/ops unsafe-bytes->immutable-bytes!)
         "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "output-port.rkt"
         "custom-port.rkt"
         "lock.rkt"
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
  
  ;; with lock held, which implies in atomic mode
  (define (check-write-result who r self start end non-block/buffer? #:as-evt? [as-evt? #f])
    (cond
      [(exact-nonnegative-integer? r)
       (if (eqv? r 0)
           (unless (= start end)
             (port-unlock self)
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
  

  ;; possibly with lock held, which implies in atomic mode
  (define (wrap-check-write-evt-result who evt self start end non-block/buffer?)
    (wrap-evt evt (lambda (r)
                    (port-lock self)
                    (check-write-result who r self start end non-block/buffer? #:as-evt? #t)
                    (port-unlock self)
                    (cond
                      [(pipe-output-port? r) 0]
                      [(evt? r)
                       (wrap-check-write-evt-result who r self start end non-block/buffer?)]
                      [else r]))))

  ;; with lock held, which implies in atomic mode
  (define (write-out self bstr start end non-block/buffer? enable-break? copy? no-escape?)
    (cond
      [output-pipe
       (cond
         [(or non-block/buffer?
              (= start end)
              (not (sync/timeout 0 output-pipe)))
          (set! output-pipe #f)
          (write-out self bstr start end non-block/buffer? enable-break? copy? no-escape?)]
         [else
          (send core-output-port output-pipe write-out bstr start end non-block/buffer? enable-break? copy? no-escape?)])]
      [else
       (define-values (imm-bstr imm-start imm-end)
         ;; If `copy?` is false, we're allowed to do anything with the string,
         ;; so it's ok to destroy it and get an immutable string. If `copy?`
         ;; is true, then we allocate a fresh string, anyway:
         (let ([immutable! unsafe-bytes->immutable-bytes!])
           (if (and copy? (not (immutable? bstr)))
               (values (immutable! (subbytes bstr start end)) 0 (- end start))
               (values (immutable! bstr) start end))))
       (define r
         ;; Always tell user port to re-enable breaks if it blocks, since
         ;; we always disable breaks:
         (let ([enable-break? (and (not non-block/buffer?) (break-enabled))])
           (parameterize-break #f
             (with-no-lock self
               (user-write-out imm-bstr imm-start imm-end non-block/buffer? enable-break?)))))
       (check-write-result '|user port write| r self imm-start imm-end non-block/buffer?)
       (cond
         [(pipe-output-port? r)
          (write-out self imm-bstr imm-start imm-end non-block/buffer? enable-break? copy? no-escape?)]
         [(evt? r)
          (wrap-check-write-evt-result '|user port write| r self imm-start imm-end non-block/buffer?)]
         [else r])]))

  ;; with lock held, which implies in atomic mode
  (define (get-write-evt self bstr start end)
    (define-values (imm-bstr imm-start imm-end)
      (if (immutable? bstr)
          (values bstr start end)
          (values (unsafe-bytes->immutable-bytes! (subbytes bstr start end)) 0 (- end start))))
    (port-unlock self)
    (define r (user-get-write-evt imm-bstr imm-start imm-end))
    (unless (evt? r)
      (raise-result-error '|user port get-write-evt| "evt?" r))
    (port-lock self)
    (wrap-check-write-evt-result '|user port write-evt| r self imm-start imm-end #t))

  ;; with lock held, which implies in atomic mode
  (define (write-out-special self v non-block/buffer? enable-break?)
    (let ([enable-break? (and (not non-block/buffer?) (break-enabled))])
      (parameterize-break #f
        (with-no-lock self
          (user-write-out-special v non-block/buffer? enable-break?)))))

  ;; with lock held, which implies in atomic mode
  (define get-location
    (and user-get-location
         (make-get-location user-get-location)))

  ;; with lock held, which implies in atomic mode
  (define count-lines!
    (and user-count-lines!
         (lambda (self) (port-unlock self) (user-count-lines!) (port-lock self))))

  (define-values (init-offset file-position)
    (make-init-offset+file-position user-init-position))

  ;; with lock held, which implies in atomic mode
  (define buffer-mode
    (and user-buffer-mode
         (make-buffer-mode user-buffer-mode #:output? #t)))

  ;; with lock held, which implies in atomic mode
  (define (close self)
    (port-unlock self)
    (user-close)
    (port-lock self))

  (finish-port/count
   (port-lock-init-atomic-mode
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
         [buffer-mode buffer-mode]))))
