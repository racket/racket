#lang racket/base
(require racket/fixnum
         "../common/internal-error.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "../string/utf-8-decode.rkt"
         "port.rkt"
         "input-port.rkt"
         "lock.rkt"
         "count.rkt"
         "check.rkt"
         "prepare-change.rkt"
         "progress-evt.rkt")

(provide read-some-bytes!
         peek-some-bytes!

         read-a-byte
         peek-a-byte
         peek-byte-via-bytes

         maybe-read-a-line)

;; Read up to `(- end start)` bytes, producing at least a
;; single by unless `zero-ok?` is true. The result is
;; EOF or the number of bytes read.
(define (read-some-bytes! who orig-in bstr start end
                          ;; Zero is ok for `read-bytes!*`:
                          #:zero-ok? [zero-ok? #f]
                          ;; Enable breaks while blocking?
                          #:enable-break? [enable-break? #f]
                          ;; When calling an externally implemented
                          ;; port, we normally make a fresh byte
                          ;; string, because we don't trust the
                          ;; reading proc to not retain the byte
                          ;; string and change it later. We can skip
                          ;; the copy if the bstr is the right length
                          ;; and won't be exposed, though.
                          #:copy-bstr? [copy-bstr? #t]
                          ;; If `keep-eof?`, don't consume an EOF
                          #:keep-eof? [keep-eof? #f]
                          ;; If not `special-ok?` and a special value is
                          ;; received, raise an exception
                          #:special-ok? [special-ok? #t]
                          ;; For a special result, limit the procedure
                          ;; to 4 unless `read-byte-or-special`, etc.,
                          ;; need access to a 0-argument version
                          #:limit-special-arity? [limit-special-arity? #t])
  (let loop ([in orig-in] [extra-count-ins null])
    (port-lock in)
    (prepare-change in)
    (cond
      [(fx= start end) ; intentionally before the port-closed check
       (port-unlock in)
       0]
      [(core-port-closed? in)
       (check-not-closed who in)]
      ;; previously detected EOF?
      [(core-input-port-pending-eof? in)
       (unless keep-eof?
         (set-core-input-port-pending-eof?! in #f))
       (port-unlock in)
       eof]
      [else
       (define buffer (core-port-buffer in))
       (define buf-pos (direct-pos buffer))
       (define buf-end (direct-end buffer))
       (cond
         [(buf-pos . fx< . buf-end)
          ;; Read bytes directly from buffer
          (define v (fxmin (fx- buf-end buf-pos) (fx- end start)))
          (define new-pos (fx+ buf-pos v))
          (bytes-copy! bstr start (direct-bstr buffer) buf-pos new-pos)
          (set-direct-pos! buffer new-pos)
          (when (or (pair? extra-count-ins) (core-port-count in))
            (port-count-all! in extra-count-ins v bstr start))
          (port-unlock in)
          v]
         [else
          ;; Call port's `read-in` method
          (define read-in (method core-input-port in read-in))
          (cond
            [(procedure? read-in)
             (define v (read-in in bstr start end copy-bstr?))
             (let result-loop ([v v])
               (cond
                 [(and (integer? v) (not (eq? v 0)))
                  (port-count-all! in extra-count-ins v bstr start)]
                 [(procedure? v)
                  (port-count-byte-all! in extra-count-ins #f)])
               (port-unlock in)
               (cond
                 [(exact-nonnegative-integer? v)
                  (cond
                    [(zero? v)
                     (if zero-ok?
                         0
                         (loop in extra-count-ins))]
                    [(v . <= . (- end start)) v]
                    [else
                     (raise-arguments-error who
                                            "result integer is larger than the supplied byte string"
                                            "result" v
                                            "byte-string length" (- end start))])]
                 [(eof-object? v) eof]
                 [(semaphore? v)
                  ;; A semaphore is treated as a special case, making
                  ;; it equivalent to an evt that returns 0
                  (cond
                    [zero-ok?
                     ;; Poll:
                     (cond
                       [(semaphore-try-wait? v)
                        (loop in extra-count-ins)]
                       [else 0])]
                    [else
                     (if enable-break?
                         (semaphore-wait/enable-break v)
                         (semaphore-wait v))
                     (loop in extra-count-ins)])]
                 [(evt? v)
                  ;; If `zero-ok?`, we should at least poll the event
                  (define timeout (if zero-ok? (lambda () 0) #f))
                  (define next-v (if enable-break?
                                     (sync/timeout/enable-break timeout v)
                                     (sync/timeout timeout v)))
                  (cond
                    [(and zero-ok? (evt? next-v))
                     ;; Avoid looping on events
                     0]
                    [else
                     (port-lock in)
                     (result-loop next-v)])]
                 [(procedure? v)
                  (if special-ok?
                      (if limit-special-arity?
                          (lambda (a b c d) (v a b c d))
                          v)
                      (raise-arguments-error who
                                             "non-character in an unsupported context"
                                             "port" orig-in))]
                 [else
                  (internal-error (format "weird read-bytes result ~s" v))]))]
            [else
             (port-unlock in)
             (loop (->core-input-port read-in) (cons in extra-count-ins))])])])))

;; Like `read-some-bytes!`, but merely peeks
(define (peek-some-bytes! who orig-in bstr start end skip
                          #:progress-evt [progress-evt #f]
                          #:zero-ok? [zero-ok? #f]
                          #:enable-break? [enable-break? #f]
                          #:copy-bstr? [copy-bstr? #t]
                          #:special-ok? [special-ok? #t]
                          #:limit-special-arity? [limit-special-arity? #t])
  (let loop ([in orig-in])
    (port-lock in)
    (prepare-change in)
    (cond
      [(= start end)
       (port-unlock in)
       0]
      ;; check progress evt before continuing with other possibilities
      [(and progress-evt
            ;; the lock must require atomic mode if we get progress-evt
            (sync/timeout 0 progress-evt))
       (port-unlock in)
       0]
      [(core-port-closed? in)
       (check-not-closed who in)]
      ;; previously detected EOF? (never skip past it)
      [(core-input-port-pending-eof? in)
       (port-unlock in)
       eof]
      [else
       (define buffer (core-port-buffer in))
       (define buf-pos (+ (direct-pos buffer) skip))
       (define buf-end (direct-end buffer))
       (cond
         [(buf-pos . < . buf-end)
          ;; Copy bytes from buffer
          (define v (min (- buf-end buf-pos) (- end start)))
          (bytes-copy! bstr start (direct-bstr buffer) buf-pos (fx+ buf-pos v))
          (port-unlock in)
          v]
         [else
          (define peek-in (method core-input-port in peek-in))
          (cond
            [(procedure? peek-in)
             (define v (peek-in in bstr start end skip progress-evt copy-bstr?))
             (port-unlock in)
             (let result-loop ([v v])
               (cond
                 [(exact-nonnegative-integer? v)
                  (cond
                    [(zero? v)
                     (if zero-ok?
                         0
                         (loop in))]
                    [(v . <= . (- end start)) v]
                    [else
                     (raise-arguments-error who
                                            "result integer is larger than the supplied byte string"
                                            "result" v
                                            "byte-string length" (- end start))])]
                 [(eof-object? v) eof]
                 [(semaphore? v)
                  (cond
                    [zero-ok? 0]
                    [else
                     (if enable-break?
                         (semaphore-wait/enable-break v)
                         (semaphore-wait v))
                     (loop in)])]
                 [(evt? v)
                  (cond
                    [zero-ok?
                     ;; can return 0, but should poll the evt; a port might
                     ;; take polling as a sign that it needs to make progress
                     (define r (sync/timeout 0 v))
                     (if r
                         (result-loop r)
                         0)]
                    [else (result-loop (if enable-break?
                                           (sync/enable-break v)
                                           (sync v)))])]
                 [(procedure? v)
                  (if special-ok?
                      (if limit-special-arity?
                          (lambda (a b c d) (v a b c d))
                          v)
                      (raise-arguments-error who
                                             "non-character in an unsupported context"
                                             "port" orig-in))]
                 [else
                  (internal-error (format "weird peek-bytes result ~s" v))]))]
            [else
             (port-unlock in)
             (loop (->core-input-port peek-in))])])])))

;; Try the buffer shortcut first
(define (read-a-byte who in #:special-ok? [special-ok? #f])
  (port-lock in)
  (define buffer (core-port-buffer in))
  (define pos (direct-pos buffer))
  (cond
    [(pos . fx< . (direct-end buffer))
     (define b (bytes-ref (direct-bstr buffer) pos))
     (set-direct-pos! buffer (fx+ pos 1))
     (when (core-port-count in)
       (port-count-byte! in b))
     (port-unlock in)
     b]
    [else
     (port-unlock in)
     (read-byte-via-bytes who in #:special-ok? special-ok?)]))

;; Use the general path; may return a procedure for a special
(define (read-byte-via-bytes who in #:special-ok? [special-ok? #t])
  (define bstr (make-bytes 1))
  (define v (read-some-bytes! who in bstr 0 1
                              #:copy-bstr? #f
                              #:special-ok? special-ok?
                              #:limit-special-arity? #f))
  (if (eq? v 1)
      (bytes-ref bstr 0)
      v))

;; Try the buffer shortcut first
(define (peek-a-byte who in skip-k #:special-ok? [special-ok? #f])
  (port-lock in)
  (define buffer (core-port-buffer in))
  (define pos (+ (direct-pos buffer) skip-k))
  (cond
    [(pos . < . (direct-end buffer))
     (define b (bytes-ref (direct-bstr buffer) pos))
     (port-unlock in)
     b]
    [else
     (port-unlock in)
     (peek-byte-via-bytes who in skip-k #:special-ok? special-ok?)]))

;; Use the general path; may return a procedure for a special
(define (peek-byte-via-bytes who in skip-k
                             #:special-ok? [special-ok? #t]
                             #:progress-evt [progress-evt #f])
  (define bstr (make-bytes 1))
  (define v (peek-some-bytes! who in bstr 0 1 skip-k
                              #:copy-bstr? #f
                              #:special-ok? special-ok?
                              #:limit-special-arity? #f
                              #:progress-evt progress-evt))
  (if (eq? v 1)
      (bytes-ref bstr 0)
      v))

;; ----------------------------------------

;; Tries to read a line from the fast-path buffer
(define (maybe-read-a-line in cr? lf? crlf? as-string?)
  (port-lock in)
  (define buffer (core-port-buffer in))
  (define bstr (direct-bstr buffer))
  (define pos (direct-pos buffer))
  (define end (fxmin (direct-end buffer)
                     ;; limit atomicity
                     (fx+ pos 4096)))
  (define (finish end read-end)
    (set-direct-pos! buffer read-end)
    (when (core-port-count in)
      (port-count! in (fx- read-end pos) bstr pos))
    (define result
      (if as-string?
          (a-bytes->string/utf-8 bstr pos end)
          (subbytes bstr pos end)))
    (port-unlock in)
    result)
  (let loop ([i pos])
    (cond
      [(fx= i end)
       (port-unlock in)
       #f]
      [else
       (define b (bytes-ref bstr i))
       (cond
         [(and lf? (eqv? b (char->integer #\linefeed)))
          (finish i (fx+ i 1))]
         [(and (or cr? crlf?) (eqv? b (char->integer #\return)))
          (cond
            [(and crlf?
                  ((fx+ i 1) . fx< . end)
                  (eqv? (bytes-ref bstr (fx+ i 1)) (char->integer #\linefeed)))
             (finish i (fx+ i 2))]
            [cr?
             (cond
               [(and crlf?
                     ((fx+ i 1) . fx= . end))
                ;; needs more input to decide
                (port-unlock in)
                #f]
               [else
                (finish i (fx+ i 1))])]
            [else (loop (fx+ i 1))])]
         [else (loop (fx+ i 1))])])))
