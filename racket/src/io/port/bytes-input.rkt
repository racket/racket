#lang racket/base
(require "../common/check.rkt"
         "parameter.rkt"
         "read-and-peek.rkt"
         "input-port.rkt"
         "progress-evt.rkt"
         "flush-output.rkt")

(provide read-byte
         read-bytes
         read-bytes!
         read-bytes-avail!
         read-bytes-avail!*
         read-bytes-avail!/enable-break
         
         peek-byte
         peek-bytes
         peek-bytes!
         peek-bytes-avail!
         peek-bytes-avail!*
         peek-bytes-avail!/enable-break)

(module+ internal
  (provide do-read-bytes!))

;; ----------------------------------------

;; Read `(- end start)` bytes, stopping early only if an EOF is found
(define (do-read-bytes! who in bstr start end)
  (define amt (- end start))
  (define v (read-some-bytes! who in bstr start end))
  (cond
    [(not (exact-integer? v)) v]
    [(= v amt) v]
    [else
     (let loop ([got v])
       (define v (read-some-bytes! who in bstr (+ start got) end #:keep-eof? #t #:special-ok? #f))
       (cond
         [(eof-object? v)
          got]
         [else
          (define new-got (+ got v))
          (cond
            [(= new-got amt) amt]
            [else (loop new-got)])]))]))

;; ----------------------------------------

(define/who (read-byte [orig-in (current-input-port)])
  (let ([in (->core-input-port orig-in who)])
    (read-a-byte who in)))

(define/who (read-bytes amt [in (current-input-port)])
  (check who exact-nonnegative-integer? amt)
  (check who input-port? in)
  (maybe-flush-stdout in)
  (let ([in (->core-input-port in)])
    (define bstr (make-bytes amt))
    (define v (do-read-bytes! 'read-bytes in bstr 0 amt))
    (if (exact-integer? v)
        (if (= v amt)
            bstr
            (subbytes bstr 0 v))
        v)))

(define/who (read-bytes! bstr [in (current-input-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                    (bytes-length bstr))])
  (check who bytes? bstr)
  (check who input-port? in)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? end-pos)
  (check-range who start-pos end-pos (bytes-length bstr) bstr)
  (maybe-flush-stdout in)
  (let ([in (->core-input-port in)])
    (do-read-bytes! who in bstr start-pos end-pos)))

(define (do-read-bytes-avail! who bstr in start-pos end-pos
                              #:zero-ok? [zero-ok? #f]
                              #:enable-break? [enable-break? #f])
  (check who bytes? bstr)
  (check who input-port? in)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? end-pos)
  (check-range who start-pos end-pos (bytes-length bstr) bstr)
  (maybe-flush-stdout in)
  (let ([in (->core-input-port in)])
    (read-some-bytes! who in bstr start-pos end-pos #:zero-ok? zero-ok? #:enable-break? enable-break?)))

(define/who (read-bytes-avail! bstr [in (current-input-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                          (bytes-length bstr))])
  (do-read-bytes-avail! who bstr in start-pos end-pos))
   
(define/who (read-bytes-avail!* bstr [in (current-input-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                           (bytes-length bstr))])
  (do-read-bytes-avail! who bstr in start-pos end-pos #:zero-ok? #t))

(define/who (read-bytes-avail!/enable-break bstr [in (current-input-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                                       (bytes-length bstr))])
  (do-read-bytes-avail! who bstr in start-pos end-pos #:enable-break? #t))

;; ----------------------------------------

;; Peek `(- end start)` bytes, stopping early only if an EOF is found
(define (do-peek-bytes! who in bstr start end skip)
  (define amt (- end start))
  (define v (peek-some-bytes! who in bstr start end skip))
  (if (exact-integer? v)
      (cond
       [(= v amt) v]
       [else
        (let loop ([got v])
          (define v (peek-some-bytes! who in bstr got amt (+ got skip) #:copy-bstr? #f #:special-ok? #f))
          (cond
           [(eof-object? v)
            got]
           [else
            (define new-got (+ got v))
            (cond
             [(= new-got amt) amt]
             [else (loop new-got)])]))])
      v))

(define/who (peek-byte [orig-in (current-input-port)] [skip-k 0])
  (let ([in (->core-input-port orig-in who)])
    (check who exact-nonnegative-integer? skip-k)
    (peek-a-byte who in skip-k)))

(define/who (peek-bytes amt skip-k [in (current-input-port)])
  (check who exact-nonnegative-integer? amt)
  (check who exact-nonnegative-integer? skip-k)
  (check who input-port? in)
  (maybe-flush-stdout in)
  (let ([in (->core-input-port in)])
    (define bstr (make-bytes amt))
    (define v (do-peek-bytes! 'read-bytes in bstr 0 amt skip-k))
    (if (exact-integer? v)
        (if (= v amt)
            bstr
            (subbytes bstr 0 v))
        v)))

(define/who (peek-bytes! bstr skip-k [in (current-input-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                           (bytes-length bstr))])
  (check who bytes? bstr)
  (check who exact-nonnegative-integer? skip-k)
  (check who input-port? in)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? end-pos)
  (check-range who start-pos end-pos (bytes-length bstr) bstr)
  (maybe-flush-stdout in)
  (let ([in (->core-input-port in)])
    (do-peek-bytes! who in bstr start-pos end-pos skip-k)))

(define (do-peek-bytes-avail! who bstr skip-k progress-evt in start-pos end-pos
                              #:zero-ok? [zero-ok? #f]
                              #:enable-break? [enable-break? #f])
  (check who bytes? bstr)
  (check who exact-nonnegative-integer? skip-k)
  (check who (lambda (e) (or (not e) (progress-evt? e)))
         #:contract "(or/c #f progress-evt?)" progress-evt)
  (check who input-port? in)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? end-pos)
  (when progress-evt
    (check-progress-evt who progress-evt in))
  (check-range who start-pos end-pos (bytes-length bstr) bstr)
  (maybe-flush-stdout in)
  (let ([in (->core-input-port in)])
    (peek-some-bytes! who in bstr start-pos end-pos skip-k
                      #:progress-evt (unwrap-progress-evt progress-evt)
                      #:zero-ok? zero-ok?
                      #:enable-break? enable-break?)))

(define/who (peek-bytes-avail! bstr skip-k [progress-evt #f] [in (current-input-port)]
                               [start-pos 0] [end-pos (and (bytes? bstr)
                                                           (bytes-length bstr))])
  (do-peek-bytes-avail! who bstr skip-k progress-evt in start-pos end-pos))

(define/who (peek-bytes-avail!* bstr skip-k [progress-evt #f] [in (current-input-port)]
                                [start-pos 0] [end-pos (and (bytes? bstr)
                                                            (bytes-length bstr))])
  (do-peek-bytes-avail! who bstr skip-k progress-evt in start-pos end-pos
                        #:zero-ok? #t))

(define/who (peek-bytes-avail!/enable-break bstr skip-k [progress-evt #f] [in (current-input-port)]
                                            [start-pos 0] [end-pos (and (bytes? bstr)
                                                                        (bytes-length bstr))])
  (do-peek-bytes-avail! who bstr skip-k progress-evt in start-pos end-pos
                        #:enable-break? #t))
