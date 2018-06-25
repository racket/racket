#lang racket/base
(require "../common/check.rkt"
         "../host/rktio.rkt"
         "../port/input-port.rkt"
         "../port/bytes-input.rkt")

(provide sha1-bytes
         sha224-bytes
         sha256-bytes)

(define/who (sha1-bytes in [start 0] [end #f])
  (sha who
       in start end
       (rktio_make_sha1_ctx)
       RKTIO_SHA1_DIGEST_SIZE
       rktio_sha1_init
       rktio_sha1_update
       rktio_sha1_final))

(define/who (sha224-bytes in [start 0] [end #f])
  (sha who
       in start end
       (rktio_make_sha2_ctx)
       RKTIO_SHA224_DIGEST_SIZE
       (lambda (p) (rktio_sha2_init p #t))
       rktio_sha2_update
       rktio_sha2_final))

(define/who (sha256-bytes in [start 0] [end #f])
  (sha who
       in start end
       (rktio_make_sha2_ctx)
       RKTIO_SHA256_DIGEST_SIZE
       (lambda (p) (rktio_sha2_init p #f))
       rktio_sha2_update
       rktio_sha2_final))

(define (sha who in start end p sz init update final)
  (check who (lambda (p) (or (bytes? p) (input-port? p))) in)
  (check who exact-nonnegative-integer? start)
  (when (bytes? in)
    (unless (<= 0 start (bytes-length in))
      (raise-range-error who
                         "byte string"
                         "starting "
                         start
                         in
                         0
                         (bytes-length in)
                         #f)))
  (when end
    (check who #:or-false exact-nonnegative-integer? end)
    (if (bytes? in)
        (unless (<= start end (bytes-length in))
          (raise-range-error who
                             "byte string"
                             "ending "
                             end
                             in
                             0
                             (bytes-length in)
                             start))
        (unless (start . <= . end)
          (raise-arguments-error who
                                 "ending index is smaller than starting index"
                                 "starting index" start
                                 "ending index" end))))
  (init p)
  (cond
    [(bytes? in)
     (update p in start (or end (bytes-length in)))]
    [else
     (define buffer-size (min 256 (if end (- end start) 256)))
     (define buffer (make-bytes buffer-size))
     ;; Discard bytes until `start` goes to 0....
     (let loop ([skip start])
       (cond
         [(zero? skip)
          ;; Read up to `(- end start)` bytes and hash
          (let loop ([len (and end (- end start))])
            (unless (and len (zero? len))
              (define got (read-bytes! buffer in 0 (if len
                                                       (min len buffer-size)
                                                       buffer-size)))
              (unless (eof-object? got)
                (update p buffer 0 got)
                (loop (and len (- len got))))))]
         [else
          (define got (read-bytes! buffer in 0 (min skip buffer-size)))
          (unless (eof-object? got)
            (loop (- skip got)))]))])
  (define bstr (make-bytes sz))
  (final p bstr)
  bstr)
