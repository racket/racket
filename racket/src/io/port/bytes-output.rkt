#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "output-port.rkt"
         "parameter.rkt"
         "write.rkt"
         "check.rkt")

(provide write-byte
         write-bytes
         write-bytes-avail
         write-bytes-avail*
         write-bytes-avail/enable-break
         write-bytes-avail-evt
         port-writes-atomic?)

(module+ internal
  (provide do-write-bytes))

(define/who (write-byte b [out (current-output-port)])
  (check who byte? b)
  (check who output-port? out)
  (let ([out (->core-output-port out)])
    (write-some-bytes 'write-byte out (bytes b) 0 1 #:buffer-ok? #t #:copy-bstr? #f))
  (void))

(define (do-write-bytes who out bstr start end)
  (let loop ([i start])
    (cond
      [(= i end) (- i start)]
      [else
       (define n (write-some-bytes who out bstr i end #:buffer-ok? #t))
       (loop (+ n i))])))

(define/who (write-bytes bstr [out (current-output-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                      (bytes-length bstr))])
  (check who bytes? bstr)
  (check who output-port? out)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? end-pos)
  (check-range who start-pos end-pos (bytes-length bstr) bstr)
  (let ([out (->core-output-port out)])
    (do-write-bytes who out bstr start-pos end-pos)))

(define (do-write-bytes-avail who bstr out start-pos end-pos
                              #:zero-ok? [zero-ok? #f]
                              #:enable-break? [enable-break? #f])
  (check who bytes? bstr)
  (check who output-port? out)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? end-pos)
  (check-range who start-pos end-pos (bytes-length bstr) bstr)
  (let ([out (->core-output-port out)])
    (write-some-bytes who out bstr start-pos end-pos #:zero-ok? zero-ok? #:enable-break? enable-break?)))

(define/who (write-bytes-avail bstr [out (current-output-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                            (bytes-length bstr))])
  (do-write-bytes-avail who bstr out start-pos end-pos))

(define/who (write-bytes-avail* bstr [out (current-output-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                             (bytes-length bstr))])
  (do-write-bytes-avail who bstr out start-pos end-pos #:zero-ok? #t))

(define/who (write-bytes-avail/enable-break bstr [out (current-output-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                                         (bytes-length bstr))])
  (do-write-bytes-avail who bstr out start-pos end-pos #:enable-break? #t))

(define/who (write-bytes-avail-evt bstr [out (current-output-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                                (bytes-length bstr))])
  (check who bytes? bstr)
  (check who output-port? out)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? end-pos)
  (check-range who start-pos end-pos (bytes-length bstr) bstr)
  (let ([out (->core-output-port out)])
    (atomically
     (check-not-closed who out)
     (define get-write-evt (core-output-port-get-write-evt out))
     (unless get-write-evt
       (end-atomic)
       (raise-arguments-error who
                              "port does not support output events"
                              "port" out))
     (get-write-evt bstr start-pos end-pos))))

(define/who (port-writes-atomic? out)
  (check who output-port? out)
  (let ([out (->core-output-port out)])
    (and (core-output-port-get-write-evt out) #t)))
