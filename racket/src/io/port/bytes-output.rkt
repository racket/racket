#lang racket/base
(require racket/fixnum
         "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "count.rkt"
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
         port-writes-atomic?

         unsafe-write-bytes)

(module+ internal
  (provide do-write-bytes))

(define/who (write-byte b [out (current-output-port)])
  (check who byte? b)
  (check who output-port? out)
  (let ([out (->core-output-port out)])
    (start-atomic)
    (define buffer (core-port-buffer out))
    (define pos (direct-pos buffer))
    (cond
      [(pos . fx< . (direct-end buffer))
       (bytes-set! (direct-bstr buffer) pos b)
       (set-direct-pos! buffer (fx+ pos 1))
       (when (core-port-count out)
         (port-count-byte! out b))
       (end-atomic)]
      [else
       (end-atomic)
       (write-some-bytes 'write-byte out (bytes b) 0 1 #:buffer-ok? #t #:copy-bstr? #f)]))
  (void))

(define (do-write-bytes who out bstr start end)
  (let loop ([i start])
    (cond
      [(fx= i end) (fx- i start)]
      [else
       (define n (write-some-bytes who out bstr i end #:buffer-ok? #t))
       (loop (fx+ n i))])))

(define/who write-bytes
  (case-lambda
    [(bstr)
     (check who bytes? bstr)
     (let ([out (->core-output-port (current-output-port))])
       (do-write-bytes who out bstr 0 (bytes-length bstr)))]
    [(bstr out)
     (check who bytes? bstr)
     (let ([out (->core-output-port out who)])
       (do-write-bytes who out bstr 0 (bytes-length bstr)))]
    [(bstr out start-pos)
     (write-bytes bstr out start-pos (and (bytes? bstr) (bytes-length bstr)))]
    [(bstr out start-pos end-pos)
     (check who bytes? bstr)
     (let ([out (->core-output-port out who)])
       (check who exact-nonnegative-integer? start-pos)
       (check who exact-nonnegative-integer? end-pos)
       (check-range who start-pos end-pos (bytes-length bstr) bstr)
       (do-write-bytes who out bstr start-pos end-pos))]))

;; `o` must be a core output port
(define (unsafe-write-bytes who bstr o)
  (do-write-bytes who o bstr 0 (bytes-length bstr)))

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
     (define get-write-evt (method core-output-port out get-write-evt))
     (unless get-write-evt
       (end-atomic)
       (raise-arguments-error who
                              "port does not support output events"
                              "port" out))
     (get-write-evt out bstr start-pos end-pos))))

(define/who (port-writes-atomic? out)
  (check who output-port? out)
  (let ([out (->core-output-port out)])
    (and (method core-output-port out get-write-evt) #t)))
