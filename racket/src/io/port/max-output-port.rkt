#lang racket/base
(require "../common/class.rkt"
         "../host/thread.rkt"
         "output-port.rkt"
         "bytes-output.rkt")

(provide make-max-output-port
         max-output-port-max-length)

(class max-output-port #:extends core-output-port
  #:field
  [o #f]
  [max-length 0]
  #:override
  [write-out
   (lambda (src-bstr src-start src-end nonblock? enable-break? copy?)
     (cond
       [max-length
        (define len (- src-end src-start))
        (unless (eq? max-length 'full)
          (define write-len (min len max-length))
          (end-atomic)
          (define wrote-len (write-bytes src-bstr o src-start (+ src-start write-len)))
          (start-atomic)
          (if (= max-length wrote-len)
              (set! max-length 'full)
              (set! max-length (- max-length wrote-len))))
        len]
       [else
        (end-atomic)
        (define len (write-bytes src-bstr o src-start src-end))
        (start-atomic)
        len]))])

(define (make-max-output-port o max-length)
  (new max-output-port
       #:field
       [name (object-name o)]
       [evt o]
       [o o]
       [max-length max-length]))
