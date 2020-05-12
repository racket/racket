#lang racket/base
(require "../common/class.rkt"
         "../host/thread.rkt"
         "../print/more-pending.rkt"
         "output-port.rkt"
         "bytes-output.rkt")

(provide make-max-output-port
         max-output-port-max-length)

(class max-output-port #:extends core-output-port
  #:field
  [o #f]
  [max-length 0] ;; see "../print/write-with-max.rkt"
  #:override
  [write-out
   (lambda (src-bstr src-start src-end nonblock? enable-break? copy?)
     (cond
       [max-length
        (define len (- src-end src-start))
        (cond
          [(eq? max-length 'full)
           ;; all consumed
           len]
          [(pair? max-length)
           (set! max-length (more-pending max-length src-start src-end src-bstr))
           ;; all consumed
           len]
          [else
           (define write-len (min len max-length))
           (end-atomic)
           (define wrote-len (write-bytes src-bstr o src-start (+ src-start write-len)))
           (start-atomic)
           (cond
             [(= max-length wrote-len)
              (set! max-length (more-pending '(0 . #"") (+ src-start max-length) src-end src-bstr))
              ;; all consumed
              len]
             [else
              (set! max-length (- max-length wrote-len))
              wrote-len])])]
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
