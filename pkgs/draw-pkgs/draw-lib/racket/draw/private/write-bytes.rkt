#lang racket/base
(require "../unsafe/cairo.rkt"
         "../unsafe/bstr.rkt")

(provide make-port-writer
         port-writer-wait)

(define (make-port-writer port) 
  (let ([t (thread/suspend-to-kill
            (lambda ()
              (let loop ()
                (let ([msg (thread-receive)])
                  (when (bytes? msg)
                    (write-bytes msg port)
                    (loop))))))])
    (values t
            (lambda (bytes len)
              (thread-send t (scheme_make_sized_byte_string bytes len 1) 
                           void)
              CAIRO_STATUS_SUCCESS))))

(define (port-writer-wait t)
  (thread-resume t)
  (thread-send t eof void)
  (thread-wait t))

