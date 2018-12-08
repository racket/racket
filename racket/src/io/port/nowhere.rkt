#lang racket/base
(require "output-port.rkt")

(provide open-output-nowhere)

(define (open-output-nowhere)
  (make-core-output-port #:name 'nowhere
                         #:self #f
                          #:evt always-evt
                          #:write-out (lambda (self bstr start-k end-k no-block/buffer? enable-break? copy?)
                                        (- end-k start-k))
                          #:close void
                          #:write-out-special (lambda (self any no-block/buffer? enable-break?)
                                                #t)))
