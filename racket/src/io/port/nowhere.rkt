#lang racket/base
(require "../common/class.rkt"
         "output-port.rkt"
         "count.rkt")

(provide open-output-nowhere)

(class nowhere-output-port #:extends core-output-port
  #:override
  [write-out-special
   (lambda (any no-block/buffer? enable-break?)
     #t)])

(define (open-output-nowhere)
  (finish-port/count
   (new nowhere-output-port
        #:field
        [name 'nowhere])))
