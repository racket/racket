#lang racket/base
(require "rktio.rkt")

(provide processor-count)

(define (processor-count)
  1 #;(rktio_processor_count rktio))
