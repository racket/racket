#lang racket/base
(require "rktio.rkt")

(provide processor-count)

(define (processor-count)
  (rktio_processor_count rktio))
