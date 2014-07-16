#lang racket/base
(require racket/class wxme)
(provide reader)
(define reader
  (class* object% (snip-reader<%>)
    (define/public (read-header) (void))
    (define/public (read-snip text-only? version stream)
      (send stream read-raw-bytes 'ellipsis-snip) ;; discard the 'extra' info
      #"...")
    (super-new)))