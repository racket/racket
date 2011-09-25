#lang racket/base
(require racket/gui/base)
(provide frame:lookup-focus-table
         frame:set-focus-table)

;; focus-table : hash[eventspace -o> (listof frame)]
(define focus-table (make-hash))
(define (frame:lookup-focus-table [eventspace (current-eventspace)])
  (hash-ref focus-table eventspace '()))
(define (frame:set-focus-table eventspace new)
  (if (null? new)
      (hash-remove! focus-table eventspace)
      (hash-set! focus-table eventspace new)))
