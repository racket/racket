#lang racket/base

(require racket/string unstable/pretty racket/class racket/gui/base
         unstable/sequence)

(provide format-message make-color-table)

(define (format-message stxs+msgs)
  (string-join (for/list ([(stx msg) (in-pairs stxs+msgs)])
                 (format "~a:~a: ~a~a"
                         (syntax-line stx)
                         (syntax-column stx)
                         (pretty-format/write (syntax->datum stx))
                         msg))
               "\n\n"))

(define lowest-badness-color  (make-object color% "pink"))
(define highest-badness-color (make-object color% "red"))
;; the higher the badness, the closer to red the highlight should be
(define (make-color-table max-badness)
  (define min-g (send highest-badness-color green))
  (define max-g (send lowest-badness-color  green))
  (define min-b (send highest-badness-color blue))
  (define max-b (send lowest-badness-color  blue))
  (define delta-g (- max-g min-g))
  (define delta-b (- max-b min-b))
  (define bucket-size-g (quotient delta-g max-badness))
  (define bucket-size-b (quotient delta-b max-badness))
  (build-vector (add1 max-badness) ; to index directly using badness
                (lambda (x)
                  (make-object
                   color%
                   255
                   ;; clipping, since the first (unused, for
                   ;; badness of 0) would have invalid components
                   (min 255 (- max-g (* (sub1 x) bucket-size-g)))
                   (min 255 (- max-b (* (sub1 x) bucket-size-b)))))))
