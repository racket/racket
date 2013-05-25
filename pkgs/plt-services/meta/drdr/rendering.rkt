#lang racket
(require "list-count.rkt")

(define-struct rendering (start end duration timeout? unclean-exit? stderr? responsible changed?) #:prefab)

(define (rendering-responsibles r)
  (regexp-split #rx"," (rendering-responsible r)))

(provide/contract
 [struct rendering ([start number?]
                    [end number?]
                    [duration number?]
                    [timeout? list/count]
                    [unclean-exit? list/count]
                    [stderr? list/count]
                    [responsible string?]
                    [changed? list/count])]
 [rendering-responsibles (rendering? . -> . (listof string?))])
