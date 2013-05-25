#lang racket/base
(require framework
         racket/class
         racket/snip)
(define snip-class (send (get-the-snip-class-list) find
                        (format "~s" `(lib "number-snip.ss" "drscheme" "private"))))
(provide snip-class)
;; this is here because there may be old number snips
;; out there that still point to this file
