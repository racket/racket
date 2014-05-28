#lang racket/base
(require racket/class
         racket/gui/base
         racket/format
         string-constants
         (prefix-in db: pkg/db)
         "common.rkt")

(provide make-filter-panel)

(define (make-filter-panel parent changed!)

  (define filter-panel
    (new horizontal-panel%
         [parent parent]
         [stretchable-height #f]))

  (define keep-rx #rx"")
  
  (define filter-text
    (new text-field%
         [label (~a (string-constant install-pkg-filter) ":")]
         [parent filter-panel]
         [font small-control-font]
         [stretchable-width #t]
         [callback (lambda (tf e)
                     (define s (send tf get-value))
                     (define terms (filter (lambda (s) (not (string=? s "")))
                                           (regexp-split #rx"[, \t\r\n]" s)))
                     (define rx
                       (regexp (apply ~a
                                      #:separator "|"
                                      (for/list ([term terms])
                                        (~a "(?i:" (regexp-quote term) ")")))))
                     (unless (equal? rx keep-rx)
                       (set! keep-rx rx)
                       (changed!)))]))
  
  (define filter-result
    (new message%
         [label "9999/9999 match"]
         [parent filter-panel]
         [font small-control-font]))
  (send filter-result set-label "")

  (new (class object%
         (super-new)
         (define/public (get-rx) keep-rx)
         (define/public (set-result match-count out-of-count)
           (send filter-result set-label (format "~a/~a match" match-count out-of-count))))))
