#lang racket/base

(require racket/string racket/class racket/gui/base racket/match racket/port
         "report.rkt"
         unstable/sequence unstable/pretty)

(provide popup-callback make-color-table)

(define (format-message stx msg)
  (let* ([location (format "~a:~a: " (syntax-line stx) (syntax-column stx))]
         [message  (format "~a~a\n\n~a" location (syntax->datum stx) msg)])
    ;; return the message and the starting location of the syntax object
    (values message (string-length location))))

(define ((popup-callback entry) ed start end)
  (match-define (report-entry subs start end badness) entry)
  (define text (new text%))
  (define win (new dialog% [label "Performance Report"]
                   [width 500] [height 300]))
  (define pane (new vertical-pane% [parent win] [alignment '(left center)]))
  (for ([s (in-list subs)])
    (match-define (sub-report-entry stx msg) s)
    (define-values (message stx-start) (format-message stx msg))
    (define text (new text%))
    (send text auto-wrap #t)
    (send text insert-port (open-input-string message))
    (send text lock #t)
    (new editor-canvas% [parent pane] [editor text]
         [style '(no-hscroll no-vscroll)]))
  (send win show #t))

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
