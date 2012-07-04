#lang racket/base

(require racket/string racket/class racket/gui/base racket/match racket/port
         framework syntax/to-string
         "report.rkt"
         unstable/sequence unstable/pretty
         images/icons/symbol)

(provide popup-callback make-color-table)

(define popup-width  500)
(define popup-height 300)

(define tt-style-delta (new style-delta%))
(send tt-style-delta set-family 'modern)

(define ((popup-callback entry) ed start end)
  (match-define (report-entry subs start end badness) entry)
  (define win (new frame% [label "Optimization Coach"]
                   [width popup-width] [height popup-height]))
  (define pane (new text% [auto-wrap #t]))
  (define canvas
    (new editor-canvas% [parent win] [editor pane] [style '(no-hscroll)]))
  (for-each (format-sub-report-entry pane) subs)
  (send canvas scroll-to 0 0 0 0 #t) ; display the beginning
  (send pane lock #t)
  (send win show #t))

;; each sub-entry is displayed in its own text%, contained in the main
;; editor, to simplify irritant highlighting
(define ((format-sub-report-entry pane) s)
  (match-define (sub-report-entry stx msg provenance) s)

  (define usable-width (- popup-width 20)) ; minus the scrollbar

  ;; the location, the syntax and the message are in separate editors
  (define location-text (new text:basic% [auto-wrap #t]))
  (define location (format "~a:~a:" (syntax-line stx) (syntax-column stx)))
  (send location-text insert-port (open-input-string location))
  (send location-text lock #t)
  ;; add to the main editor
  (send pane insert
        (new editor-snip% [editor location-text] [with-border? #f]))
  (send pane insert-port (open-input-string "\n"))

  (define syntax-text (new text:basic%))
  ;; typeset the syntax as code
  (send syntax-text change-style tt-style-delta)
  (send syntax-text insert-port
        (open-input-string (syntax->string #`(#,stx)))) ; takes a list of stxs
  ;; circle irritants, if necessary
  (when (missed-opt-report-entry? s)
    (for ([i (in-list (missed-opt-report-entry-irritants s))]
          #:when (syntax-position i))
      (define start (- (syntax-position i) (syntax-position stx)))
      (define len   (syntax-span i))
      ;; will be off if there are comments inside an irritant (span will be
      ;; higher than what's actually displayed), but unless we make the
      ;; located version of irritants available, this is the best we can do
      (send syntax-text highlight-range
            start (+ start len) "pink" #f 'high 'rectangle)))
  (send syntax-text set-max-width usable-width)
  (send syntax-text auto-wrap #t)
  (send syntax-text lock #t)
  (send pane insert
        (new editor-snip% [editor syntax-text] [max-width popup-width]
             [with-border? #f] [bottom-margin 10]))
  (send pane insert-port (open-input-string "\n"))

  (define message-text (new text:basic% [auto-wrap #t]))
  (send message-text insert
        (make-object image-snip% (if (missed-opt-report-entry? s)
                                     (x-icon #:height 20)
                                     (check-icon #:height 20))))
  (send message-text insert-port
        (open-input-string (string-append "  " msg)))
  ;; adjust display
  (send message-text set-max-width usable-width)
  (send message-text auto-wrap #t)
  (send message-text lock #t)
  (send pane insert
        (new editor-snip% [editor message-text] [max-width popup-width]
             [with-border? #f] [top-margin 10] [bottom-margin 15]))

  ;; to place the next sub-entry below
  (send pane insert-port (open-input-string "\n\n"))
  (define line-bitmap (make-object bitmap% usable-width 5))
  (define bitmap-dc (make-object bitmap-dc% line-bitmap))
  (send bitmap-dc draw-line 0 2.5 usable-width 2.5)
  (send pane insert (make-object image-snip% line-bitmap))
  (send pane insert-port (open-input-string "\n\n")))

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
