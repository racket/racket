#lang racket/base

(require racket/class racket/port racket/list racket/match unstable/sequence
         racket/gui/base racket/unit drracket/tool mrlib/switchable-button
         images/compile-time
         (for-syntax racket/base images/icons/misc images/icons/style))

(require "report.rkt" "display.rkt")

(provide tool@)

;; DrRacket tool for reporting missed optimizations in the editor.

(define performance-report-bitmap
  (compiled-bitmap (stopwatch-icon #:height (toolbar-icon-height))))

;; performance-report-callback : drracket:unit:frame<%> -> void
(define (performance-report-callback drr-frame)
  (with-handlers
      ([exn?
        ;; typechecking failed, report in the interactions window
        (lambda (e)
          (define interactions (send drr-frame get-interactions-text))
          (send interactions reset-console)
          (send interactions run-in-evaluation-thread (lambda () (raise e))))])
    (send (send drr-frame get-definitions-text) add-highlights)))

(define check-boxes
  `(("Report Typed Racket optimizations?" .
     ,(match-lambda [(sub-report-entry s m 'typed-racket) #t]
                    [_ #f]))
    ("Report inlining optimizations?" .
     ,(match-lambda [(sub-report-entry s m 'mzc) #t]
                    [_ #f]))))

(define highlights-mixin
  (mixin ((class->interface text%)) ()
    (inherit begin-edit-sequence
             end-edit-sequence
             insert
             get-text)

    (define highlights     '())
    (define color-table    #f)

    ;; filters : Listof (sub-report-entry -> Bool)
    ;; If any of these predicates return true for a given log entry's
    ;; sub, show it.
    ;; Note: at the point where these are called, report entries have
    ;; a single sub.
    (define filters (map cdr check-boxes)) ; all enabled by default
    (define/public (get-filters) filters)
    (define/public (set-filters! new-fs) (set! filters new-fs))

    (define/private (highlight-entry l)
      (match l
        [(report-entry subs start end badness)
         (let ([color (if (= badness 0)
                          "lightgreen"
                          (vector-ref color-table badness))])
           (send this highlight-range start end color #f 'high)
           (send this set-clickback start end (popup-callback l))
           ;; record highlight to undo it later
           (list start end color))]))

    (define report-cache #f)
    (define/public (add-highlights #:use-cache? [use-cache? #f])
      (clear-highlights)
      (send (send this get-tab) show-performance-report-panel)
      (unless (and report-cache use-cache?)
        (set! report-cache (generate-report this)))
      (define report
        (collapse-report
         (for/list ([entry (in-list report-cache)]
                    ;; At this point, report enties have a single sub.
                    #:when (for/or ([f (in-list filters)])
                             (f (first (report-entry-subs entry)))))
           entry)))
      (define max-badness
        (apply max (cons 0 (map report-entry-badness report))))
      (unless (= max-badness 0) ; no missed opts, color table code would error
        (set! color-table (make-color-table max-badness)))
      (define new-highlights
        (let loop ([report report])
          (cond
            [(null? report) highlights]
            [else (cons (highlight-entry (car report))
                        (loop (cdr report)))])))
      (set! highlights new-highlights))

    (define/public (clear-highlights)
      (for ([h (in-list highlights)])
        (match h
          [`(,start ,end . ,rest )
           (send this unhighlight-range . h)
           (send this remove-clickback start end)]))
      (set! highlights '())
      (send (send this get-tab) hide-performance-report-panel))

    (define/augment (on-insert start len)
      (clear-highlights))
    (define/augment (on-delete start len)
      (clear-highlights))

    (super-new)))


(define-unit tool@

  (import drracket:tool^)
  (export drracket:tool-exports^)

  (define (phase1) (void))
  (define (phase2) (void))

  (drracket:get/extend:extend-definitions-text highlights-mixin)

  (define button-mixin
    (mixin (drracket:unit:frame<%>) ()
      (super-new)
      (inherit get-button-panel get-definitions-text)
      (inherit register-toolbar-button)
      (let ((btn
             (new switchable-button%
                  (label "Performance Report")
                  (callback (lambda (btn)
                              (performance-report-callback this)))
                  (parent (get-button-panel))
                  (bitmap performance-report-bitmap))))
        (register-toolbar-button btn)
        (send (get-button-panel) change-children
              (λ (l)
                (cons btn (remq btn l)))))))

  (drracket:get/extend:extend-unit-frame button-mixin)

  (define toolbar-mixin
    (mixin (drracket:unit:tab<%>) ()
      (super-new)

      (inherit get-defs)

      (define panel #f)
      (define/public (get-panel) panel) ; called when switching tabs

      (define/public (show-performance-report-panel)
        (set! panel
              (new horizontal-panel%
                   [parent (send (send this get-frame) get-area-container)]
                   [stretchable-height #f]))
        (define definitions (send this get-defs))
        (new button%
             [label "Clear"]
             [parent panel]
             [callback (lambda _ (send definitions clear-highlights))])
        (define filters (send definitions get-filters))
        (for ([(l f) (in-pairs check-boxes)])
          (new check-box%
               [label l]
               [parent panel]
               [callback
                (lambda _
                  (send definitions set-filters! (if (memq f filters)
                                                     (remq f filters)
                                                     (cons f filters)))
                  ;; redraw
                  (send definitions add-highlights #:use-cache? #t))]
               [value (memq f filters)]))
        panel) ; return panel, so that the other mixing can hide it

      (define/public (hide-performance-report-panel [close #t])
        (when panel
          (send (send (send this get-frame) get-area-container)
                delete-child panel)
          (when close ; if we just switch tabs, keep panel around to restore it
            (set! panel #f))))))

  (drracket:get/extend:extend-tab toolbar-mixin)

  (define tab-switch-mixin
    (mixin (drracket:unit:frame<%>) ()
      (super-new)
      (define/augment (on-tab-change old-tab new-tab)
        (send old-tab hide-performance-report-panel #f) ; don't close it
        (when (send new-tab get-panel) ; if it was open before
          (send new-tab show-performance-report-panel)))))

  (drracket:get/extend:extend-unit-frame tab-switch-mixin))
