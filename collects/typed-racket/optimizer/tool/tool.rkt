#lang racket/base

(require racket/class racket/port racket/list racket/match unstable/sequence
         racket/gui/base racket/unit drracket/tool mrlib/switchable-button
         images/compile-time framework
         (for-syntax racket/base images/icons/misc images/icons/style)
         string-constants)

(require "report.rkt" "display.rkt")

(provide tool@ optimization-coach-drracket-button)

;; DrRacket tool for reporting missed optimizations in the editor.

(define optimization-coach-bitmap
  (compiled-bitmap (stopwatch-icon #:height (toolbar-icon-height))))

;; optimization-coach-callback : drracket:unit:frame<%> -> void
(define (optimization-coach-callback drr-frame)
  (with-handlers
      ([(lambda (e) (and (exn? e) (not (exn:break? e))))
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

(define-local-member-name
  get-optimization-coach-menu-item
  highlighting-shown?
  add-highlights
  clear-highlights
  show-optimization-coach-panel
  hide-optimization-coach-panel
  get-filters
  set-filters!
  optimization-coach-visible?
  build-optimization-coach-popup-menu)

(define optimization-coach-drracket-button
  (list
   "Optimization Coach"
   optimization-coach-bitmap
   optimization-coach-callback))

(define-unit tool@

  (import drracket:tool^)
  (export drracket:tool-exports^)

  (define (phase1) (void))
  (define (phase2) (void))

  (define highlights-mixin
    (mixin (text:basic<%> drracket:unit:definitions-text<%>) ()
      (inherit highlight-range unhighlight-range
               get-tab get-canvas get-pos/text
               position-line line-end-position
               begin-edit-sequence end-edit-sequence)

      (define highlights   '()) ; (listof `(,start ,end ,popup-fun))
      (define clear-thunks '()) ; list of thunks that clear highlights
      (define color-table #f)

      ;; filters : Listof (sub-report-entry -> Bool)
      ;; If any of these predicates return true for a given log entry's
      ;; sub, show it.
      ;; Note: at the point where these are called, report entries have
      ;; a single sub.
      (define filters (map cdr check-boxes)) ; all enabled by default
      (define/public (get-filters) filters)
      (define/public (set-filters! fs) (set! filters fs))

      ;; highlight-range, for ranges that span multiple lines, highlights
      ;; to the end of the first n-1 lines. Since the space at end of lines
      ;; does not have editor positions, I can't figure out how to make the
      ;; popup menu appear there (I can only make it appear in places that
      ;; have positions). To work around that, we highlight only the code
      ;; proper, not the space at the end of lines. That way, everywhere in
      ;; the highlight has a position, and can spawn popup menus.
      (define/private (highlight-entry l)
        (match-define (report-entry subs start end badness) l)
        (define color (if (= badness 0)
                          "lightgreen"
                          (vector-ref color-table badness)))
        (define (highlight-part start end)
          (highlight-range start end color #f 'high))
        ;; record highlight for popup menus
        (set! highlights (cons (list start end (popup-callback l))
                               highlights))
        (let loop ([start start])
          (define line        (position-line start))
          (define end-of-line (line-end-position line))
          (cond [(>= end-of-line end)
                 (list (highlight-part start end))] ; done
                [else
                 (cons (highlight-part start end-of-line)
                       (loop (add1 end-of-line)))])))

      (define on? #f)
      (define/public (highlighting-shown?) on?)

      (define report-cache #f)
      (define/public (add-highlights #:use-cache? [use-cache? #f])
        (clear-highlights)
        (send (get-tab) show-optimization-coach-panel)
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
        (begin-edit-sequence)
        (set! clear-thunks (for/fold ([res '()])
                               ([r (in-list report)])
                             (append (highlight-entry r) res)))
        (end-edit-sequence)
        (set! on? #t))

      (define/public (clear-highlights)
        (for ([h (in-list clear-thunks)]) (h))
        (set! highlights '())
        (send (get-tab) hide-optimization-coach-panel)
        (set! on? #f))

      (define/augment (on-insert start len)
        (clear-highlights))
      (define/augment (on-delete start len)
        (clear-highlights))

      (define/public (build-optimization-coach-popup-menu menu pos text)
        (and pos
             (is-a? text text%)
             ;; pos is in a highlight
             (for/fold ([new-item #f])
                 ([h (in-list highlights)])
               (match-define `(,start ,end ,popup-fun) h)
               (or new-item
                   (and (<= start pos end)
                        (new menu-item%
                             [label "Show Optimization Info"]
                             [parent menu]
                             [callback (lambda _
                                         (popup-fun text start end))]))))))

      (super-new)))

  (drracket:get/extend:extend-definitions-text highlights-mixin)

  (define toolbar-mixin
    (mixin (drracket:unit:tab<%>) ()
      (super-new)

      (inherit get-defs get-frame)

      (define visible? #f)
      (define/public (optimization-coach-visible?) visible?)

      (define panel #f)

      (define/public (show-optimization-coach-panel)
        (set! visible? #t)
        (define area-container (send (get-frame) get-area-container))
        (define definitions (get-defs))
        (define filters (send definitions get-filters))
        (cond [panel
               (send area-container add-child panel)]
              [else
               (set! panel (new horizontal-panel%
                                [parent area-container]
                                [stretchable-height #f]))
               (new button%
                    [label "Clear"]
                    [parent panel]
                    [callback (lambda _ (send (get-defs) clear-highlights))])
               (for ([(l f) (in-pairs check-boxes)])
                 (new check-box%
                      [label l]
                      [parent panel]
                      [callback
                       (lambda _
                         (define definitions (get-defs))
                         (define filters (send definitions get-filters))
                         (send definitions set-filters! (if (memq f filters)
                                                            (remq f filters)
                                                            (cons f filters)))
                         ;; redraw
                         (send definitions add-highlights #:use-cache? #t))]
                      [value (memq f filters)]))])
        ;; update check-boxes
        (for ([c (in-list (for/list ([c (in-list (send panel get-children))]
                                     #:when (is-a? c check-box%))
                            c))]
              [(l f) (in-pairs check-boxes)])
          (send c set-value (memq f filters))))

      (define/public (hide-optimization-coach-panel [close #t])
        (when visible?
          (send (send (get-frame) get-area-container) delete-child panel)
          (when close
            (set! visible? #f))))))

  (drracket:get/extend:extend-tab toolbar-mixin)

  (define tab-switch-mixin
    (mixin (drracket:unit:frame<%>) ()
      (inherit set-show-menu-sort-key get-current-tab
               get-definitions-text)

      (define/public (get-optimization-coach-menu-item)
        optimization-coach-menu-item)

      (define/override (add-show-menu-items show-menu)
        (super add-show-menu-items show-menu)
        (set! optimization-coach-menu-item
              (new menu-item%
                   [label (string-constant show-optimization-coach)]
                   [parent show-menu]
                   [demand-callback
                    (λ (item)
                      (send item set-label
                            (if (send (get-current-tab)
                                      optimization-coach-visible?)
                                (string-constant hide-optimization-coach)
                                (string-constant show-optimization-coach))))]
                   [callback
                    (λ (a b)
                      (define tab (get-current-tab))
                      (if (send tab optimization-coach-visible?)
                          (send (send tab get-defs) clear-highlights)
                          (optimization-coach-callback this)))]))
        (set-show-menu-sort-key optimization-coach-menu-item 403))

      (define/augment (on-tab-change old-tab new-tab)
        (send old-tab hide-optimization-coach-panel #f) ; don't close it
        (when (send new-tab optimization-coach-visible?)
          ;; if it was open before
          (send new-tab show-optimization-coach-panel)))

      (define optimization-coach-menu-item #f)


      ;; right-click menu
      (keymap:add-to-right-button-menu
       (let ([old (keymap:add-to-right-button-menu)])
         (lambda (menu editor event)
           (define definitions (get-definitions-text))
           (let-values ([(pos text) (send definitions get-pos/text event)])
             (send definitions build-optimization-coach-popup-menu
                   menu pos text))
           (old menu editor event))))

      (super-new)))

  (drracket:get/extend:extend-unit-frame tab-switch-mixin))
