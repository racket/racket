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

(define check-boxes
  `(("Report Typed Racket optimizations?" .
     ,(match-lambda [(sub-report-entry s m 'typed-racket) #t]
                    [_ #f]))
    ("Report inlining optimizations?" .
     ,(match-lambda [(sub-report-entry s m 'mzc) #t]
                    [_ #f]))))

(define (copy-definitions definitions)
  ;; this code is from Robby
  (define definitions-copy
    (new (class text:basic%
           ;; overriding get-port-name like this ensures
           ;; that the resulting syntax objects are connected
           ;; to the actual definitions-text, not this copy
           (define/override (get-port-name)
             (send definitions get-port-name))
           (super-new))))
  (send definitions-copy set-style-list
        (send definitions get-style-list)) ;; speeds up the copy
  (send definitions copy-self-to definitions-copy)
  definitions-copy)

(define-local-member-name
  get-optimization-coach-menu-item
  add-highlights
  clear-highlights
  show-optimization-coach
  hide-optimization-coach
  get-filters
  set-filters!
  optimization-coach-visible?
  build-optimization-coach-popup-menu
  launch-optimization-coach
  close-optimization-coach)

(define optimization-coach-drracket-button
  (list
   "Optimization Coach"
   optimization-coach-bitmap
   (lambda (drr-frame) (send drr-frame launch-optimization-coach))))

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
      (define/public (optimization-coach-visible?) on?)

      (define report-cache #f)
      ;; source is either a copy of the definitions text (we're not in the
      ;; main thread, so operating on the definitions directly is a bad idea)
      ;; or #f, in which case the report cache is used.
      (define/public (add-highlights #:source [source #f])
        (clear-highlights)
        (unless (and report-cache (not source))
          (set! report-cache (generate-report source)))
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
        (set! on? #f))

      (define (clear-and-close)
        (when on?
          (send+ this (get-tab) (get-frame) (close-optimization-coach))))
      (define/augment (on-insert start len)
        (clear-and-close))
      (define/augment (on-delete start len)
        (clear-and-close))

      (define/public (build-optimization-coach-popup-menu menu pos text)
        (and pos
             (is-a? text text%)
             ;; pos is in a highlight
             (for/fold ([new-item #f])
                 ([h (in-list highlights)])
               (match-define `(,start ,end ,popup-fun) h)
               (or new-item
                   (and (<= start pos end)
                        (new separator-menu-item% [parent menu])
                        (new menu-item%
                             [label "Show Optimization Info"]
                             [parent menu]
                             [callback (lambda _
                                         (popup-fun text start end))]))))))

      (super-new)))

  (drracket:get/extend:extend-definitions-text highlights-mixin)

  (define frame-mixin
    (mixin (drracket:unit:frame<%>) ()
      (inherit set-show-menu-sort-key get-current-tab
               get-definitions-text get-interactions-text get-area-container)


      ;; view menu
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
                            (if (send (get-definitions-text)
                                      optimization-coach-visible?)
                                (string-constant hide-optimization-coach)
                                (string-constant show-optimization-coach))))]
                   [callback
                    (λ (a b)
                      (define defs (get-definitions-text))
                      (if (send defs optimization-coach-visible?)
                          (close-optimization-coach)
                          (launch-optimization-coach)))]))
        (set-show-menu-sort-key optimization-coach-menu-item 403))
      (define optimization-coach-menu-item #f)


      ;; right-click menu
      (keymap:add-to-right-button-menu
       (let ([old (keymap:add-to-right-button-menu)])
         (lambda (menu editor event)
           (let-values ([(pos text) (send editor get-pos/text event)])
             (when (is-a? editor drracket:unit:definitions-text<%>)
               ;; has the optimization-coach mixin
               (send editor build-optimization-coach-popup-menu
                     menu pos text)))
           (old menu editor event))))


      ;; control panel
      (define panel #f)
      (define (create-panel)
        (set! panel (new horizontal-panel%
                         [parent (get-area-container)]
                         [stretchable-height #f]))
        (new button%
             [label "Clear"]
             [parent panel]
             [callback (lambda _ (close-optimization-coach))])
        (for ([(l f) (in-pairs check-boxes)])
          (new check-box%
               [label l]
               [parent panel]
               [callback
                (lambda _
                  (define definitions (get-definitions-text))
                  (define filters (send definitions get-filters))
                  (send definitions set-filters! (if (memq f filters)
                                                     (remq f filters)
                                                     (cons f filters)))
                  ;; redraw
                  (send definitions add-highlights))]
               [value #f]))) ; will be updated in `show-optimization-coach'

      (define/public (show-optimization-coach)
        (define area-container (get-area-container))
        (cond [panel (or (memq panel (send area-container get-children))
                         (send area-container add-child panel))]
              [else  (create-panel)])
        ;; update check-boxes
        (define filters (send (get-definitions-text) get-filters))
        (for ([c (in-list (for/list ([c (in-list (send panel get-children))]
                                     #:when (is-a? c check-box%))
                            c))]
              [(l f) (in-pairs check-boxes)])
          (send c set-value (memq f filters))))

      (define/public (hide-optimization-coach)
        (send (get-area-container) delete-child panel))


      ;; tab switching
      (define/augment (on-tab-change old-tab new-tab)
        (when (send (send old-tab get-defs) optimization-coach-visible?)
          (hide-optimization-coach))
        (when (send (send new-tab get-defs) optimization-coach-visible?)
          ;; if it was open before
          (show-optimization-coach)))


      ;; entry point
      (define/public (launch-optimization-coach)
        (define definitions  (get-definitions-text))
        (define interactions (get-interactions-text))
        ;; copy contents of the definitions window before handing control back
        ;; to the event loop
        (define definitions-copy (copy-definitions definitions))
        ;; launch OC proper
        (show-optimization-coach)
        (send this update-running #t)
        (thread ; do the work in a separate thread, to avoid blocking the GUI
           (lambda ()
             (with-handlers
                 ([(lambda (e) (and (exn? e) (not (exn:break? e))))
                   ;; typechecking failed, report in the interactions window
                   (lambda (e)
                     (close-optimization-coach)
                     (send interactions reset-console)
                     (send interactions run-in-evaluation-thread
                           (lambda () (raise e))))])
               (send (get-definitions-text) add-highlights
                     #:source definitions-copy))
             (send this update-running #f))))

      (define/public (close-optimization-coach)
        (hide-optimization-coach)
        (send (get-definitions-text) clear-highlights))

      (super-new)))

  (drracket:get/extend:extend-unit-frame frame-mixin))
