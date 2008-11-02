
#lang scheme/base
(require scheme/class
         scheme/unit
         scheme/list
         scheme/match
         scheme/gui
         framework/framework
         syntax/boundmap
         "interfaces.ss"
         "prefs.ss"
         "extensions.ss"
         "warning.ss"
         "hiding-panel.ss"
         "term-record.ss"
         (prefix-in s: "../syntax-browser/widget.ss")
         (prefix-in s: "../syntax-browser/params.ss")
         "../model/deriv.ss"
         "../model/deriv-util.ss"
         "../model/deriv-find.ss"
         "../model/trace.ss"
         "../model/reductions.ss"
         "../model/steps.ss"
         "cursor.ss"
         "../util/notify.ss")
(provide macro-stepper-widget%
         macro-stepper-widget/process-mixin)

;; Macro Stepper

;; macro-stepper-widget%
(define macro-stepper-widget%
  (class* object% ()
    (init-field parent)
    (init-field config)
    (init-field director)

    ;; Terms

    ;; all-terms : (list-of TermRecord)
    ;; (Reversed)
    (define all-terms null)

    ;; terms : (Cursor-of TermRecord)
    ;; Contains visible terms of all-terms
    (define terms (cursor:new null))

    ;; focused-term : -> TermRecord or #f
    (define (focused-term)
      (cursor:next terms))

    ;; add-deriv : Deriv -> void
    (define/public (add-deriv d)
      (let ([trec (new term-record% (stepper this) (raw-deriv d))])
        (add trec)))

    ;; add-trace : (list-of event) -> void
    (define/public (add-trace events)
      (let ([trec (new term-record% (stepper this) (events events))])
        (add trec)))

    ;; add : TermRecord -> void
    (define/public (add trec)
      (set! all-terms (cons trec all-terms))
      (let ([display-new-term? (cursor:at-end? terms)]
            [invisible? (send trec get-deriv-hidden?)])
        (unless invisible?
          (cursor:add-to-end! terms (list trec))
          (trim-navigator)
          (if display-new-term?
              (refresh)
              (update)))))

    ;; remove-current-term : -> void
    (define/public (remove-current-term)
      (cursor:remove-current! terms)
      (trim-navigator)
      (refresh))

    ;; show-in-new-frame : -> void
    (define/public (show-in-new-frame)
      (let ([term (focused-term)])
        (when term
          (let ([new-stepper (send director new-stepper '(no-new-traces))])
            (send new-stepper add-deriv (send term get-raw-deriv))
            (void)))))

    ;; duplicate-stepper : -> void
    (define/public (duplicate-stepper)
      (let ([new-stepper (send director new-stepper)])
        (for ([term (cursor->list terms)])
          (send new-stepper add-deriv (send term get-raw-deriv)))))

    (define/public (get-config) config)
    (define/public (get-controller) sbc)
    (define/public (get-view) sbview)
    (define/public (get-warnings-area) warnings-area)
    (define/public (get-macro-hiding-prefs) macro-hiding-prefs)

    (define/public (reset-primary-partition)
      (send sbc reset-primary-partition)
      (update/preserve-view))

    (define area (new vertical-panel% (parent parent)))
    (define supernavigator
      (new horizontal-panel%
           (parent area)
           (stretchable-height #f)
           (alignment '(center center))))
    (define navigator
      (new horizontal-panel%
           (parent supernavigator)
           (stretchable-width #f)
           (stretchable-height #f)
           (alignment '(left center))))
    (define extra-navigator
      (new horizontal-panel%
           (parent supernavigator)
           (stretchable-width #f)
           (stretchable-height #f)
           (alignment '(left center))
           (style '(deleted))))
    
    (define warnings-area (new stepper-warnings% (parent area)))
    
    (define sbview (new stepper-syntax-widget% 
                        (parent area)
                        (macro-stepper this)))
    (define sbc (send sbview get-controller))
    (define control-pane
      (new vertical-panel% (parent area) (stretchable-height #f)))
    (define macro-hiding-prefs
      (new macro-hiding-prefs-widget%
           (parent control-pane)
           (stepper this)
           (config config)))

    (send config listen-show-syntax-properties?
          (lambda (show?) (send sbview show-props show?)))
    (send config listen-show-hiding-panel?
          (lambda (show?) (show-macro-hiding-prefs show?)))
    (send sbc listen-selected-syntax
          (lambda (stx) (send macro-hiding-prefs set-syntax stx)))
    (send config listen-highlight-foci?
          (lambda (_) (update/preserve-view)))
    (send config listen-highlight-frontier?
          (lambda (_) (update/preserve-view)))
    (send config listen-show-rename-steps?
          (lambda (_) (refresh/re-reduce)))
    (send config listen-one-by-one?
          (lambda (_) (refresh/re-reduce)))
    (send config listen-force-letrec-transformation?
          (lambda (_) (refresh/resynth)))
    (send config listen-extra-navigation?
          (lambda (show?) (show-extra-navigation show?)))

    (define nav:up
      (new button% (label "Previous term") (parent navigator)
           (callback (lambda (b e) (navigate-up)))))
    (define nav:start
      (new button% (label "<-- Start") (parent navigator)
           (callback (lambda (b e) (navigate-to-start)))))
    (define nav:previous
      (new button% (label "<- Step") (parent navigator)
           (callback (lambda (b e) (navigate-previous)))))
    (define nav:next
      (new button% (label "Step ->") (parent navigator)
           (callback (lambda (b e) (navigate-next)))))
    (define nav:end
      (new button% (label "End -->") (parent navigator)
           (callback (lambda (b e) (navigate-to-end)))))
    (define nav:down
      (new button% (label "Next term") (parent navigator)
           (callback (lambda (b e) (navigate-down)))))

    (define/private (trim-navigator)
      (if (> (length (cursor->list terms)) 1)
          (send navigator change-children
                (lambda _
                  (list nav:up
                        nav:start
                        nav:previous
                        nav:next
                        nav:end
                        nav:down)))
          (send navigator change-children
                (lambda _
                  (list nav:start
                        nav:previous
                        nav:next
                        nav:end)))))

    (define/public (show-macro-hiding-prefs show?)
      (send area change-children
            (lambda (children)
              (if show?
                  (append (remq control-pane children) (list control-pane))
                  (remq control-pane children)))))

    (define/private (show-extra-navigation show?)
      (send supernavigator change-children
            (lambda (children)
              (if show?
                  (list navigator extra-navigator)
                  (list navigator)))))

    ;; Navigation

    (define/public-final (at-start?)
      (send (focused-term) at-start?))
    (define/public-final (at-end?)
      (send (focused-term) at-end?))

    (define/public-final (navigate-to-start)
      (send (focused-term) navigate-to-start)
      (update/save-position))
    (define/public-final (navigate-to-end)
      (send (focused-term) navigate-to-end)
      (update/save-position))
    (define/public-final (navigate-previous)
      (send (focused-term) navigate-previous)
      (update/save-position))
    (define/public-final (navigate-next)
      (send (focused-term) navigate-next)
      (update/save-position))

    (define/public-final (navigate-up)
      (when (focused-term)
        (send (focused-term) on-lose-focus))
      (cursor:move-prev terms)
      (refresh/move))
    (define/public-final (navigate-down)
      (when (focused-term)
        (send (focused-term) on-lose-focus))
      (cursor:move-next terms)
      (refresh/move))

    ;; Update

    ;; update/save-position : -> void
    (define/private (update/save-position)
      (update/preserve-lines-view))

    ;; update/preserve-lines-view : -> void
    (define/public (update/preserve-lines-view)
      (define text (send sbview get-text))
      (define start-box (box 0))
      (define end-box (box 0))
      (send text get-visible-line-range start-box end-box)
      (update)
      (send text scroll-to-position
            (send text line-start-position (unbox start-box))
            #f
            (send text line-start-position (unbox end-box))
            'start))
    
    ;; update/preserve-view : -> void
    (define/public (update/preserve-view)
      (define text (send sbview get-text))
      (define start-box (box 0))
      (define end-box (box 0))
      (send text get-visible-position-range start-box end-box)
      (update)
      (send text scroll-to-position (unbox start-box) #f (unbox end-box) 'start))

    ;; update : -> void
    ;; Updates the terms in the syntax browser to the current step
    (define/private (update)
      (define text (send sbview get-text))
      (define position-of-interest 0)
      (define multiple-terms? (> (length (cursor->list terms)) 1))
      (send text begin-edit-sequence)
      (send sbview erase-all)
      
      (update:show-prefix)
      (when multiple-terms? (send sbview add-separator))
      (set! position-of-interest (send text last-position))
      (update:show-current-step)
      (when multiple-terms? (send sbview add-separator))
      (update:show-suffix)
      (send text end-edit-sequence)
      (send text scroll-to-position
            position-of-interest
            #f
            (send text last-position)
            'start)
      (enable/disable-buttons))

    ;; update:show-prefix : -> void
    (define/private (update:show-prefix)
      ;; Show the final terms from the cached synth'd derivs
      (for-each (lambda (trec) (send trec display-final-term))
                (cursor:prefix->list terms)))

    ;; update:show-current-step : -> void
    (define/private (update:show-current-step)
      (when (focused-term)
        (send (focused-term) display-step)))

    ;; update:show-suffix : -> void
    (define/private (update:show-suffix)
      (let ([suffix0 (cursor:suffix->list terms)])
        (when (pair? suffix0)
          (for-each (lambda (trec)
                      (send trec display-initial-term))
                    (cdr suffix0)))))

    ;; enable/disable-buttons : -> void
    (define/private (enable/disable-buttons)
      (define term (focused-term))
      (send nav:start enable (and term (send term has-prev?)))
      (send nav:previous enable (and term (send term has-prev?)))
      (send nav:next enable (and term (send term has-next?)))
      (send nav:end enable (and term (send term has-next?)))
      (send nav:up enable (cursor:has-prev? terms))
      (send nav:down enable (cursor:has-next? terms)))

    ;; --

    ;; refresh/resynth : -> void
    ;; Macro hiding policy has changed; invalidate cached parts of trec
    (define/public (refresh/resynth)
      (for-each (lambda (trec) (send trec invalidate-synth!))
                (cursor->list terms))
      (refresh))

    ;; refresh/re-reduce : -> void
    ;; Reduction config has changed; invalidate cached parts of trec
    (define/private (refresh/re-reduce)
      (for-each (lambda (trec) (send trec invalidate-steps!))
                (cursor->list terms))
      (refresh))

    ;; refresh/move : -> void
    ;; Moving between terms; clear the saved position
    (define/private (refresh/move)
      (refresh))

    ;; refresh : -> void
    (define/public (refresh)
      (send warnings-area clear)
      (when (focused-term)
        (send (focused-term) on-get-focus))
      (update))

    ;; delayed-recache-errors : (list-of (cons exn string))
    (define delayed-recache-errors null)

    ;; handle-recache-error : exception string -> void
    (define/private (handle-recache-error exn part)
      (if (send config get-debug-catch-errors?)
          (begin
            (set! delayed-recache-errors 
                  (cons (cons exn part) delayed-recache-errors))
            (queue-callback
             (lambda ()
               (when (pair? delayed-recache-errors)
                 (message-box
                  "Error"
                  (string-append 
                   "Internal errors in macro stepper:\n"
                   (if (memq 'macro-hiding (map cdr delayed-recache-errors))
                       (string-append 
                        "Macro hiding failed on one or more terms. "
                        "The macro stepper is showing the terms "
                        "with macro hiding disabled.\n")
                       "")
                   (if (memq 'reductions (map cdr delayed-recache-errors))
                       (string-append
                        "The macro stepper failed to compute the reduction sequence "
                        "for one or more terms.\n")
                       "")))
                 (set! delayed-recache-errors null)))))
          (raise exn)))

    (define/private (foci x) (if (list? x) x (list x)))

    ;; Hiding policy
    
    (define/public (get-show-macro?)
      (send macro-hiding-prefs get-policy))

    ;; Derivation pre-processing

    (define/public (get-preprocess-deriv) (lambda (d) d))

    ;; Initialization

    (super-new)
    (send sbview show-props (send config get-show-syntax-properties?))
    (show-macro-hiding-prefs (send config get-show-hiding-panel?))
    (show-extra-navigation (send config get-extra-navigation?))
    (refresh/move)
    ))

(define (macro-stepper-widget/process-mixin %)
  (class %
    (super-new)
    (define/override (get-preprocess-deriv)
      (lambda (d) (get-original-part d)))

    ;; get-original-part : Deriv -> Deriv/#f
    ;; Strip off mzscheme's #%top-interaction
    ;; Careful: the #%top-interaction node may be inside of a lift-deriv
    (define/private (get-original-part deriv)
      (let ([deriv* (adjust-deriv/lift deriv)])
        deriv*))

    ;; adjust-deriv/lift : Derivation -> (list-of Derivation)
    (define/private (adjust-deriv/lift deriv)
      (match deriv
        [(Wrap lift-deriv (e1 e2 first lifted-stx second))
         (let ([first (adjust-deriv/top first)])
           (and first
                (let ([e1 (wderiv-e1 first)])
                  (make-lift-deriv e1 e2 first lifted-stx second))))]
        [else (adjust-deriv/top deriv)]))

    ;; adjust-deriv/top : Derivation -> Derivation
    (define/private (adjust-deriv/top deriv)
      (if (or (syntax-source (wderiv-e1 deriv))
              (p:module? deriv))
          deriv
          ;; It's not original...
          ;; Strip out mzscheme's top-interactions
          ;; Keep anything that is a non-mzscheme top-interaction
          ;; Drop everything else (not original program)
          (cond [(not (mrule? deriv)) #f]
                [(for/or ([x (base-resolves deriv)]) (top-interaction-kw? x))
                 ;; Just mzscheme's top-interaction; strip it out
                 (adjust-deriv/top (mrule-next deriv))]
                [(equal? (map syntax-e (base-resolves deriv))
                         '(#%top-interaction))
                 ;; A *different* top interaction; keep it
                 deriv]
                [else
                 ;; Not original and not tagged with top-interaction
                 #f])))

    (define/public (top-interaction-kw? x)
      (free-identifier=? x #'#%top-interaction))

    ))
