#lang scheme/base
(require scheme/class
         (rename-in unstable/class-iop
                    [define/i define:]
                    [send/i send:]
                    [send*/i send*:]
                    [init-field/i init-field:])
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
         "step-display.ss"
	 (prefix-in sb: "../syntax-browser/interfaces.ss")
         "../model/deriv.ss"
         "../model/deriv-util.ss"
         "../model/trace.ss"
         "../model/reductions.ss"
         "../model/steps.ss"
         "cursor.ss"
         unstable/gui/notify
         (only-in mzscheme [#%top-interaction mz-top-interaction]))
(provide macro-stepper-widget%
         macro-stepper-widget/process-mixin)

;; Macro Stepper

;; macro-stepper-widget%
(define macro-stepper-widget%
  (class* object% (widget<%>)
    (init-field parent)
    (init-field config)
    (init-field: (director director<%>))

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

    ;; current-step-index : notify of number/#f
    (define-notify current-step-index (new notify-box% (value #f)))

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
            [invisible? (send: trec term-record<%> get-deriv-hidden?)])
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
          (let ([new-stepper (send: director director<%> new-stepper '(no-new-traces))])
            (send: new-stepper widget<%> add-deriv (send: term term-record<%> get-raw-deriv))
            (void)))))

    ;; duplicate-stepper : -> void
    (define/public (duplicate-stepper)
      (let ([new-stepper (send: director director<%> new-stepper)])
        (for ([term (cursor->list terms)])
          (send: new-stepper widget<%> add-deriv
                 (send: term term-record<%> get-raw-deriv)))))

    (define/public (get-config) config)
    (define/public (get-controller) sbc)
    (define/public (get-view) sbview)
    (define/public (get-step-displayer) step-displayer)
    (define/public (get-warnings-area) warnings-area)
    (define/public (get-macro-hiding-prefs) macro-hiding-prefs)

    (define/public (reset-primary-partition)
      (send: sbc sb:controller<%> reset-primary-partition)
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
    
    (define: sbview sb:syntax-browser<%>
      (new stepper-syntax-widget% 
           (parent area)
           (macro-stepper this)))
    (define: step-displayer step-display<%>
      (new step-display%
           (config config)
           (syntax-widget sbview)))
    (define: sbc sb:controller<%>
      (send: sbview sb:syntax-browser<%> get-controller))
    (define control-pane
      (new vertical-panel% (parent area) (stretchable-height #f)))
    (define: macro-hiding-prefs hiding-prefs<%>
      (new macro-hiding-prefs-widget%
           (parent control-pane)
           (stepper this)
           (config config)))

    (send: sbc sb:controller<%>
           listen-selected-syntax
           (lambda (stx) (send: macro-hiding-prefs hiding-prefs<%> set-syntax stx)))
    (send*: config config<%>
      (listen-show-hiding-panel?
       (lambda (show?) (show-macro-hiding-panel show?)))
      (listen-split-context?
       (lambda (_) (update/preserve-view)))
      (listen-highlight-foci?
       (lambda (_) (update/preserve-view)))
      (listen-highlight-frontier?
       (lambda (_) (update/preserve-view)))
      (listen-show-rename-steps?
       (lambda (_) (refresh/re-reduce)))
      (listen-one-by-one?
       (lambda (_) (refresh/re-reduce)))
      (listen-extra-navigation?
       (lambda (show?) (show-extra-navigation show?))))
    (send config listen-pretty-styles
          (lambda (_) (update/preserve-view)))

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

    (define nav:text
      (new text-field%
           (label "Step#")
           (init-value "00000")
           (parent extra-navigator)
           (stretchable-width #f)
           (stretchable-height #f)
           (callback
            (lambda (b e)
              (when (eq? (send e get-event-type) 'text-field-enter)
                (let* ([value (send b get-value)]
                       [step (string->number value)])
                  (cond [(exact-positive-integer? step)
                         (navigate-to (sub1 step))]
                        [(equal? value "end")
                         (navigate-to-end)])))))))
    (send nav:text set-value "")
    (listen-current-step-index
     (lambda (n)
       (send nav:text set-value
             (if (number? n) (number->string (add1 n)) ""))))

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

    (define/public (show-macro-hiding-panel show?)
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
#|
    (define/public-final (at-start?)
      (send: (focused-term) term-record<%> at-start?))
    (define/public-final (at-end?)
      (send: (focused-term) term-record<%> at-end?))
|#
    (define/public-final (navigate-to-start)
      (send: (focused-term) term-record<%> navigate-to-start)
      (update/save-position))
    (define/public-final (navigate-to-end)
      (send: (focused-term) term-record<%> navigate-to-end)
      (update/save-position))
    (define/public-final (navigate-previous)
      (send: (focused-term) term-record<%> navigate-previous)
      (update/save-position))
    (define/public-final (navigate-next)
      (send: (focused-term) term-record<%> navigate-next)
      (update/save-position))
    (define/public-final (navigate-to n)
      (send: (focused-term) term-record<%> navigate-to n)
      (update/save-position))

    (define/public-final (navigate-up)
      (when (focused-term)
        (send: (focused-term) term-record<%> on-lose-focus))
      (cursor:move-prev terms)
      (refresh/move))
    (define/public-final (navigate-down)
      (when (focused-term)
        (send: (focused-term) term-record<%> on-lose-focus))
      (cursor:move-next terms)
      (refresh/move))

    ;; Update

    ;; update/save-position : -> void
    (define/private (update/save-position)
      (update/preserve-lines-view))

    ;; update/preserve-lines-view : -> void
    (define/public (update/preserve-lines-view)
      (define text (send: sbview sb:syntax-browser<%> get-text))
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
      (define text (send: sbview sb:syntax-browser<%> get-text))
      (define start-box (box 0))
      (define end-box (box 0))
      (send text get-visible-position-range start-box end-box)
      (update)
      (send text scroll-to-position (unbox start-box) #f (unbox end-box) 'start))

    ;; update : -> void
    ;; Updates the terms in the syntax browser to the current step
    (define/private (update)
      (define text (send: sbview sb:syntax-browser<%> get-text))
      (define position-of-interest 0)
      (define multiple-terms? (> (length (cursor->list terms)) 1))
      (send text begin-edit-sequence #f)
      (send: sbview sb:syntax-browser<%> erase-all)

      (update:show-prefix)
      (when multiple-terms? (send: sbview sb:syntax-browser<%> add-separator))
      (set! position-of-interest (send text last-position))
      (update:show-current-step)
      (when multiple-terms? (send: sbview sb:syntax-browser<%> add-separator))
      (update:show-suffix)
      (send text end-edit-sequence)
      (send text scroll-to-position
            position-of-interest
            #f
            (send text last-position)
            'start)
      (update-nav-index)
      (enable/disable-buttons))

    ;; update:show-prefix : -> void
    (define/private (update:show-prefix)
      ;; Show the final terms from the cached synth'd derivs
      (for-each (lambda (trec) (send: trec term-record<%> display-final-term))
                (cursor:prefix->list terms)))

    ;; update:show-current-step : -> void
    (define/private (update:show-current-step)
      (when (focused-term)
        (send: (focused-term) term-record<%> display-step)))

    ;; update:show-suffix : -> void
    (define/private (update:show-suffix)
      (let ([suffix0 (cursor:suffix->list terms)])
        (when (pair? suffix0)
          (for-each (lambda (trec)
                      (send: trec term-record<%> display-initial-term))
                    (cdr suffix0)))))

    ;; update-nav-index : -> void
    (define/private (update-nav-index)
      (define term (focused-term))
      (set-current-step-index
       (and term (send: term term-record<%> get-step-index))))

    ;; enable/disable-buttons : -> void
    (define/private (enable/disable-buttons)
      (define term (focused-term))
      (send nav:start enable (and term (send: term term-record<%> has-prev?)))
      (send nav:previous enable (and term (send: term term-record<%> has-prev?)))
      (send nav:next enable (and term (send: term term-record<%> has-next?)))
      (send nav:end enable (and term (send: term term-record<%> has-next?)))
      (send nav:text enable (and term #t))
      (send nav:up enable (cursor:has-prev? terms))
      (send nav:down enable (cursor:has-next? terms)))

    ;; --

    ;; refresh/resynth : -> void
    ;; Macro hiding policy has changed; invalidate cached parts of trec
    (define/public (refresh/resynth)
      (for-each (lambda (trec) (send: trec term-record<%> invalidate-synth!))
                (cursor->list terms))
      (refresh))

    ;; refresh/re-reduce : -> void
    ;; Reduction config has changed; invalidate cached parts of trec
    (define/private (refresh/re-reduce)
      (for-each (lambda (trec) (send: trec term-record<%> invalidate-steps!))
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
        (send: (focused-term) term-record<%> on-get-focus))
      (update))

    (define/private (foci x) (if (list? x) x (list x)))

    ;; Hiding policy
    
    (define/public (get-show-macro?)
      (send: macro-hiding-prefs hiding-prefs<%> get-policy))

    ;; Derivation pre-processing

    (define/public (get-preprocess-deriv) (lambda (d) d))

    ;; Initialization

    (super-new)
    (show-macro-hiding-panel (send: config config<%> get-show-hiding-panel?))
    (show-extra-navigation (send: config config<%> get-extra-navigation?))
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
         (let ([first (adjust-deriv/lift first)])
           (and first
                (let ([e1 (wderiv-e1 first)])
                  (make-lift-deriv e1 e2 first lifted-stx second))))]
        [(Wrap ecte (e1 e2 first second))
         (let ([first (adjust-deriv/lift first)])
           (and first
                (let ([e1 (wderiv-e1 first)])
                  (make ecte e1 e2 first second))))]
        [else (adjust-deriv/top deriv)]))

    ;; adjust-deriv/top : Derivation -> Derivation
    (define/private (adjust-deriv/top deriv)
      (if (or (and #| (syntax-source (wderiv-e1 deriv)) |#
                   (syntax-original? (wderiv-e1 deriv)))
              (p:module? deriv))
          deriv
          ;; It's not original...
          ;; Strip out mzscheme's top-interactions
          ;; Keep anything that is a non-mzscheme top-interaction
          (cond [(for/or ([x (base-resolves deriv)]) (top-interaction-kw? x))
                 ;; Just mzscheme's top-interaction; strip it out
                 (adjust-deriv/top (mrule-next deriv))]
                [else deriv])))

    (define/public (top-interaction-kw? x)
      (or (free-identifier=? x #'#%top-interaction)
          (free-identifier=? x #'mz-top-interaction)))

    ))
