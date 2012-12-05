#lang racket/base
(require racket/class
         racket/gui/base
         racket/match
         framework
         syntax/id-table
         unstable/class-iop
         "interfaces.rkt"
         "controller.rkt"
         "display.rkt"
         "keymap.rkt"
         "hrule-snip.rkt"
         "properties.rkt"
         "text.rkt"
         "util.rkt"
         "../util/eomap.rkt"
         "../util/logger.rkt"
         "../util/mpi.rkt")
(provide widget%)

;; widget%
;; A syntax widget creates its own syntax-controller.
(define widget%
  (class* object% (syntax-browser<%> widget-hooks<%>)
    (init parent)
    (init-field config)

    (field [controller (new controller%)])

    (define -main-panel
      (new vertical-panel% (parent parent)))
    (define -split-panel
      (new panel:horizontal-dragable% (parent -main-panel)))
    (define -text (new browser-text%))
    (define -ecanvas
      (new canvas:color% (parent -split-panel) (editor -text)))
    (define -props-panel
      (new horizontal-panel% (parent -split-panel) (style '(deleted))))
    (define props
      (new properties-view%
           (parent -props-panel)
           (controller controller)))

    (define/public (setup-keymap)
      (new syntax-keymap% 
           (editor -text)
           (controller controller)
           (config config)))

    (send -text set-styles-sticky #f)
    (send -text lock #t)

    (define/public (show-props show?)
      (internal-show-props show?))

    (define saved-props-percentage #f)

    (define/private (internal-show-props show?)
      (if show?
          (unless (send -props-panel is-shown?)
            (send -split-panel begin-container-sequence)
            (let ([p (or saved-props-percentage
                         (send/i config config<%> get-props-percentage))])
              (send -split-panel add-child -props-panel)
              (update-props-percentage p))
            (send -props-panel show #t)
            (send -split-panel end-container-sequence))
          (when (send -props-panel is-shown?)
            (send -split-panel begin-container-sequence)
            (set! saved-props-percentage
                  (cadr (send -split-panel get-percentages)))
            (send -split-panel delete-child -props-panel)
            (send -props-panel show #f)
            (send -split-panel end-container-sequence))))

    (define/private (update-props-percentage p)
      (send -split-panel set-percentages
            (list (- 1 p) p)))

    (define/private (props-panel-shown?)
      (send -props-panel is-shown?))

    ;;

    (define/public (get-controller)
      controller)

    ;;

    (define/public (get-main-panel)
      -main-panel)

    (define/public (shutdown)
      (when (props-panel-shown?)
        (send/i config config<%> set-props-percentage
               (cadr (send -split-panel get-percentages)))))

    ;; syntax-browser<%> Methods

    (define/public (add-text text)
      (with-unlock -text
        (send -text insert text)))

    (define/public (add-error-text text)
      (with-unlock -text
        (let ([a (send -text last-position)])
          (send -text insert text)
          (let ([b (send -text last-position)])
            (send -text change-style error-text-style a b)))))

    (define/public (add-clickback text handler)
      (with-unlock -text
        (let ([a (send -text last-position)])
          (send -text insert text)
          (let ([b (send -text last-position)])
            (send -text set-clickback a b handler)
            (send -text change-style clickback-style a b)))))

    (define/public (add-syntax stx
                               #:binders [binders '#hash()]
                               #:shift-table [shift-table '#hash()]
                               #:definites [definites #f]
                               #:hi-colors [hi-colors null]
                               #:hi-stxss [hi-stxss null]
                               #:substitutions [substitutions null])
      (define (get-shifted id) (hash-ref shift-table id null))

      (with-unlock -text
        (define display
          (print-syntax-to-editor stx -text controller config
                                  (calculate-columns)
                                  (send -text last-position)))
        (send -text insert "\n")
        (define range (send/i display display<%> get-range))
        (define offset (send/i display display<%> get-start-position))
        (with-log-time "substitutions"
        (for ([subst (in-list substitutions)])
          (for ([r (in-list (send/i range range<%> get-ranges (car subst)))])
            (send -text insert (cdr subst)
                  (+ offset (car r))
                  (+ offset (cdr r))
                  #f)
            (send -text change-style
                  (code-style -text (send/i config config<%> get-syntax-font-size))
                  (+ offset (car r))
                  (+ offset (cdr r))
                  #f))))
        ;; Apply highlighting
        (with-log-time "highlights"
        (for ([hi-stxs (in-list hi-stxss)] [hi-color (in-list hi-colors)])
          (send/i display display<%> highlight-syntaxes hi-stxs hi-color)))
        ;; Underline binders (and shifted binders)
        (with-log-time "underline binders"
        (send/i display display<%> underline-syntaxes
                (let ([binder-list (hash-map binders (lambda (k v) k))])
                  (append (apply append (map get-shifted binder-list))
                          binder-list))))
        (send display refresh)

        ;; Make arrows (& billboards, when enabled)
        (with-log-time "add arrows"
        (when (send config get-draw-arrows?)
          (define (definite-phase id)
            (and definites
                 (or (eomap-ref definites id #f)
                     (for/or ([shifted (in-list (hash-ref shift-table id null))])
                       (eomap-ref definites shifted #f)))))

          (define phase-binder-table (make-hash))
          (define (get-binder-table phase)
            (hash-ref! phase-binder-table phase (lambda () (make-free-id-table #:phase phase))))
          (for ([(binder phase) (in-hash binders)])
            (free-id-table-set! (get-binder-table phase) binder binder))

          (define (get-binders id phase)
            (define (for-one-table table id)
              (let ([binder (free-id-table-ref table id #f)])
                (cond [(not binder) null]
                      [shift-table (cons binder (get-shifted binder))]
                      [else (list binder)])))
            (cond [phase (for-one-table (get-binder-table phase) id)]
                  [else
                   (apply append
                          (for/list ([table (in-hash-values phase-binder-table)])
                            (for-one-table table id)))]))

          (for ([id (in-list (send/i range range<%> get-identifier-list))])
            (define phase (definite-phase id))
            (when #f ;; DISABLED
              (add-binding-billboard offset range id phase))
            (for ([binder (in-list (get-binders id phase))])
              (for ([binder-r (in-list (send/i range range<%> get-ranges binder))])
                (for ([id-r (in-list (send/i range range<%> get-ranges id))])
                  (add-binding-arrow offset binder-r id-r phase)))))))
        (void)))

    (define/private (add-binding-arrow start binder-r id-r phase)
      ;; phase = #f means not definite binding (ie, "?" arrow)
      (send -text add-arrow
            (+ start (car binder-r))
            (+ start (cdr binder-r))
            (+ start (car id-r))
            (+ start (cdr id-r))
            (if phase "blue" "purple")
            (cond [(equal? phase 0) #f]
                  [phase (format "phase ~s" phase)]
                  [else "?"])
            (if phase 'end 'start)))

    (define/private (add-binding-billboard start range id definite?)
      (match (identifier-binding id)
        [(list-rest src-mod src-name nom-mod nom-name _)
         (for ([id-r (in-list (send/i range range<%> get-ranges id))])
           (send -text add-billboard
                 (+ start (car id-r))
                 (+ start (cdr id-r))
                 (string-append "from " (mpi->string src-mod))
                 (if definite? "blue" "purple")))]
        [_ (void)]))

    (define/public (add-separator)
      (with-unlock -text
        (send* -text
          (insert (new hrule-snip%))
          (insert "\n"))))

    (define/public (erase-all)
      (with-unlock -text
        (send -text erase))
      (send/i controller displays-manager<%> remove-all-syntax-displays))

    (define/public (get-text) -text)

    (define/private (calculate-columns)
      (define style (code-style -text (send/i config config<%> get-syntax-font-size)))
      (define char-width (send style get-text-width (send -ecanvas get-dc)))
      #|
      (define-values (canvas-w canvas-h) (send -ecanvas get-client-size))
      (sub1 (inexact->exact (floor (/ canvas-w char-width))))
      |#
      (let ([admin (send -text get-admin)]
            [w-box (box 0.0)])
        (send admin get-view #f #f w-box #f)
        (sub1 (inexact->exact (floor (/ (unbox w-box) char-width))))))

    ;; Initialize
    (super-new)
    (setup-keymap)

    (send/i config config<%> listen-props-shown?
           (lambda (show?)
             (show-props show?)))
    (send/i config config<%> listen-props-percentage
           (lambda (p)
             (update-props-percentage p)))
    (internal-show-props (send/i config config<%> get-props-shown?))))


(define clickback-style
  (let ([sd (new style-delta%)])
    (send sd set-delta 'change-toggle-underline)
    (send sd set-delta-foreground "blue")
    sd))

(define error-text-style
  (let ([sd (new style-delta%)])
    (send sd set-delta 'change-italic)
    (send sd set-delta-foreground "red")
    sd))
