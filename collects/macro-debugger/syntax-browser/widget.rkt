#lang racket/base
(require racket/class
         racket/gui
         racket/list
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
    (define -props-panel (new horizontal-panel% (parent -split-panel)))
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

    (define/private (internal-show-props show?)
      (if show?
          (unless (send -props-panel is-shown?)
            (let ([p (send/i config config<%> get-props-percentage)])
              (send -split-panel add-child -props-panel)
              (update-props-percentage p))
            (send -props-panel show #t))
          (when (send -props-panel is-shown?)
            (send -split-panel delete-child -props-panel)
            (send -props-panel show #f))))

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
                               #:binders [binders null]
                               #:shift-table [shift-table #f]
                               #:definites [definites null]
                               #:hi-colors [hi-colors null]
                               #:hi-stxss [hi-stxss null]
                               #:substitutions [substitutions null])
      (let ([display (internal-add-syntax stx)]
            [definite-table (make-hasheq)])
        (let ([range (send/i display display<%> get-range)]
              [offset (send/i display display<%> get-start-position)])
          (for ([subst substitutions])
            (for ([r (send/i range range<%> get-ranges (car subst))])
              (with-unlock -text
                (send -text insert (cdr subst)
                      (+ offset (car r))
                      (+ offset (cdr r))
                      #f)
                (send -text change-style
                      (code-style -text (send/i config config<%> get-syntax-font-size))
                      (+ offset (car r))
                      (+ offset (cdr r)))))))
        (for ([hi-stxs hi-stxss] [hi-color hi-colors])
          (send/i display display<%> highlight-syntaxes hi-stxs hi-color))
        (for ([definite definites])
          (hash-set! definite-table definite #t)
          (when shift-table
            (for ([shifted-definite (hash-ref shift-table definite null)])
              (hash-set! definite-table shifted-definite #t))))
        (let ([binder-table (make-free-id-table)])
          (define range (send/i display display<%> get-range))
          (define start (send/i display display<%> get-start-position))
          (define (get-binders id)
            (let ([binder (free-id-table-ref binder-table id #f)])
              (cond [(not binder) null]
                    [shift-table (cons binder (get-shifted binder))]
                    [else (list binder)])))
          (define (get-shifted id)
            (hash-ref shift-table id null))
          ;; Populate table
          (for ([binder binders])
            (free-id-table-set! binder-table binder binder))
          ;; Underline binders (and shifted binders)
          (send/i display display<%> underline-syntaxes
                 (append (apply append (map get-shifted binders))
                         binders))
          ;; Make arrows (& billboards, when enabled)
          (for ([id (send/i range range<%> get-identifier-list)])
            (define definite? (hash-ref definite-table id #f))
            (when #f ;; DISABLED
              (add-binding-billboard start range id definite?))
            (for ([binder (get-binders id)])
              (for ([binder-r (send/i range range<%> get-ranges binder)])
                (for ([id-r (send/i range range<%> get-ranges id)])
                  (add-binding-arrow start binder-r id-r definite?))))))
        (void)))

    (define/private (add-binding-arrow start binder-r id-r definite?)
      (if definite?
          (send -text add-arrow
                (+ start (car binder-r))
                (+ start (cdr binder-r))
                (+ start (car id-r))
                (+ start (cdr id-r))
                "blue")
          (send -text add-question-arrow
                (+ start (car binder-r))
                (+ start (cdr binder-r))
                (+ start (car id-r))
                (+ start (cdr id-r))
                "purple")))

    (define/private (add-binding-billboard start range id definite?)
      (match (identifier-binding id)
        [(list-rest src-mod src-name nom-mod nom-name _)
         (for-each (lambda (id-r)
                     (send -text add-billboard
                           (+ start (car id-r))
                           (+ start (cdr id-r))
                           (string-append "from " (mpi->string src-mod))
                           (if definite? "blue" "purple")))
                   (send/i range range<%> get-ranges id))]
        [_ (void)]))

    (define/public (add-separator)
      (with-unlock -text
        (send* -text
          (insert (new hrule-snip%))
          (insert "\n"))))

    (define/public (erase-all)
      (with-unlock -text
        (send -text erase)
        (send -text delete-all-drawings))
      (send/i controller displays-manager<%> remove-all-syntax-displays))

    (define/public (get-text) -text)

    ;; internal-add-syntax : syntax -> display
    (define/private (internal-add-syntax stx)
      (with-unlock -text
        (let ([display
               (print-syntax-to-editor stx -text controller config
                                       (calculate-columns)
                                       (send -text last-position))])
          (send* -text
            (insert "\n")
            ;;(scroll-to-position current-position)
            )
          display)))

    (define/private (calculate-columns)
      (define style (code-style -text (send/i config config<%> get-syntax-font-size)))
      (define char-width (send style get-text-width (send -ecanvas get-dc)))
      (define-values (canvas-w canvas-h) (send -ecanvas get-client-size))
      (sub1 (inexact->exact (floor (/ canvas-w char-width)))))

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

;; Specialized classes for widget

(define browser-text%
  (let ([browser-text-default-style-name "widget.rkt::browser-text% basic"])
    (class (text:arrows-mixin
            (text:tacking-mixin
             (text:hover-drawings-mixin
              (text:hover-mixin
               (text:hide-caret/selection-mixin
                (text:foreground-color-mixin
                 (editor:standard-style-list-mixin text:basic%)))))))
      (inherit set-autowrap-bitmap get-style-list)
      (define/override (default-style-name) browser-text-default-style-name)
      (super-new (auto-wrap #t))
      (let* ([sl (get-style-list)]
             [standard (send sl find-named-style (editor:get-default-color-style-name))]
             [browser-basic (send sl find-or-create-style standard
                                  (make-object style-delta% 'change-family 'default))])
        (send sl new-named-style browser-text-default-style-name browser-basic))
      (set-autowrap-bitmap #f))))
