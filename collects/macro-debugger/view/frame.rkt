#lang racket/base
(require racket/class
         racket/path
         racket/gui/base
         framework
         unstable/class-iop
         "interfaces.rkt"
         "stepper.rkt"
         (prefix-in sb: "../syntax-browser/embed.rkt")
	 (prefix-in sb: "../syntax-browser/interfaces.rkt")
         unstable/gui/notify)
(provide macro-stepper-frame-mixin)

(define-syntax override/return-false
  (syntax-rules ()
    [(override/return-false m ...)
     (begin (define/override (m) #f) ...)]))

(define (macro-stepper-frame-mixin base-frame%)
  (class* base-frame% (stepper-frame<%>)
    (init-field config)
    (init-field director)
    (init-field (filename #f))

    (define obsoleted? #f)

    (inherit get-area-container
             get-size
             set-label
             get-menu%
             get-menu-item%
             get-menu-bar
             get-file-menu
             get-edit-menu
             get-help-menu)

    (super-new (label (make-label))
               (width (send/i config config<%> get-width))
               (height (send/i config config<%> get-height)))

    (define/private (make-label)
      (if filename
          (string-append (path->string
                          (file-name-from-path filename))
                         (if obsoleted? " (old)" "")
                         " - Macro stepper")
          "Macro stepper"))

    ;; Grrr... we get a spurious on-size event sometime after the
    ;; frame is created, probably when the window-manager gets around
    ;; to doing something. Avoid unnecessary updates.
    (define-values (w0 h0) (get-size))
    (define/override (on-size w h)
      (send/i config config<%> set-width w)
      (send/i config config<%> set-height h)
      (unless (and (= w0 w) (= h0 h))
        (when (send/i config config<%> get-refresh-on-resize?)
          (send/i widget widget<%> update/preserve-view)))
      (set!-values (w0 h0) (values w h)))

    (define warning-panel
      (new horizontal-panel%
           (parent (get-area-container))
           (stretchable-height #f)
           (style '(deleted))))

    (define/public (get-macro-stepper-widget%)
      macro-stepper-widget%)

    (define/i widget widget<%>
      (new (get-macro-stepper-widget%)
           (parent (get-area-container))
           (director director)
           (config config)))
    (define/i controller sb:controller<%>
      (send/i widget widget<%> get-controller))

    (define/public (get-widget) widget)
    (define/public (get-controller) controller)

    (define/public (add-obsoleted-warning)
      (unless obsoleted?
        (set! obsoleted? #t)
        (new warning-canvas%
             (warning
              (string-append
               "Warning: This macro stepper session is obsolete. "
               "The program may have changed."))
             (parent warning-panel))
        (set-label (make-label))
        (send (get-area-container) change-children
              (lambda (children)
                (cons warning-panel
                      (remq warning-panel children))))))

    ;; Set up menus

    (override/return-false file-menu:create-new?
                           file-menu:create-open?
                           file-menu:create-open-recent?
                           file-menu:create-revert?
                           file-menu:create-save?
                           file-menu:create-save-as?
                           ;file-menu:create-print?
                           edit-menu:create-undo?
                           edit-menu:create-redo?
                           ;edit-menu:create-cut?
                           ;edit-menu:create-paste?
                           edit-menu:create-clear?)

    (define stepper-menu
      (new (get-menu%) (parent (get-menu-bar)) (label "Stepper")))

    (define/override (file-menu:between-new-and-open file-menu)
      (new (get-menu-item%)
           (label "Duplicate stepper")
           (parent file-menu)
           (callback (lambda _ (send/i widget widget<%> duplicate-stepper))))
      (new (get-menu-item%)
           (label "Duplicate stepper (current term only)")
           (parent file-menu)
           (callback (lambda _ (send/i widget widget<%> show-in-new-frame)))))

    (menu-option/notify-box stepper-menu
                            "View syntax properties"
                            (get-field props-shown? config))

    (let ([id-menu
           (new (get-menu%)
                (label "Identifier=?")
                (parent stepper-menu))])
      (for-each (lambda (p)
                  (let ([this-choice
                         (new checkable-menu-item%
                              (label (car p)) 
                              (parent id-menu)
                              (callback
                               (lambda _ 
                                 (send/i controller sb:controller<%> set-identifier=? p))))])
                    (send/i controller sb:controller<%> listen-identifier=?
                           (lambda (name+func)
                             (send this-choice check
                                   (eq? (car name+func) (car p)))))))
                (sb:identifier=-choices)))

    (let ([identifier=? (send/i config config<%> get-identifier=?)])
      (when identifier=?
        (let ([p (assoc identifier=? (sb:identifier=-choices))])
          (send/i controller sb:controller<%> set-identifier=? p))))

    (new (get-menu-item%)
         (label "Clear selection")
         (parent stepper-menu)
         (callback
          (lambda _ (send/i controller sb:controller<%> 
			   set-selected-syntax #f))))

    (new separator-menu-item% (parent stepper-menu))

    (menu-option/notify-box stepper-menu
                            "Show macro hiding panel"
                            (get-field show-hiding-panel? config))

    (new (get-menu-item%)
         (label "Remove selected term")
         (parent stepper-menu)
         (callback (lambda _ (send/i widget widget<%> remove-current-term))))
    (new (get-menu-item%)
         (label "Reset mark numbering")
         (parent stepper-menu)
         (callback (lambda _ (send/i widget widget<%> reset-primary-partition))))
    (let ([extras-menu
           (new (get-menu%)
                (label "Extra options")
                (parent stepper-menu))])
      (new checkable-menu-item%
           (label "Always suffix marked identifiers")
           (parent extras-menu)
           (callback
            (lambda (i e)
              (send/i config config<%> set-suffix-option
                     (if (send i is-checked?)
                         'always
                         'over-limit))
              (send/i widget widget<%> update/preserve-view))))
      (menu-option/notify-box extras-menu
                              "Factor out common context?"
                              (get-field split-context? config))
      (menu-option/notify-box extras-menu
                              "Highlight redex/contractum"
                              (get-field highlight-foci? config))
      #|
      (menu-option/notify-box extras-menu
                              "Highlight frontier"
                              (get-field highlight-frontier? config))
      |#
      (menu-option/notify-box extras-menu
                              "Include renaming steps"
                              (get-field show-rename-steps? config))
      (menu-option/notify-box extras-menu
                              "One term at a time"
                              (get-field one-by-one? config))
      (menu-option/notify-box extras-menu
                              "Refresh on resize"
                              (get-field refresh-on-resize? config))
      (menu-option/notify-box extras-menu
                              "Close old stepper on Run"
                              (get-field close-on-reset-console? config))
      (menu-option/notify-box extras-menu
                              "Draw binding arrows"
                              (get-field draw-arrows? config))
      (menu-option/notify-box extras-menu
                              "Enable reader abbreviations"
                              (get-field pretty-abbrev? config))
      (menu-option/notify-box extras-menu
                              "Extra navigation"
                              (get-field extra-navigation? config)))

    ;; fixup-menu : menu -> void
    ;; Delete separators at beginning/end and duplicates in middle
    (define/private (fixup-menu menu)
      (define items
        (filter (lambda (i) (not (send i is-deleted?)))
                (send menu get-items)))
      (define (delete-seps-loop items)
        (if (and (pair? items) (is-a? (car items) separator-menu-item%))
            (begin (send (car items) delete)
                   (delete-seps-loop (cdr items)))
            items))
      (define (middle-loop items)
        (cond
         [(and (pair? items) (is-a? (car items) separator-menu-item%))
          (middle-loop (delete-seps-loop (cdr items)))]
         [(pair? items)
          (middle-loop (cdr items))]
         [else null]))
      (middle-loop (delete-seps-loop items))
      (delete-seps-loop (reverse items))
      (void))

    (for ([menu (send (get-menu-bar) get-items)])
      (fixup-menu menu))
    (frame:remove-empty-menus this)
    (frame:reorder-menus this)))

;; Stolen from stepper

(define warning-color "yellow")
(define warning-font normal-control-font)

(define warning-canvas%
  (class canvas%
    (init-field warning)
    (inherit get-dc get-client-size)
    (define/override (on-paint)
      (let ([dc (get-dc)])
        (send dc set-font warning-font) 
        (let-values ([(cw ch) (get-client-size)]
                     [(tw th dont-care dont-care2) 
                      (send dc get-text-extent warning)])
          (send dc set-pen 
                (send the-pen-list find-or-create-pen warning-color 1 'solid))
          (send dc set-brush
                (send the-brush-list find-or-create-brush warning-color 'solid))
          (send dc draw-rectangle 0 0 cw ch)
          (send dc draw-text 
                warning
                (- (/ cw 2) (/ tw 2))
                (- (/ ch 2) (/ th 2))))))
    (super-new)
    (inherit min-width min-height stretchable-height)
    (let-values ([(tw th dc dc2)
                  (send (get-dc) get-text-extent warning warning-font)])
      (min-width (+ 2 (inexact->exact (ceiling tw))))
      (min-height (+ 2 (inexact->exact (ceiling th)))))
    (stretchable-height #f)))
