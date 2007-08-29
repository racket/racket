
(module widget mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "kw.ss")
           (lib "boundmap.ss" "syntax")
           "interfaces.ss"
           "params.ss"
           "controller.ss"
           "display.ss"
           "keymap.ss"
           "hrule-snip.ss"
           "properties.ss"
           "text.ss"
           "util.ss")
  (provide widget%
           widget-keymap%
           widget-context-menu%)

  ;; widget%
  ;; A syntax widget creates its own syntax-controller.
  (define widget%
    (class* object% (widget-hooks<%>)
      (init parent)
      (init-field config)

      (define controller (new controller%))

      (define -main-panel
        (new vertical-panel% (parent parent)))
      (define -split-panel
        (new panel:horizontal-dragable% (parent -main-panel)))
      (define -text (new browser-text%))
      (define -ecanvas
        (new editor-canvas% (parent -split-panel) (editor -text)))
      (define -props-panel (new horizontal-panel% (parent -split-panel)))
      (define props
        (new properties-view%
             (parent -props-panel)
             (controller controller)))
      (define props-percentage (send config pref:props-percentage))

      (define/public (setup-keymap)
        (new widget-keymap% 
             (editor -text)
             (widget this)))

      (send -text lock #t)
      (send -split-panel set-percentages 
            (list (- 1 props-percentage) props-percentage))

      ;; syntax-properties-controller<%> methods

      (define/public (set-syntax stx)
        (send props set-syntax stx))

      (define/public (props-shown?)
        (send -props-panel is-shown?))

      (define/public (toggle-props)
        (show-props (not (send -props-panel is-shown?))))

      (define/public (show-props show?)
        (if show?
            (unless (send -props-panel is-shown?)
              (send -split-panel add-child -props-panel)
              (send -split-panel set-percentages
                    (list (- 1 props-percentage) props-percentage))
              (send -props-panel show #t))
            (when (send -props-panel is-shown?)
              (set! props-percentage
                    (cadr (send -split-panel get-percentages)))
              (send -split-panel delete-child -props-panel)
              (send -props-panel show #f))))

      ;;

      (define/public (get-controller) controller)

      ;;

      (define/public (get-main-panel) -main-panel)

      (define/public (shutdown)
        (unless (= props-percentage (send config pref:props-percentage))
          (send config pref:props-percentage props-percentage)))

      ;; syntax-browser<%> Methods

      (define/public (add-text text)
        (with-unlock -text
          (send -text insert text)))

      (define/public add-syntax
        (lambda/kw (stx #:key [hi-stxs null] hi-color alpha-table [definites null]
                        hi2-color [hi2-stxs null])
          (define (get-binder id)
            (module-identifier-mapping-get alpha-table id (lambda () #f)))
          (when (and (pair? hi-stxs) (not hi-color))
            (error 'syntax-widget%::add-syntax "no highlight color specified"))
          (let ([display (internal-add-syntax stx)]
                [definite-table (make-hash-table)])
            (when (and hi2-color (pair? hi2-stxs))
              (send display highlight-syntaxes hi2-stxs hi2-color))
            (when (and hi-color (pair? hi-stxs))
              (send display highlight-syntaxes hi-stxs hi-color))
            (for-each (lambda (x) (hash-table-put! definite-table x #t)) definites)
            (when alpha-table
              (let ([range (send display get-range)]
                    [start (send display get-start-position)])
                (define (adjust n) (+ start n))
                (for-each
                 (lambda (id)
                   #; ;; DISABLED
                   (match (identifier-binding id)
                     [(list src-mod src-name nom-mod nom-name _)
                      (for-each (lambda (id-r)
                                  (send -text add-billboard
                                        (adjust (car id-r))
                                        (adjust (cdr id-r))
                                        (string-append "from "
                                                       (mpi->string src-mod))
                                        (if (hash-table-get definite-table id #f)
                                            "blue"
                                            "purple")))
                                (send range get-ranges id))]
                     [_ (void)])
                   (let ([binder (get-binder id)])
                     (when binder
                       (for-each
                        (lambda (binder-r)
                          (for-each (lambda (id-r)
                                      (if (hash-table-get definite-table id #f)
                                          (send -text add-arrow
                                                (adjust (car binder-r))
                                                (adjust (cdr binder-r))
                                                (adjust (car id-r))
                                                (adjust (cdr id-r))
                                                "blue")
                                          (send -text add-question-arrow
                                                (adjust (car binder-r))
                                                (adjust (cdr binder-r))
                                                (adjust (car id-r))
                                                (adjust (cdr id-r))
                                                "purple")))
                                    (send range get-ranges id)))
                        (send range get-ranges binder)))))
                 (send range get-identifier-list))))
            display)))
      
      (define/public (add-separator)
        (with-unlock -text
          (send* -text
            (insert (new hrule-snip%))
            (insert "\n"))))

      (define/public (erase-all)
        (with-unlock -text
          (send -text erase)
          (send -text delete-all-drawings))
        (send controller remove-all-syntax-displays))

      (define/public (select-syntax stx)
        (send controller select-syntax stx))

      (define/public (get-text) -text)

      ;; internal-add-syntax : syntax -> display
      (define/private (internal-add-syntax stx)
        (with-unlock -text
          (parameterize ((current-default-columns (calculate-columns)))
            (let ([display (print-syntax-to-editor stx -text controller)])
              (send* -text
                (insert "\n")
                ;(scroll-to-position current-position)
                )
              display))))

      (define/private (calculate-columns)
        (define style (code-style -text))
        (define char-width (send style get-text-width (send -ecanvas get-dc)))
        (define-values (canvas-w canvas-h) (send -ecanvas get-client-size))
        (sub1 (inexact->exact (floor (/ canvas-w char-width)))))

      ;; Initialize
      (super-new)
      (setup-keymap)))


  ;; Specialized classes for widget

  (define widget-keymap%
    (class syntax-keymap%
      (init-field widget)
      (super-new (controller (send widget get-controller)))
      (inherit add-function)
      (inherit-field controller)

      (define/override (get-context-menu%)
        widget-context-menu%)

      (add-function "show-syntax-properties"
                    (lambda (i e)
                      (send widget toggle-props)))

      (define/public (get-widget) widget)))

  (define widget-context-menu%
    (class context-menu%
      (inherit-field keymap)
      (inherit-field props-menu)

      (define/override (on-demand)
        (send props-menu set-label
              (if (send (send keymap get-widget) props-shown?)
                  "Hide syntax properties"
                  "Show syntax properties"))
        (super on-demand))
      (super-new)))

  (define browser-text%
    (class (text:arrows-mixin
            (text:tacking-mixin
             (text:mouse-drawings-mixin
              (text:hide-caret/selection-mixin
               (editor:standard-style-list-mixin text:basic%)))))
      (define/override (default-style-name) "Basic")
      (super-new)))
  )
