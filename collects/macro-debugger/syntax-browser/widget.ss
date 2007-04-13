
(module widget mzscheme
  (require (lib "class.ss")
           (lib "unit.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "list.ss")
           (lib "kw.ss")
           (lib "boundmap.ss" "syntax")
           "interfaces.ss"
           "params.ss"
           "controller.ss"
           "typesetter.ss"
           "hrule-snip.ss"
           "properties.ss"
           "text.ss"
           "util.ss")
  (provide widget@
           widget-keymap-extension@
           widget-context-menu-extension@)

  (define widget@
    (unit
      (import keymap^)
      (export widget^)

      ;; syntax-widget%
      ;; A syntax-widget creates its own syntax-controller.
      (define syntax-widget%
        (class* object% (syntax-browser<%> syntax-properties-controller<%>)
          (init parent)
          (init-field pref:props-percentage)
          
          (define -main-panel (new vertical-panel% (parent parent)))
          (define -split-panel (new panel:horizontal-dragable% (parent -main-panel)))
          (define -text (new browser-text%))
          (define -ecanvas (new editor-canvas% (parent -split-panel) (editor -text)))
          (define -props-panel (new horizontal-panel% (parent -split-panel)))
          (define props (new properties-view% (parent -props-panel)))
          (define props-percentage (pref:props-percentage))
          
          (define controller
            (new syntax-controller%
                 (properties-controller this)))
          
          (define/public (make-keymap text)
            (new syntax-keymap% 
                 (editor text)
                 (widget this)))
          (make-keymap -text)
          
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
          
          (define/public (save-prefs)
            (unless (= props-percentage (pref:props-percentage))
              (pref:props-percentage props-percentage)))
          
          ;; syntax-browser<%> Methods
          
          (define/public (add-text text)
            (with-unlock -text
              (send -text insert text)))
          
          (define/public add-syntax
            (lambda/kw (stx #:key [hi-stxs null] hi-color alpha-table [definites null]
                            hi2-color [hi2-stxs null])
              (when (and (pair? hi-stxs) (not hi-color))
                (error 'syntax-widget%::add-syntax "no highlight color specified"))
              (let ([colorer (internal-add-syntax stx)]
                    [definite-table (make-hash-table)])
                (when (and hi2-color (pair? hi2-stxs))
                  (send colorer highlight-syntaxes hi2-stxs hi2-color))
                (when (and hi-color (pair? hi-stxs))
                  (send colorer highlight-syntaxes hi-stxs hi-color))
                (for-each (lambda (x) (hash-table-put! definite-table x #t)) definites)
                (when alpha-table
                  (let ([range (send colorer get-range)])
                    (for-each (lambda (id)
                                (let ([binder
                                       (module-identifier-mapping-get alpha-table
                                                                      id
                                                                      (lambda () #f))])
                                  (when binder
                                    (for-each
                                     (lambda (binder-r)
                                       (for-each (lambda (id-r)
                                                   (if (hash-table-get definite-table id #f)
                                                       (send -text add-arrow
                                                             (car id-r) (cdr id-r)
                                                             (car binder-r) (cdr binder-r)
                                                             "blue")
                                                       (send -text add-question-arrow
                                                             (car id-r) (cdr id-r)
                                                             (car binder-r) (cdr binder-r)
                                                             "purple")))
                                                 (send range get-ranges id)))
                                     (send range get-ranges binder)))))
                              (send colorer get-identifier-list))))
                colorer)))
          
          (define/public (add-separator)
            (with-unlock -text
              (send* -text
                (insert (new hrule-snip%))
                (insert "\n"))))
          
          (define/public (erase-all)
            (with-unlock -text
              (send -text erase)
              (send -text delete-all-drawings))
            (send controller erase))
          
          (define/public (select-syntax stx)
            (send controller select-syntax stx))
          
          (define/public (get-text) -text)
          
          (define/private (internal-add-syntax stx)
            (with-unlock -text
              (parameterize ((current-default-columns (calculate-columns)))
                (let ([current-position (send -text last-position)])
                  (let* ([new-ts (new typesetter-for-text%
                                      (controller controller)
                                      (syntax stx)
                                      (text -text))]
                         [new-colorer (send new-ts get-colorer)])
                    (send* -text
                      (insert "\n")
                      (scroll-to-position current-position))
                    new-colorer)))))

          (define/private (calculate-columns)
            (define style (code-style -text))
            (define char-width (send style get-text-width (send -ecanvas get-dc)))
            (define-values (canvas-w canvas-h) (send -ecanvas get-client-size))
            (sub1 (inexact->exact (floor (/ canvas-w char-width)))))

          (super-new)))
      
      ))

  (define widget-keymap-extension@
    (unit
      (import (prefix pre: keymap^))
      (export keymap^)

      (define syntax-keymap%
        (class pre:syntax-keymap%
          (init-field widget)
          (super-new (controller (send widget get-controller)))
          (inherit add-function)
          
          (add-function "show-syntax-properties"
                        (lambda (i e)
                          (send widget toggle-props)))
          
          (define/public (get-widget) widget)
          ))))
  
  (define widget-context-menu-extension@
    (unit
      (import (prefix pre: context-menu^))
      (export context-menu^)

      (define context-menu%
        (class pre:context-menu%
          (inherit-field keymap)
          (inherit-field props-menu)

          (define/override (on-demand)
            (send props-menu set-label
                  (if (send (send keymap get-widget) props-shown?)
                      "Hide syntax properties"
                      "Show syntax properties"))
            (super on-demand))
          (super-new)))))
  
  (define browser-text%
    (text:arrows-mixin
     (text:mouse-drawings-mixin
      (text:drawings-mixin
       (text:hide-caret/selection-mixin
        (editor:standard-style-list-mixin text:basic%))))))
  )
