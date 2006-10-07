
(module widget mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "list.ss")
           "interfaces.ss"
           "params.ss"
           "controller.ss"
           "typesetter.ss"
           "hrule-snip.ss"
           "properties.ss"
           "util.ss")
  (provide widget@
           widget-context-menu-extension@)

  (define widget@
    (unit/sig widget^
      (import keymap^
              context-menu^)
      
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
          
          (define/public (make-context-menu)
            (new context-menu% (widget this)))
          
          (new syntax-keymap% 
               (editor -text)
               (context-menu (make-context-menu)))
          
          (send -text lock #t)
          (send -split-panel set-percentages 
                (list (- 1 props-percentage) props-percentage))
          (toggle-props)
          
          ;; syntax-properties-controller<%> methods
          
          (define/public (set-syntax stx)
            (send props set-syntax stx))
          
          (define/public (show ?)
            (if ? (show-props) (hide-props)))
          
          (define/public (props-shown?)
            (send -props-panel is-shown?))
          
          (define/public (toggle-props)
            (if (send -props-panel is-shown?)
                (hide-props)
                (show-props)))
          
          (define/public (hide-props)
            (when (send -props-panel is-shown?)
              (set! props-percentage (cadr (send -split-panel get-percentages)))
              (send -split-panel delete-child -props-panel)
              (send -props-panel show #f)))
          
          (define/public (show-props)
            (unless (send -props-panel is-shown?)
              (send -split-panel add-child -props-panel)
              (send -split-panel set-percentages
                    (list (- 1 props-percentage) props-percentage))
              (send -props-panel show #t)))
          
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
            (case-lambda
              [(stx) 
               (internal-add-syntax stx null #f)]
              [(stx hi-stxs hi-color)
               (internal-add-syntax stx hi-stxs hi-color)]))
          
          (define/public (add-separator)
            (with-unlock -text
              (send* -text
                (insert (new hrule-snip%))
                (insert "\n"))))
          
          (define/public (erase-all)
            (with-unlock -text (send -text erase))
            (send controller erase))
          
          (define/public (select-syntax stx)
            (send controller select-syntax stx))
          
          (define/public (get-text) -text)
          
          (define/private (internal-add-syntax stx hi-stxs hi-color)
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
                    (unless (null? hi-stxs)
                      (send new-colorer highlight-syntaxes hi-stxs hi-color)))))))
          
          (define/private (calculate-columns)
            (define style (code-style -text))
            (define char-width (send style get-text-width (send -ecanvas get-dc)))
            (define-values (canvas-w canvas-h) (send -ecanvas get-client-size))
            (sub1 (inexact->exact (floor (/ canvas-w char-width)))))
          
          (super-new)))
      
      ))

  (define widget-context-menu-extension@
    (unit/sig context-menu^
      (import (pre : context-menu^))
      
      (define context-menu%
        (class pre:context-menu%
          (init-field widget)
          
          (define props-menu #f)

          (define/override (after-selection-items)
            (super after-selection-items)
            (set! props-menu
                  (new menu-item% (label "Show/hide syntax properties")
                       (parent this)
                       (callback (lambda _ (send widget toggle-props)))))
            (void))
          
          (define/override (on-demand)
            (send props-menu set-label
                  (if (send widget props-shown?)
                      "Hide syntax properties"
                      "Show syntax properties"))
            (super on-demand))
          
          (super-new (controller (send widget get-controller)))))))

  (define browser-text% (editor:standard-style-list-mixin text:basic%))
  )
