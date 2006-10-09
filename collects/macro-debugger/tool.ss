
(module tool mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "unitsig.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "tool.ss" "drscheme")
           (lib "bitmap-label.ss" "mrlib")
           (lib "string-constant.ss" "string-constants")
           "model/trace.ss"
           (prefix view: "view/interfaces.ss")
           (prefix view: "view/gui.ss")
           (prefix view: "view/prefs.ss")
           (prefix sb: "syntax-browser/embed.ss"))

  (define view-base/tool@
    (unit/sig view:view-base^
        (import)
      (define base-frame%
        (frame:standard-menus-mixin frame:basic%))))

  (define stepper@
    (compound-unit/sig
      (import)
      (link [BASE : view:view-base^ (view-base/tool@)]
            [STEPPER : view:view^ (view:pre-stepper@ BASE)])
      (export (open STEPPER))))

  #;(define stepper@
      (compound-unit/sig
        (import)
        (link (PREFS : view:prefs^ (view:prefs@))
              (SB   : sb:implementation^ (sb:implementation@))
              (BASE : view:view-base^ (view-base/tool@))
              (VIEW : view:view^ (view:view@ PREFS BASE SB)))
        (export (open VIEW))))
  
  (define-values/invoke-unit/sig view:view^ stepper@)

  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      (define drscheme-eventspace (current-eventspace))

      (define-local-member-name check-language)
      (define-local-member-name get-debug-button)
      
      (define (macro-debugger-unit-frame-mixin %)
        (class %
          (super-new)
          (inherit get-button-panel
                   get-interactions-text
                   get-definitions-text)

          (define macro-debug-panel
            (new vertical-pane% (parent (get-button-panel))))
          (define macro-debug-button
            (new button%
                 (label (make-bitmap-label 
                         "Macro Stepper"
                         (build-path (collection-path "macro-debugger") 
                                     "view"
                                     "icon-small.png")))
                 (parent macro-debug-panel)
                 (callback (lambda (button event) (execute #t)))))

          (define/override (execute-callback)
            (execute #f))

          (define/private (execute debugging?)
            (send (get-interactions-text) enable-macro-debugging debugging?)
            (super execute-callback))

          (define/public (get-debug-button) macro-debug-button)

          ;; Hide button for inappropriate languages

          (define/augment (on-tab-change old new)
	    (check-language)
	    (inner (void) on-tab-change old new))

          (define/public (check-language)
	    (if (debugger-works-for?
                 (extract-language-level 
                  (send (get-definitions-text) get-next-settings)))
		(unless (send macro-debug-button is-shown?)
		  (send macro-debug-panel
                        add-child macro-debug-button))
		(when (send macro-debug-button is-shown?)
                  (send macro-debug-panel
                        delete-child macro-debug-button))))

          (send (get-button-panel) change-children
                (lambda (_)
                  (cons macro-debug-panel
                        (remq macro-debug-panel _))))
          (check-language)
          ))

      (define (macro-debugger-definitions-text-mixin %)
        (class %
          (inherit get-top-level-window)
          (define/augment (after-set-next-settings s)
	    (send (get-top-level-window) check-language)
	    (inner (void) after-set-next-settings s))
          (super-new)))

      (define (macro-debugger-tab-mixin %)
        (class %
          (inherit get-frame)
          (define/override (enable-evaluation)
            (super enable-evaluation)
            (send (send (get-frame) get-debug-button) enable #t))
          (define/override (disable-evaluation)
            (super disable-evaluation)
            (send (send (get-frame) get-debug-button) enable #f))
          (super-new)))

      (define (macro-debugger-interactions-text-mixin %)
        (class %
          (super-new)
          (inherit run-in-evaluation-thread)

          (define debugging? #f)
          (define/public (enable-macro-debugging ?)
            (set! debugging? ?))

          (define/override (reset-console)
            (super reset-console)
            (run-in-evaluation-thread
             (lambda ()
               (let-values ([(e mnr) 
                             (make-handlers (current-eval)
                                            (current-module-name-resolver))])
                 (current-eval e)
                 (current-module-name-resolver mnr)))))

          (define/private (make-handlers original-eval-handler original-module-name-resolver)
            (let ([stepper
                   (delay 
                     (let ([frame (new macro-stepper-frame%)])
                       (send frame show #t)
                       (send frame get-widget)))]
                  [debugging? debugging?])
              (values
               (lambda (expr)
                 (if (and debugging? (and (syntax? expr) (syntax-source expr)))
                     (let-values ([(e-expr deriv) (trace/result expr)])
                       (show-deriv deriv stepper)
                       (if (syntax? e-expr)
                           (parameterize ((current-eval original-eval-handler))
                             (original-eval-handler e-expr))
                           (raise e-expr)))
                     (original-eval-handler expr)))
               (lambda args
                 (let ([eo (current-expand-observe)]
                       [saved-debugging? debugging?])
                   (dynamic-wind
                       (lambda ()
                         (set! debugging? #f)
                         (when eo (current-expand-observe void)))
                       (lambda ()
                         (apply original-module-name-resolver args))
                       (lambda ()
                         (set! debugging? saved-debugging?)
                         (when eo (current-expand-observe eo)))))))))

          (define/private (show-deriv deriv stepper-promise)
            (parameterize ([current-eventspace drscheme-eventspace])
              (queue-callback
               (lambda () (send (force stepper-promise) add-deriv deriv)))))
          ))

      ;; Borrowed from mztake/debug-tool.ss

      (define (extract-language-level settings)
	(let* ([language
                (drscheme:language-configuration:language-settings-language
                 settings)])
	  (send language get-language-position)))

      (define (debugger-works-for? lang)
        (let ([main-group (car lang)]
              [second (and (pair? (cdr lang)) (cadr lang))]
              [third (and (pair? (cdr lang)) (pair? (cddr lang)) (caddr lang))])
          (and (equal? main-group (string-constant professional-languages))
               (or (member second 
                           (list (string-constant r5rs-lang-name)
                                 "(module ...)"
                                 "Swindle"))
                   (member third
                           (list (string-constant mzscheme-w/debug)
                                 (string-constant mred-w/debug)
                                 (string-constant pretty-big-scheme)))))))
      
      ;; Macro debugger code

      (drscheme:get/extend:extend-unit-frame
       macro-debugger-unit-frame-mixin)
      (drscheme:get/extend:extend-interactions-text 
       macro-debugger-interactions-text-mixin)
      (drscheme:get/extend:extend-definitions-text
       macro-debugger-definitions-text-mixin)
      (drscheme:get/extend:extend-tab
       macro-debugger-tab-mixin)

      )))
