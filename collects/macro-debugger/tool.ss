
#lang scheme/base
(require scheme/base
         scheme/list
         scheme/unit
         scheme/match
         scheme/gui
         framework/framework
         drscheme/tool
         mrlib/switchable-button
         string-constants
         "model/trace.ss"
         "model/deriv.ss"
         "model/deriv-util.ss"
         "view/frame.ss"
         "view/stepper.ss"
         "view/prefs.ss")

(provide tool@
         language/macro-stepper<%>)

(define language/macro-stepper<%>
  (interface ()
    enable-macro-stepper?))

(define (ext-macro-stepper-frame-mixin %)
  (class %
    (define/override (get-macro-stepper-widget%)
      (macro-stepper-widget/process-mixin
       (super get-macro-stepper-widget%)))
    (super-new)))

(define macro-stepper-frame%
  (ext-macro-stepper-frame-mixin
   (macro-stepper-frame-mixin
    (frame:standard-menus-mixin
     frame:basic%))))

(define tool@
  (unit (import drscheme:tool^)
        (export drscheme:tool-exports^)
    
    (define (phase1)
      (drscheme:language:extend-language-interface
       language/macro-stepper<%>
       (mixin (drscheme:language:language<%>) (language/macro-stepper<%>)
         (inherit get-language-position)
         (define/public (enable-macro-stepper?)
           (macro-stepper-works-for? (get-language-position)))
         (super-new))))
    (define (phase2) (void))

    (define drscheme-eventspace (current-eventspace))

    (define-local-member-name check-language)
    (define-local-member-name get-debug-button)
    
    (define macro-debugger-bitmap 
      (make-object bitmap%
        (build-path (collection-path "icons") "macro-stepper.png")
        'png/mask))

    (define macro-debugger-up-bitmap
      (make-object bitmap%
        (build-path (collection-path "icons") "macro-stepper-narrow.png")
        'png/mask))

    (define (macro-debugger-unit-frame-mixin %)
      (class %
        (super-new)
        (inherit get-button-panel
                 get-interactions-text
                 get-definitions-text)

        (define macro-debug-panel
          (new vertical-pane% (parent (get-button-panel))))
        (define macro-debug-button
          (new switchable-button%
               (label "Macro Stepper")
               (bitmap macro-debugger-bitmap)
               (alternate-bitmap macro-debugger-up-bitmap)
               (parent macro-debug-panel)
               (callback (λ (button) (execute #t)))))
        (inherit register-toolbar-button)
        (register-toolbar-button macro-debug-button)

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
          (let ([lang
                 (drscheme:language-configuration:language-settings-language
                  (send (get-definitions-text) get-next-settings))])
            (if (send lang enable-macro-stepper?)
                (unless (send macro-debug-button is-shown?)
                        (send macro-debug-panel
                              add-child macro-debug-button))
                (when (send macro-debug-button is-shown?)
                      (send macro-debug-panel
                            delete-child macro-debug-button)))))

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
          (let ([tlw (get-top-level-window)])
            (when tlw
              (send tlw check-language)))
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
        (inherit run-in-evaluation-thread
                 get-top-level-window)

        (define debugging? #f)

        (define current-stepper #f)
        
        (define/public (enable-macro-debugging ?)
          (set! debugging? ?))

        (define/override (reset-console)
          (super reset-console)
          (when current-stepper
            #;(message-box "obsoleting stepper" "before" #f '(ok))
            (send current-stepper add-obsoleted-warning)
            #;(message-box "obsoleting stepper" "after" #f '(ok))
            (set! current-stepper #f))
          (run-in-evaluation-thread
           (lambda ()
             (let-values ([(e mnr) 
                           (make-handlers (current-eval)
                                          (current-module-name-resolver))])
               (current-eval e)
               (current-module-name-resolver mnr)))))

        (define/private (make-stepper filename)
          (let ([frame (new macro-stepper-frame%
                            (filename filename)
                            (config (new macro-stepper-config/prefs%)))])
            (set! current-stepper frame)
            (send frame show #t)
            (send frame get-widget)))

        (define/private (make-handlers original-eval-handler original-module-name-resolver)
          (let* ([filename (send (send (get-top-level-window) get-definitions-text)
                                 get-filename/untitled-name)]
                 [stepperp (delay (make-stepper filename))]
                 [debugging? debugging?])
            (values
             (lambda (expr)
               (if (and debugging? (syntax? expr))
                   (let-values ([(e-expr events derivp) (trace* expr expand)])
                     (show-deriv stepperp events)
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

        (define/private (show-deriv stepperp events)
          (parameterize ([current-eventspace drscheme-eventspace])
            (queue-callback
             (lambda ()
               (send (force stepperp) add-trace events)))))
        ))

    ;; Borrowed from mztake/debug-tool.ss

    (define (macro-stepper-works-for? lang)
      (let ([main-group (car lang)]
            [second (and (pair? (cdr lang)) (cadr lang))]
            [third (and (pair? (cdr lang)) (pair? (cddr lang)) (caddr lang))])
        (or (equal? main-group "Module")
            (and (equal? main-group (string-constant legacy-languages))
                 (or (member second 
                             (list (string-constant r5rs-language-name)
                                   "Swindle"))
                     (member third
                             (list (string-constant mzscheme-w/debug)
                                   (string-constant mred-w/debug)
                                   (string-constant pretty-big-scheme))))))))
    
    ;; Macro debugger code

    (drscheme:get/extend:extend-unit-frame
     macro-debugger-unit-frame-mixin)
    (drscheme:get/extend:extend-interactions-text 
     macro-debugger-interactions-text-mixin)
    (drscheme:get/extend:extend-definitions-text
     macro-debugger-definitions-text-mixin)
    (drscheme:get/extend:extend-tab
     macro-debugger-tab-mixin)

    ))
