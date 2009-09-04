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
         (only-in "view/view.ss" macro-stepper-director%)
         "view/stepper.ss"
         "view/prefs.ss")

(provide tool@
         language/macro-stepper<%>)

(define language/macro-stepper<%>
  (interface ()
    enable-macro-stepper?))

(define (drscheme-macro-stepper-frame-mixin %)
  (class %
    (define/override (get-macro-stepper-widget%)
      (macro-stepper-widget/process-mixin
       (super get-macro-stepper-widget%)))
    (super-new)))

(define macro-stepper-frame%
  (drscheme-macro-stepper-frame-mixin
   (macro-stepper-frame-mixin
    (frame:standard-menus-mixin
     frame:basic%))))

(define drscheme-macro-stepper-director%
  (class macro-stepper-director%
    (init-field filename)
    (define eventspace (current-eventspace))

    (define stepper #f)
    (inherit new-stepper)

    (define/public (lazy-new-stepper)
      (unless stepper
        (set! stepper (new-stepper))))

    (define/override (add-trace events)
      (lazy-new-stepper)
      (parameterize ((current-eventspace eventspace))
        (queue-callback
         (lambda ()
           (super add-trace events)))))
    (define/override (add-deriv deriv)
      (lazy-new-stepper)
      (parameterize ((current-eventspace eventspace))
        (queue-callback
         (lambda ()
           (super add-deriv deriv)))))

    (define/override (new-stepper-frame)
      (parameterize ((current-eventspace eventspace))
        (new macro-stepper-frame%
             (config (new macro-stepper-config/prefs%))
             (filename filename)
             (director this))))

    (super-new)))


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
    (define drscheme-custodian (current-custodian))

    (define-local-member-name check-language)
    
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
          (new horizontal-pane%
               (parent (get-button-panel))
               (stretchable-height #f)
               (stretchable-width #f)))
        (define macro-debug-button
          (new switchable-button%
               (label "Macro Stepper")
               (bitmap macro-debugger-bitmap)
               (alternate-bitmap macro-debugger-up-bitmap)
               (parent macro-debug-panel)
               (callback (λ (button) (execute #t)))))
        (inherit register-toolbar-button)
        (register-toolbar-button macro-debug-button)

        (define/augment (enable-evaluation)
          (send macro-debug-button enable #t)
          (inner (void) enable-evaluation))
        (define/augment (disable-evaluation)
          (send macro-debug-button enable #f)
          (inner (void) disable-evaluation))

        (define/override (execute-callback)
          (execute #f))

        (define/private (execute debugging?)
          (send (get-interactions-text) enable-macro-debugging debugging?)
          (super execute-callback))

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

    (define (macro-debugger-interactions-text-mixin %)
      (class %
        (super-new)
        (inherit run-in-evaluation-thread
                 get-top-level-window)

        (define debugging? #f)

        (define current-stepper-director #f)
        
        (define/public (enable-macro-debugging ?)
          (set! debugging? ?))

        (define/override (reset-console)
          (super reset-console)
          (when current-stepper-director
            (send current-stepper-director add-obsoleted-warning)
            (set! current-stepper-director #f))

          ;; setting the eval handler at this point disables CM,
          ;; so only do it when we are debugging
          (when debugging?
            (run-in-evaluation-thread
             (lambda ()
               (let-values ([(e mnr) 
                             (make-handlers (current-eval)
                                            (current-module-name-resolver))])
                 (current-eval e)
                 (current-module-name-resolver mnr))))))

        (define/private (make-stepper filename)
          (parameterize ((current-eventspace
                          (parameterize ((current-eventspace drscheme-eventspace)
                                         (current-custodian drscheme-custodian))
                            (make-eventspace))))
            (new drscheme-macro-stepper-director% (filename filename))))

        (define/private (inner-eval original-eval-handler e-expr)
          (original-eval-handler e-expr))

        (define/private (make-handlers original-eval-handler
                                       original-module-name-resolver)
          (define filename (send (send (get-top-level-window) get-definitions-text)
                                 get-filename/untitled-name))
          (define director (make-stepper filename))
          (define local-debugging? debugging?)
          (define (call-without-debugging thunk)
            (let ([eo (current-expand-observe)]
                  [saved-debugging? local-debugging?])
              (dynamic-wind
                  (lambda ()
                    (set! local-debugging? #f)
                    (when eo (current-expand-observe void)))
                  thunk
                  (lambda ()
                    (set! local-debugging? saved-debugging?)
                    (when eo (current-expand-observe eo))))))
          (define (the-eval expr)
            (if (and local-debugging? (syntax? expr))
                (let-values ([(e-expr events derivp) (trace* expr expand)])
                  (show-deriv director events)
                  (if (syntax? e-expr)
                      (inner-eval e-expr)
                      (raise e-expr)))
                (original-eval-handler expr)))
          (define (inner-eval e-expr)
            (if #f ;; fixme: turn into parameter/preference???
                (call-without-debugging (lambda () (original-eval-handler e-expr)))
                (original-eval-handler e-expr)))
          (define (the-module-resolver . args)
            (call-without-debugging
             (lambda () (apply original-module-name-resolver args))))
          (set! current-stepper-director director)
          (values the-eval
                  the-module-resolver))

        (define/private (show-deriv director events)
          (send director add-trace events))
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
                                   "Swindle"
                                   (string-constant pretty-big-scheme))))))))
    
    ;; Macro debugger code

    (drscheme:get/extend:extend-unit-frame
     macro-debugger-unit-frame-mixin)
    (drscheme:get/extend:extend-interactions-text 
     macro-debugger-interactions-text-mixin)
    (drscheme:get/extend:extend-definitions-text
     macro-debugger-definitions-text-mixin)

    ))
