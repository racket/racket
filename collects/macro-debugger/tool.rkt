#lang racket/base
(require racket/list
         racket/unit
         racket/match
         racket/gui
         framework
         drscheme/tool
         mrlib/switchable-button
         string-constants
         "model/trace.rkt"
         "model/deriv.rkt"
         "model/deriv-util.rkt"
         "view/frame.rkt"
         (only-in "view/view.rkt" macro-stepper-director%)
         "view/stepper.rkt"
         "view/prefs.rkt")

;; Capability name: 'macro-stepper:enabled

(provide tool@)

(define-local-member-name allow-macro-stepper?)
(define-local-member-name run-macro-stepper)

(define frame/supports-macro-stepper<%>
  (interface ()
    allow-macro-stepper?
    run-macro-stepper))

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


(define macro-stepper-button-label "Macro Stepper")

(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)

    (define (phase1)
      (drscheme:module-language-tools:add-opt-out-toolbar-button
       (λ (frame parent)
         (new switchable-button%
               (label macro-stepper-button-label)
               (bitmap macro-debugger-bitmap)
               (alternate-bitmap macro-debugger-up-bitmap)
               (parent parent)
               (callback (lambda (button) (send frame run-macro-stepper)))))
       'macro-stepper)
      (drscheme:language:register-capability
       'macro-stepper:enabled
       boolean?
       #f))
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
      (class* % (frame/supports-macro-stepper<%>)
        (super-new)
        (inherit get-button-panel
                 get-language-menu
                 get-interactions-text
                 get-definitions-text)

        (define macro-debug-panel
          (new horizontal-pane%
               (parent (get-button-panel))
               (stretchable-height #f)
               (stretchable-width #f)))
        (define macro-debug-button
          (new switchable-button%
               (label macro-stepper-button-label)
               (bitmap macro-debugger-bitmap)
               (alternate-bitmap macro-debugger-up-bitmap)
               (parent macro-debug-panel)
               (callback (lambda (button) (run-macro-stepper)))))
        (inherit register-toolbar-button)
        (register-toolbar-button macro-debug-button)

        (define/augment (enable-evaluation)
          (send macro-debug-button enable #t)
          (inner (void) enable-evaluation))
        (define/augment (disable-evaluation)
          (send macro-debug-button enable #f)
          (inner (void) disable-evaluation))

        (define macro-debug-menu-item
          (let ([lang-menu (get-language-menu)])
            (new separator-menu-item% (parent lang-menu))
            (new menu-item%
                 (label "Macro Stepper")
                 (parent lang-menu)
                 (callback (lambda _ (run-macro-stepper))))))

        (define/public-final (run-macro-stepper)
          (execute #t))

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
          (enable/disable-stuff (allow-macro-stepper?)))

        (define/public (allow-macro-stepper?)
          (let ([lang
                 (drscheme:language-configuration:language-settings-language
                  (send (get-definitions-text) get-next-settings))])
            (send lang capability-value 'macro-stepper:enabled)))

        (define/private (enable/disable-stuff enable?)
          (if enable?
              (begin (send macro-debug-menu-item enable #t)
                     (unless (send macro-debug-button is-shown?)
                       (send macro-debug-panel
                             add-child macro-debug-button)))
              (begin (send macro-debug-menu-item enable #f)
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

        ;; Borrowed from stepper/stepper-tool
        (define metadata-changing-now? #f)

        ;; don't pay attention to changes that occur on metadata.
        ;; this assumes that metadata changes cannot be nested.
        (define/augment (begin-metadata-changes)
          (set! metadata-changing-now? #t)
          (inner (void) begin-metadata-changes))

        (define/augment (end-metadata-changes)
          (set! metadata-changing-now? #f)
          (inner (void) end-metadata-changes))

        (define/private (notify-macro-stepper-of-change)
          (let ([win (get-top-level-window)])
            ;; should only be #f when win is #f
            (when (is-a? win drscheme:unit:frame<%>)
              (let ([interactions (send win get-interactions-text)])
                (send interactions obsolete-macro-debugger)))))

        ;; Catch program changes and mark macro stepper obsolete.
        (define/augment (on-insert x y)
          (unless metadata-changing-now?
            (notify-macro-stepper-of-change))
          (inner (void) on-insert x y))

        (define/augment (on-delete x y)
          (unless metadata-changing-now?
            (notify-macro-stepper-of-change))
          (inner (void) on-delete x y))

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

        (define/public (obsolete-macro-debugger)
          (when current-stepper-director
            (send current-stepper-director add-obsoleted-warning)))

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

        (define/private (expand+trace expr)
          (parameterize ((trace-macro-limit (pref:macro-step-limit))
                         (trace-limit-handler
                          (lambda (c) (handle-macro-limit c))))
            (trace* expr)))

        (define/private (handle-macro-limit c)
          (define option
            (message-box/custom
             "Macro stepper"
             (string-append
              "Macro expansion has taken a suspiciously large number of steps.\n"
              "\n"
              "Click Stop to stop macro expansion and see the steps taken "
              "so far, or click Continue to let it run a bit longer.")
             "Continue"
             "Stop"
             #f
             (get-top-level-window)))
          (case option
            ((2)
             (error "Macro expansion was stopped because it took too many steps."))
            (else (* 2 c))))

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
                (let-values ([(e-expr events derivp) (expand+trace expr)])
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

    ;; Macro debugger code

    (drscheme:get/extend:extend-unit-frame
     macro-debugger-unit-frame-mixin)
    (drscheme:get/extend:extend-interactions-text 
     macro-debugger-interactions-text-mixin)
    (drscheme:get/extend:extend-definitions-text
     macro-debugger-definitions-text-mixin)

    (define (add-macro-stepper-key-bindings keymap)
      (send keymap add-function
            "macro stepper"
            (lambda (obj evt)
              (when (is-a? obj editor<%>)
                (let ([canvas (send obj get-canvas)])
                  (when canvas
                    (let ([frame (send canvas get-top-level-window)])
                      (when (is-a? frame frame/supports-macro-stepper<%>)
                        (when (send frame allow-macro-stepper?)
                          (send frame run-macro-stepper)))))))))
      (send keymap map-function "c:c;c:m" "macro stepper"))

    (add-macro-stepper-key-bindings (drscheme:rep:get-drs-bindings-keymap))
    ))
