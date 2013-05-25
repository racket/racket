#lang racket/base
(require racket/unit
         racket/gui/base
         racket/class
         framework
         drracket/tool
         mrlib/switchable-button
         macro-debugger/model/trace
         macro-debugger/view/frame
         (only-in macro-debugger/view/view macro-stepper-director%)
         macro-debugger/view/stepper
         macro-debugger/view/prefs
         images/compile-time
         (for-syntax racket/base images/icons/tool)
         ;; FIXME:
         drracket/private/syncheck/local-member-names
         drracket/private/eval-helpers)

;; Capability name: 'macro-stepper:enabled

(provide tool@)

(define-local-member-name allow-macro-stepper?)
(define-local-member-name run-macro-stepper)

(define frame/supports-macro-stepper<%>
  (interface ()
    allow-macro-stepper?
    run-macro-stepper))

(define (drracket-macro-stepper-frame-mixin %)
  (class %
    (define/override (file-menu:create-new?) #t)
    (define/override (file-menu:create-open?) #t)
    (define/override (file-menu:create-open-recent?) #t)
    (define/override (get-macro-stepper-widget%)
      (macro-stepper-widget/process-mixin
       (super get-macro-stepper-widget%)))
    (super-new)))

(define macro-stepper-frame%
  (drracket-macro-stepper-frame-mixin
   (macro-stepper-frame-mixin
    (frame:standard-menus-mixin
     frame:basic%))))

(define drracket-macro-stepper-director%
  (class macro-stepper-director%
    (init-field filename)
    (inherit-field stepper-frames)
    (define eventspace (current-eventspace))

    (define stepper #f)
    (inherit new-stepper)

    (define/private (lazy-new-stepper)
      (unless stepper
        (set! stepper (new-stepper))))

    (define/override (add-trace events)
      (parameterize ((current-eventspace eventspace))
        (queue-callback
         (lambda ()
           (lazy-new-stepper)
           (super add-trace events)))))
    (define/override (add-deriv deriv)
      (parameterize ((current-eventspace eventspace))
        (queue-callback
         (lambda ()
           (lazy-new-stepper)
           (super add-deriv deriv)))))

    (define/override (new-stepper-frame)
      (parameterize ((current-eventspace eventspace))
        (new macro-stepper-frame%
             (config (new macro-stepper-config/prefs%))
             (filename filename)
             (director this))))

    (define/public (shutdown)
      (when (pref:close-on-reset-console?)
        (for ([(frame flags) (in-hash stepper-frames)])
          (unless (memq 'no-obsolete flags)
            (send frame show #f)))))

    (super-new)))


(define macro-stepper-button-label "Macro Stepper")

(define macro-debugger-bitmap (compiled-bitmap (macro-stepper-icon)))
(define small-macro-debugger-bitmap (compiled-bitmap (small-macro-stepper-icon)))

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1)
      (drracket:module-language-tools:add-opt-out-toolbar-button
       (λ (frame parent)
         (new switchable-button%
               (label macro-stepper-button-label)
               (bitmap macro-debugger-bitmap)
               (alternate-bitmap small-macro-debugger-bitmap)
               (parent parent)
               (callback (lambda (button) (send frame run-macro-stepper)))))
       'macro-stepper
       #:number 70)
      (drracket:language:register-capability
       'macro-stepper:enabled
       boolean?
       #f))
    (define (phase2) (void))

    (define drracket-eventspace (current-eventspace))
    (define drracket-custodian (current-custodian))

    (define-local-member-name check-language)
    
    (define (macro-debugger-unit-frame-mixin %)
      (class* % (frame/supports-macro-stepper<%>)
        (super-new)
        (inherit get-button-panel
                 get-language-menu
                 get-interactions-text
                 get-definitions-text
                 get-top-level-window
                 ensure-rep-hidden
                 get-current-tab)

        (define macro-debug-panel
          (new panel:horizontal-discrete-sizes%
               (parent (get-button-panel))))
        (define macro-debug-button
          (new switchable-button%
               (label macro-stepper-button-label)
               (bitmap macro-debugger-bitmap)
               (alternate-bitmap small-macro-debugger-bitmap)
               (parent macro-debug-panel)
               (callback (lambda (button) (run-macro-stepper)))))
        (inherit register-toolbar-button)
        (register-toolbar-button macro-debug-button #:number 70)

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

        ;; Hide button for inappropriate languages

        (define/augment (on-tab-change old new)
          (check-language)
          (inner (void) on-tab-change old new))

        (define/public (check-language)
          (enable/disable-stuff (allow-macro-stepper?)))

        (define/public (allow-macro-stepper?)
          (let ([lang
                 (drracket:language-configuration:language-settings-language
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

        ;; ----

        (define/public-final (run-macro-stepper)

          ;; FIXME!!! Lots of this is copied out of drracket/private/syncheck/gui.rkt
          ;; except some of the code (eg error handling) thrown away to avoid pulling
          ;; in lots more. Need to abstract.

          (ensure-rep-hidden)
          (define definitions-text (get-definitions-text))
          (define interactions-text (get-interactions-text))
          (define drs-eventspace (current-eventspace))
          (define drs-custodian (current-custodian))
          (define the-tab (get-current-tab))
          (define-values (old-break-thread old-custodian) (send the-tab get-breakables))
          (define error-port (send (send the-tab get-error-report-text) get-err-port))
          (define output-port (send (send the-tab get-error-report-text) get-out-port))
          (send the-tab disable-evaluation) ;; this locks the editor, so must be outside.
          (define settings (send definitions-text get-next-settings))
          (define module-language?
            (is-a? (drracket:language-configuration:language-settings-language settings)
                   drracket:module-language:module-language<%>))
          (define error-display-semaphore (make-semaphore 0))

          ;; --

          (define user-custodian #f)
          (define normal-termination? #f)
          (define original-module-name-resolver #f)

          ;; --

          (define director
            (parameterize ((current-eventspace drracket-eventspace)
                           (current-custodian drracket-custodian))
              (let ([filename (send definitions-text get-filename/untitled-name)])
                (new drracket-macro-stepper-director% (filename filename)))))
          (send interactions-text set-macro-stepper-director director)

          (define (the-module-name-resolver . args)
            (parameterize ((current-expand-observe void))
              (apply original-module-name-resolver args)))

          ;; --

          (define (init-proc) ;; =user=
            (set! original-module-name-resolver (current-module-name-resolver))
            (current-module-name-resolver the-module-name-resolver)

            (send the-tab set-breakables (current-thread) (current-custodian))
            (set-directory definitions-text)
            (current-load-relative-directory #f)
            (current-error-port error-port)
            (current-output-port output-port)
            (error-display-handler 
             (λ (msg exn) ;; =user=
                (parameterize ([current-eventspace drs-eventspace])
                  (queue-callback
                   (λ () ;; =drs=
                      ;; this has to come first or else the positioning
                      ;; computations in the highlight-errors/exn method
                      ;; will be wrong by the size of the error report box
                      (show-error-report/tab)
                      ;; a call like this one also happens in 
                      ;; drracket:debug:error-display-handler/stacktrace
                      ;; but that call won't happen here, because
                      ;; the rep is not in the current-rep parameter
                      (send interactions-text highlight-errors/exn exn))))
                (drracket:debug:error-display-handler/stacktrace
                 msg exn '()
                 #:definitions-text definitions-text)
                (semaphore-post error-display-semaphore)))
            (error-print-source-location #f) ; need to build code to render error first
            (uncaught-exception-handler
             (let ([oh (uncaught-exception-handler)])
               (λ (exn)
                  (uncaught-exception-raised)
                  (oh exn))))
            (set! user-custodian (current-custodian)))

          (define (uncaught-exception-raised) ;; =user=
            (set! normal-termination? #t)
            (parameterize ([current-eventspace drs-eventspace])
              (queue-callback
               (λ ()
                  (cleanup)
                  (custodian-shutdown-all user-custodian)))))
          (define (show-error-report/tab) ;; =drs=
            (send the-tab turn-on-error-report)
            (send (send the-tab get-error-report-text) scroll-to-position 0)
            (when (eq? (get-current-tab) the-tab)
              ;; (show-error-report)
              (void)))
          (define (cleanup) ;; =drs=
            (send the-tab set-breakables old-break-thread old-custodian)
            (send the-tab enable-evaluation)
            ;; do this with some lag ... not great, but should be okay.
            (let ([err-port (send (send the-tab get-error-report-text) get-err-port)])
              (thread
               (λ ()
                  (flush-output err-port)
                  (queue-callback
                   (λ ()
                      (unless (= 0 (send (send the-tab get-error-report-text) last-position))
                        (show-error-report/tab))))))))
          (define (kill-termination)
            (unless normal-termination?
              (parameterize ([current-eventspace drs-eventspace])
                (queue-callback
                 (λ ()
                    (cleanup)
                    (custodian-shutdown-all user-custodian))))))

          (with-lock/edit-sequence definitions-text
            (lambda ()
              (send the-tab clear-annotations)
              (send the-tab reset-offer-kill)
              (define get-terms
                (drracket:eval:traverse-program/multiple
                 settings init-proc kill-termination
                 #:gui-modules? #f))
              (get-terms
               (drracket:language:make-text/pos definitions-text
                                                0
                                                (send definitions-text last-position))
               (λ (sexp loop) ; =user=
                  (cond [(eof-object? sexp)
                         (set! normal-termination? #t)
                         (parameterize ([current-eventspace drs-eventspace])
                           (queue-callback
                            (λ () ; =drs=
                               (cleanup)
                               (custodian-shutdown-all user-custodian))))]
                        [(syntax? sexp)
                         (let-values ([(e-expr events derivp) (expand+trace sexp)])
                           (send director add-trace events)
                           (cond [(syntax? e-expr)
                                  ;; FIXME: eval compile-time parts?
                                  (void)]
                                 [else (raise e-expr)]))
                         (loop)]
                        [else
                         (eprintf "Got non-syntax: ~e" sexp)
                         (loop)]))
               #t)))
          (void))

        ;; set-directory : text -> void
        ;; sets the current-directory based on the file saved in the definitions-text
        (define/private (set-directory definitions-text)
          (define tmp-b (box #f))
          (define fn (send definitions-text get-filename tmp-b))
          (define dir (get-init-dir (and (not (unbox tmp-b)) fn)))
          (current-directory dir))

        ;; with-lock/edit-sequence : text (-> void) -> void
        ;; sets and restores some state of the definitions text
        ;; so that edits to the definitions text work out.
        (define/private (with-lock/edit-sequence definitions-text thnk)
          (let* ([locked? (send definitions-text is-locked?)])
            (send definitions-text begin-edit-sequence)
            (send definitions-text lock #f)
            (thnk)
            (send definitions-text end-edit-sequence)
            (send definitions-text lock locked?)))

        (define/private (expand+trace expr)
          (define (handle-macro-limit c)
            (define option
              (message-box/custom
               "Macro stepper"
               (string-append "Macro expansion has taken a suspiciously large number of steps.\n"
                              "\n"
                              "Click Stop to stop macro expansion and see the steps taken "
                              "so far, or click Continue to let it run a bit longer.")
               "Continue"
               "Stop"
               #f
               (get-top-level-window)))
            (case option
              ((2) (error "Macro expansion was stopped because it took too many steps."))
              (else (* 2 c))))
          (parameterize ((trace-macro-limit (pref:macro-step-limit))
                         (trace-limit-handler handle-macro-limit))
            (trace* expr)))

        ))

    ;; ============================================================

    ;; Catch modifications => obsolete macro stepper
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

        (define modified-since-macro-stepper? #f) ;; mutable
        (define/public (modified-since-macro-stepper ?)
          (set! modified-since-macro-stepper? ?))

        ;; don't pay attention to changes that occur on metadata.
        ;; this assumes that metadata changes cannot be nested.
        (define/augment (begin-metadata-changes)
          (set! metadata-changing-now? #t)
          (inner (void) begin-metadata-changes))

        (define/augment (end-metadata-changes)
          (set! metadata-changing-now? #f)
          (inner (void) end-metadata-changes))

        (define/private (notify-macro-stepper-of-change)
          (unless modified-since-macro-stepper?
            (set! modified-since-macro-stepper? #f)
            (let ([win (get-top-level-window)])
              ;; should only be #f when win is #f
              (when (is-a? win drracket:unit:frame<%>)
                (send (send win get-interactions-text)
                      obsolete-macro-stepper)))))

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

    ;; Catch reset => obsolete macro stepper
    (define (macro-debugger-interactions-text-mixin %)
      (class %
        (define current-stepper-director #f)
        (inherit get-top-level-window)
        (super-new)

        (define/override (reset-console)
          (obsolete-macro-stepper)
          (when current-stepper-director
            (send current-stepper-director shutdown)
            (set! current-stepper-director #f))
          (super reset-console))

        (define/public (obsolete-macro-stepper)
          (when current-stepper-director
            (send current-stepper-director add-obsoleted-warning)))

        (define/public (set-macro-stepper-director director)
          (set! current-stepper-director director))
        ))

    ;; Macro debugger code

    (drracket:get/extend:extend-unit-frame
     macro-debugger-unit-frame-mixin)
    (drracket:get/extend:extend-interactions-text 
     macro-debugger-interactions-text-mixin)
    (drracket:get/extend:extend-definitions-text
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

    (add-macro-stepper-key-bindings (drracket:rep:get-drs-bindings-keymap))
    ))
