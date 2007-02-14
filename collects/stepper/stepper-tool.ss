(module stepper-tool mzscheme

  (require (lib "contract.ss")
           (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "pconvert.ss")
           (lib "string-constant.ss" "string-constants")
           (lib "async-channel.ss")
           (prefix frame: (lib "framework.ss" "framework"))
           (lib "unit.ss")
           (lib "class.ss")
           (lib "list.ss")
           (prefix model: "private/model.ss")
           "private/my-macros.ss"
           (prefix x: "private/mred-extensions.ss")
           "private/shared.ss"
           "private/model-settings.ss"
           "stepper-language-interface.ss"
           "xml-sig.ss")

  ;; hidden invariant: this list should be a sublist of the language-level
  ;; dialog (i.e., same order):
  (define stepper-works-for
    (list (string-constant beginning-student)
          (string-constant beginning-student/abbrev)
          (string-constant intermediate-student)
          (string-constant intermediate-student/lambda)
          #;(string-constant advanced-student)
          ))

  (provide stepper-tool@)

  (define-unit stepper-tool@
    (import drscheme:tool^ xml^)
    (export drscheme:tool-exports^)

      ;; tool magic here:
      (define (phase1)
        
        ;; experiment with extending the language... parameter-like fields for stepper parameters
        (drscheme:language:extend-language-interface
         stepper-language<%>
         (lambda (superclass)
           (class* superclass (stepper-language<%>)
                   (public stepper:enable-let-lifting?)
                   (define (stepper:enable-let-lifting?) #f)
                   (super-instantiate ())))))
      
      (define (phase2) (void))

      ;; this should be a preference
      (define stepper-initial-width 500)
      (define stepper-initial-height 500)

      (define drscheme-eventspace (current-eventspace))

      (define (extract-language-level definitions-text)
        (settings->language-level (definitions-text->settings definitions-text)))
      
      (define (definitions-text->settings definitions-text)
        (send definitions-text get-next-settings))
      
      (define (settings->language-level settings)
        (drscheme:language-configuration:language-settings-language settings))
      
      (define (stepper-works-for? language-level-name)
        (or (member language-level-name stepper-works-for)
            (getenv "PLTSTEPPERUNSAFE")))

      ;; the stepper's frame:
      (define stepper-frame%
        (class (drscheme:frame:basics-mixin
                (frame:frame:standard-menus-mixin frame:frame:basic%))

          (init-field drscheme-frame)

          ;; PRINTING-PROC
          ;; I frankly don't think that printing (i.e., to a printer) works
          ;; correctly. 2005-07-01, JBC
          (public set-printing-proc)

          (define (set-printing-proc proc)
            (set! printing-proc proc))

          (define (printing-proc item evt)
            (message-box "error?" "shouldn't be called"))

          (define/private (file-menu:print a b) (printing-proc a b))

          ;; MENUS

          (define/override (edit-menu:between-find-and-preferences edit-menu)
            (void))
          (define/override (edit-menu:between-select-all-and-find edit-menu)
            (void))
          (define/override (file-menu:between-save-as-and-print file-menu)
            (void))

          ;; CUSTODIANS
          ;; The custodian is used to halt the stepped computation when the
          ;; stepper window closes.  The custodian is captured when the stepped
          ;; computation starts.

          (define custodian #f)
          (define/public (set-custodian! cust)
            (set! custodian cust))
          (define/augment (on-close)
            (when custodian
              (custodian-shutdown-all custodian))
            (send drscheme-frame on-stepper-close)
            (inner (void) on-close))

          ;; WARNING BOXES:

          (define program-changed-warning-str
            (string-constant stepper-program-has-changed))
          (define window-closed-warning-str
            (string-constant stepper-program-window-closed))

          (define warning-message-visible-already #f)
          (define/private (add-warning-message warning-str)
            (let ([warning-msg (new x:stepper-warning%
                                 [warning-str warning-str]
                                 [parent (get-area-container)])])
              (send (get-area-container)
                    change-children
                    (if warning-message-visible-already
                      (lambda (l)
                        (list (car l) warning-msg (caddr l)))
                      (lambda (l)
                        (list (car l) warning-msg (cadr l)))))
              (set! warning-message-visible-already #t)))

          (inherit get-area-container)
          (define program-change-already-warned? #f)
          (define/public (original-program-changed)
            (unless program-change-already-warned?
              (set! program-change-already-warned? #t)
              (add-warning-message program-changed-warning-str)))

          (define/public (original-program-gone)
            (add-warning-message window-closed-warning-str))

          (super-new [label "Stepper"] [parent #f]
                     [width stepper-initial-width]
                     [height stepper-initial-height])))

      ;; view-controller-go: called when the stepper starts; starts the
      ;;                     stepper's view&controller
      ;; drscheme-frame : the drscheme frame which is starting the stepper
      ;; program-expander : see "model.ss" for the contract on a
      ;;                    program-expander
      ;;  -> returns the new frame%

      (define (view-controller-go drscheme-frame program-expander)

        ;; get the language-level name:
        (define language-settings (definitions-text->settings (send drscheme-frame get-definitions-text)))
        (define language-level
          (settings->language-level language-settings))
        (define language-level-name
          (language-level->name language-level))

        ;; VALUE CONVERSION CODE:

        (define simple-settings
          (drscheme:language-configuration:language-settings-settings
           language-settings))

        ;; render-to-string : TST -> string
        (define (render-to-string val)
          (let ([string-port (open-output-string)])
            (send language-level render-value val simple-settings string-port)
            (get-output-string string-port)))

        ;; WE REALLY WANT TO GET RID OF THIS STUFF (2005-07-01, JBC)

        ;; make-print-convert-hook:
        ;;   simple-settings -> (TST (TST -> TST) (TST -> TST) -> TST)
        ;; this code copied from various locations in language.ss and rep.ss
        (define (make-print-convert-hook simple-settings)
          (lambda (exp basic-convert sub-convert)
            (cond
              [(is-a? exp snip%)
               (send exp copy)]
              #;
              [((drscheme:rep:use-number-snip) exp)
               (let ([number-snip-type
                      (drscheme:language:simple-settings-fraction-style
                       simple-settings)])
                 (cond
                   [(eq? number-snip-type 'repeating-decimal)
                    (drscheme:number-snip:make-repeating-decimal-snip exp #f)]
                   [(eq? number-snip-type 'repeating-decimal-e)
                    (drscheme:number-snip:make-repeating-decimal-snip exp #t)]
                   [(eq? number-snip-type 'mixed-fraction)
                    (drscheme:number-snip:make-fraction-snip exp #f)]
                   [(eq? number-snip-type 'mixed-fraction-e)
                    (drscheme:number-snip:make-fraction-snip exp #t)]
                   [else
                    (error 'which-number-snip
                           "expected either 'repeating-decimal, 'repeating-decimal-e, 'mixed-fraction, or 'mixed-fraction-e got : ~e"
                           number-snip-type)]))]
              [else (basic-convert exp)])))

        ;; render-to-sexp : TST -> sexp
        (define (render-to-sexp val)
          (cond
            [(string=? language-level-name "ACL2 Beginner (beta 8)")
             (simple-module-based-language-convert-value val simple-settings)]
            [else (parameterize ([current-print-convert-hook
                                  (make-print-convert-hook simple-settings)])
                    (set-print-settings
                     language-level
                     simple-settings
                     (lambda ()
                       (simple-module-based-language-convert-value
                        val simple-settings))))]))

        ;; channel for incoming views
        (define view-channel (make-async-channel))

        ;; the semaphore associated with the view at the end of the
        ;; view-history note that because these are fresh semaphores for every
        ;; step, posting to a semaphore multiple times is no problem.
        (define release-for-next-step #f)

        ;; the list of available views
        (define view-history null)

        ;; the view in the stepper window
        (define view 0)

        ;; whether the stepper is waiting for a new view to become available
        ;; (initially 'waiting-for-any-step)
        ;; possible values: #f, 'waiting-for-any-step, 'waiting-for-application, 'waiting-for-end
        (define stepper-is-waiting? 'waiting-for-any-step)

        ;; hand-off-and-block : (-> text%? boolean? void?)
        ;; hand-off-and-block generates a new semaphore, hands off a thunk to
        ;; drscheme's eventspace, and blocks on the new semaphore.  The thunk
        ;; adds the text% to the waiting queue, and checks to see if the
        ;; stepper is waiting for a new step.  If so, takes that new text% out
        ;; of the queue and puts it on the list of available ones.  If the
        ;; stepper is waiting for a new step, it checks to see whether this is
        ;; of the kind that the stepper wants.  If so, display it.  otherwise,
        ;; release the stepped program to continue execution.
        (define (hand-off-and-block step-text step-kind)
          (let ([new-semaphore (make-semaphore)])
            (run-on-drscheme-side
             (lambda ()
               (async-channel-put view-channel
                                  (list step-text new-semaphore step-kind))
               (when stepper-is-waiting?
                 (let ([try-get (async-channel-try-get view-channel)])
                   (unless try-get
                     (error
                      'check-for-stepper-waiting
                      "queue is empty, even though a step was just added"))
                   (add-view-triple try-get)
                   (if (right-kind-of-step? (caddr try-get))
                     ;; got the desired step; show the user:
                     (begin (set! stepper-is-waiting? #f)
                            (update-view/existing (- (length view-history) 1)))
                     ;; nope, keep running:
                     (begin (en/dis-able-buttons)
                            (semaphore-post new-semaphore)))))))
            (semaphore-wait new-semaphore)))

        ;; run-on-drscheme-side : runs a thunk in the drscheme eventspace.
        ;; Passed to 'go' so that display-break-stuff can work.  This would be
        ;; cleaner with two-way provides.
        (define (run-on-drscheme-side thunk)
          (parameterize ([current-eventspace drscheme-eventspace])
            (queue-callback thunk)))

        ;; right-kind-of-step? : (boolean? . -> . boolean?)
        ;; is this step the kind of step that the gui is waiting for?
        (define (right-kind-of-step? step-kind)
          (case stepper-is-waiting?
            [(waiting-for-any-step) #t]
            [(waiting-for-application)
             (or (eq? step-kind 'user-application)
                 (eq? step-kind 'finished-stepping))]
            [(waiting-for-end)
             (or (eq? step-kind 'finished-stepping))]
            [(#f) (error 'right-kind-of-step
                         "this code should be unreachable with stepper-is-waiting? set to #f")]
            [else (error 'right-kind-of-step
                         "unknown value for stepper-is-waiting?: ~a"
                         stepper-is-waiting?)]))

        ;; add-view-triple : set the release-semaphore to be the new one, add
        ;; the view to the list.
        (define (add-view-triple view-triple)
          (set! release-for-next-step (cadr view-triple))
          (set! view-history (append view-history
                                     (list (list (car view-triple)
                                                 (caddr view-triple))))))

        ;; find-later-step : given a predicate on history-entries, search through
        ;; the history for the first step that satisfies the predicate and whose 
        ;; number is greater than n
        (define (find-later-step p n)
          (let loop ([step 0] 
                     [remaining view-history])
            (cond [(null? remaining) #f]
                  [(and (> step n) (p (car remaining))) step]
                  [else (loop (+ step 1) (cdr remaining))])))
        
        (define (find-later-application-step n)
          (find-later-step application-step? n))
        
        (define (find-later-finished-stepping-step n)
          (find-later-step finished-stepping-step? n))
        
        (define (find-later-any-step n)
          (find-later-step (lambda (x) #t) n))
        
        ;; is this an application step?
        (define (application-step? history-entry)
          (case (cadr history-entry)
            [(user-application finished-stepping) #t]
            [else #f]))
        
        ;; is this the finished-stepping step?
        (define (finished-stepping-step? history-entry)
          (case (cadr history-entry)
            [(finished-stepping) #t]
            [else #f]))

        ;; build gui object:

        ;; home : the action of the 'home' button
        (define (home)
          (set! stepper-is-waiting? #f)
          (update-view/existing 0))

        ;; next-of-specified-kind: if the desired step is already in the list, display
        ;; it; otherwise, wait for it.
        (define (next-of-specified-kind find-step right-kind? wait-for-it-flag)
          (set! stepper-is-waiting? #f)
          (let ([found-step (find-step view)])
            (if found-step
              (update-view/existing found-step)
              (begin
                ;; each step has its own semaphore, so releasing one twice is
                ;; no problem.
                (semaphore-post release-for-next-step)
                (when stepper-is-waiting?
                  (error 'try-to-get-view
                         "try-to-get-view should not be reachable when already waiting for new step"))
                (let ([try-get (async-channel-try-get view-channel)])
                  (when try-get
                    (add-view-triple try-get))
                  (if (and try-get (right-kind? (list-ref view-history (+ view 1))))
                      (update-view/existing (+ view 1))
                      (begin
                          (set! stepper-is-waiting? wait-for-it-flag)
                          (en/dis-able-buttons))))))))
        
        ;; respond to a click on the "next" button
        (define (next)
          (next-of-specified-kind find-later-any-step
                                  (lambda (x) #t)
                                  'waiting-for-any-step))
        
        ;; respond to a click on the "next application" button
        (define (next-application)
          (next-of-specified-kind find-later-application-step 
                                  application-step?
                                  'waiting-for-application))
        
        ;; respond to a click on the "jump to end" button
        (define (jump-to-end)
          (next-of-specified-kind find-later-finished-stepping-step
                                  finished-stepping-step?
                                  'waiting-for-end))

        ;; previous : the action of the 'previous' button
        (define (previous)
          (set! stepper-is-waiting? #f)
          (when (= view 0)
            (error 'previous-application
                   "previous-step button should not be enabled in view zero."))
          (update-view/existing (- view 1)))

        ;; previous-application : the action of the 'previous-application'
        ;; button
        (define (previous-application)
          (set! stepper-is-waiting? #f)
          (when (= view 0)
            (error 'previous-application
                   "previous-application button should not be enabled in view zero."))
          (let loop ([new-view (- view 1)])
            (cond [(= new-view 0)
                   (update-view/existing new-view)]
                  [(application-step? (list-ref view-history new-view))
                   (update-view/existing new-view)]
                  [else (loop (sub1 new-view))])))

        ;; GUI ELEMENTS:
        (define s-frame
          (make-object stepper-frame% drscheme-frame))
        (define button-panel
          (make-object horizontal-panel% (send s-frame get-area-container)))
        (define (add-button name fun)
          (make-object button% name button-panel (lambda (_1 _2) (fun))))
        
        (define home-button                 (add-button (string-constant stepper-home) home))
        (define previous-application-button (add-button (string-constant stepper-previous-application) previous-application))
        (define previous-button             (add-button (string-constant stepper-previous) previous))
        (define next-button                 (add-button (string-constant stepper-next) next))
        (define next-application-button     (add-button (string-constant stepper-next-application) next-application))
        (define jump-to-end-button          (add-button (string-constant stepper-jump-to-end) jump-to-end))
        
        (define canvas
          (make-object x:stepper-canvas% (send s-frame get-area-container)))

        ;; update-view/existing : set an existing step as the one shown in the
        ;; frame
        (define (update-view/existing new-view)
          (set! view new-view)
          (let ([e (car (list-ref view-history view))])
            (send e begin-edit-sequence)
            (send canvas set-editor e)
            (send e reset-width canvas)
            (send e set-position (send e last-position))
            (send e end-edit-sequence))
          (en/dis-able-buttons))

        ;; en/dis-able-buttons : set enable & disable the stepper buttons,
        ;; based on view-controller state
        (define (en/dis-able-buttons)
          (let* ([can-go-back? (> view 0)])
            (send previous-button enable can-go-back?)
            (send previous-application-button enable can-go-back?)
            (send home-button enable can-go-back?)
            (send next-button
                  enable (or (find-later-any-step view)
                             (not stepper-is-waiting?)))
            (send next-application-button
                  enable (or (find-later-application-step view)
                             (not stepper-is-waiting?)))
            (send jump-to-end-button
                  enable (or (find-later-finished-stepping-step view)
                             (not stepper-is-waiting?)))))

        (define (print-current-view item evt)
          (send (send canvas get-editor) print))

        ;; receive-result takes a result from the model and renders it
        ;; on-screen. Runs on the user thread.
        ;; : (step-result -> void)
        (define (receive-result result)
          (let ([step-text
                 (cond [(before-after-result? result)
                        (new x:stepper-text%
                          [left-side (before-after-result-pre-exps result)]
                          [right-side (before-after-result-post-exps result)])]
                       [(before-error-result? result)
                        (new x:stepper-text%
                          [left-side (before-error-result-pre-exps result)]
                          [right-side (before-error-result-err-msg result)])]
                       [(error-result? result)
                        (new x:stepper-text%
                          [left-side null]
                          [right-side (error-result-err-msg result)])]
                       [(finished-stepping? result)
                        x:finished-text])]
                [step-kind (or (and (before-after-result? result)
                                    (before-after-result-kind result))
                               (and (finished-stepping? result)
                                    'finished-stepping))])
            (hand-off-and-block step-text step-kind)))

        ;; need to capture the custodian as the thread starts up:
        (define (program-expander-prime init iter)
          (program-expander
           (lambda args
             (send s-frame set-custodian! (current-custodian))
             (apply init args))
           iter))

        ;; CONFIGURE GUI ELEMENTS
        (send s-frame set-printing-proc print-current-view)
        (send button-panel stretchable-width #f)
        (send button-panel stretchable-height #f)
        (send canvas stretchable-height #t)
        (en/dis-able-buttons)
        (send (send s-frame edit-menu:get-undo-item) enable #f)
        (send (send s-frame edit-menu:get-redo-item) enable #f)

        ;; START THE MODEL
        (model:go
         program-expander-prime receive-result
         (get-render-settings render-to-string render-to-sexp 
                              (send language-level stepper:enable-let-lifting?))
         (not (member language-level-name
                      (list (string-constant intermediate-student/lambda)
                            (string-constant advanced-student))))
         language-level
         run-on-drscheme-side)
        (send s-frame show #t)

        s-frame)

      ;; stepper-unit-frame<%> : the interface that the extended drscheme frame
      ;; fulfils
      (define stepper-unit-frame<%>
        (interface ()
          get-stepper-frame
          on-stepper-close))

      ;; stepper-unit-frame-mixin : the mixin that is applied to the drscheme
      ;; frame to interact with a possible stepper window
      (define (stepper-unit-frame-mixin super%)
        (class* super% (stepper-unit-frame<%>)

          (inherit get-button-panel get-interactions-text get-definitions-text)

          (define stepper-frame #f)
          (define/public (on-stepper-close)
            (set! stepper-frame #f))
          (define/public (get-stepper-frame) stepper-frame)

          (super-new)

          ;; program-expander : produces expanded expressions from the
          ;; definitions window one at a time and calls 'iter' on each one
          (define (program-expander init iter)
            (let* ([lang-settings
                    (send (get-definitions-text) get-next-settings)]
                   [lang (drscheme:language-configuration:language-settings-language lang-settings)]
                   [settings (drscheme:language-configuration:language-settings-settings lang-settings)])
              (drscheme:eval:expand-program
               (drscheme:language:make-text/pos
                (get-definitions-text)
                0
                (send (get-definitions-text) last-position))
               lang-settings
               #f
               (lambda ()
                 (init)
                 (error-value->string-handler
                  (lambda (val len)
                    (let ([sp (open-output-string)])
                      (send lang render-value val settings sp)
                      (let ([str (get-output-string sp)])
                        (if ((string-length str) . <= . len)
                          str
                          (string-append (substring str 0 (max 0 (- len 3)))
                                         "...")))))))
               void ; kill
               iter)))

          ;; STEPPER BUTTON

          (define/public (get-stepper-button) stepper-button)
          (define stepper-button
            (make-object button%
              (x:stepper-bitmap this)
              (make-object vertical-pane% (get-button-panel))
              (lambda (button evt)
                (if stepper-frame
                  (send stepper-frame show #t)
                  (let ([language-level-name
                         (language-level->name
                          (extract-language-level (get-definitions-text)))])
                    (if (stepper-works-for? language-level-name)
                      (set! stepper-frame
                            (view-controller-go this program-expander))
                      (message-box
                       (string-constant stepper-name)
                       (format (string-constant stepper-language-level-message)
                               language-level-name
                               (car stepper-works-for)
                               (car (reverse stepper-works-for))))))))))

          (define/augment (enable-evaluation)
            (send stepper-button enable #t)
            (inner (void) enable-evaluation))

          (define/augment (disable-evaluation)
            (send stepper-button enable #f)
            (inner (void) disable-evaluation))

          (define/augment (on-close)
            (when stepper-frame
              (send stepper-frame original-program-gone))
            (inner (void) on-close))

          (define/augment (on-tab-change old new)
            (check-current-language-for-stepper)
            (inner (void) on-tab-change old new))

          (define/public (check-current-language-for-stepper)
            (if (stepper-works-for?
                 (language-level->name 
                  (extract-language-level (get-definitions-text))))
                (unless (send stepper-button is-shown?)
                  (send (send stepper-button get-parent)
                        add-child stepper-button))
                (when (send stepper-button is-shown?)
                  (send (send stepper-button get-parent)
                        delete-child stepper-button))))

          ;; add the stepper button to the button panel:
          (let ([p (send stepper-button get-parent)])
            (send (get-button-panel) change-children (lx (cons p (remq p _)))))
          
          ;; hide stepper button if it's not supported for the initial language:
          (check-current-language-for-stepper)))

      ;; stepper-definitions-text-mixin : a mixin for the definitions text that
      ;; alerts thet stepper when the definitions text is altered or destroyed
      (define (stepper-definitions-text-mixin %)
        (class %

          (inherit get-top-level-window)
          (define/private (notify-stepper-frame-of-change)
            (let ([win (get-top-level-window)])
              ;; should only be #f when win is #f
              (when (is-a? win stepper-unit-frame<%>)
                (let ([stepper-window (send win get-stepper-frame)])
                  (when stepper-window
                    (send stepper-window original-program-changed))))))

          (define/augment (on-insert x y)
            (notify-stepper-frame-of-change)
            (inner (void) on-insert x y))

          (define/augment (on-delete x y)
            (notify-stepper-frame-of-change)
            (inner (void) on-delete x y))

          (define/augment (after-set-next-settings s)
            (let ([tlw (get-top-level-window)])
              (when tlw
                (send tlw check-current-language-for-stepper)))
            (inner (void) after-set-next-settings s))

          (super-new)))

      ;; COPIED FROM drscheme/private/language.ss
      ;; simple-module-based-language-convert-value : TST settings -> TST
      (define (simple-module-based-language-convert-value value settings)
        (define ((leave-snips-alone-hook sh) expr basic-convert sub-convert)
          (if (or (is-a? expr snip%)
                  ;; FIXME: internal in language.ss (to-snip-value? expr)
                  )
            expr
            (sh expr basic-convert sub-convert)))
        ;; mflatt: MINOR HACK - work around temporary
        ;;         print-convert problems
        (define (stepper-print-convert v)
          (or (and (procedure? v) (object-name v))
              (print-convert v)))

        (case (drscheme:language:simple-settings-printing-style settings)
          [(write) value]
          [(current-print) value]
          [(constructor)
           (parameterize
               ([constructor-style-printing #t]
                [show-sharing
                 (drscheme:language:simple-settings-show-sharing settings)]
                [current-print-convert-hook
                 (leave-snips-alone-hook (current-print-convert-hook))])
             (stepper-print-convert value))]
          [(quasiquote)
           (parameterize
               ([constructor-style-printing #f]
                [show-sharing
                 (drscheme:language:simple-settings-show-sharing settings)]
                [current-print-convert-hook
                 (leave-snips-alone-hook (current-print-convert-hook))])
             (stepper-print-convert value))]
          [else (error "Internal stepper error: time to resync with simple-module-based-language-convert-value")]))

      ;; set-print-settings ; settings ( -> TST) -> TST
      (define (set-print-settings language simple-settings thunk)
        (if (method-in-interface? 'set-printing-parameters (object-interface language))
          (send language set-printing-parameters simple-settings thunk)
          ;; assume that the current print-convert context is fine
          ;; (error 'stepper-tool "language object does not contain set-printing-parameters method")
          (thunk)))

      ;; apply the mixins dynamically to the drscheme unit frame and
      ;; definitions text:
      (drscheme:get/extend:extend-unit-frame stepper-unit-frame-mixin)
      (drscheme:get/extend:extend-definitions-text
       stepper-definitions-text-mixin)
      
      ))
