#lang racket/unit

;; this module implements the UI side of the stepper; it 
;; opens a window, starts the stepper thread running, 
;; and handles the resulting calls to 'break'.

;; this module lies outside of the "testing boundary"
;; of through-tests; it is not tested automatically at all.

;; this version of the view-controller just collects the steps up front rather
;; than blocking until the user presses the "next" button.

(require racket/class
         racket/match
         racket/list
         drracket/tool
         mred
         string-constants
         racket/async-channel
         (prefix-in model: "model.rkt")
         (prefix-in x: "mred-extensions.rkt")
         "shared.rkt"
         "model-settings.rkt"
         "xml-sig.rkt"
         images/compile-time
         images/gui
         (for-syntax racket/base images/icons/control images/icons/style images/logos))


(import drracket:tool^ xml^ stepper-frame^)
(export view-controller^)

(define drracket-eventspace (current-eventspace))

(define (definitions-text->settings definitions-text)
  (send definitions-text get-next-settings))
  
;; the stored representation of a step
(define-struct step (text kind posns) #:transparent)

(define (show-about-dialog parent)
  (define dlg
    (new logo-about-dialog%
         [label "About the Stepper"]
         [parent parent]
         [bitmap (compiled-bitmap (stepper-logo))]
         [messages '("The Algebraic Stepper is formalized and proved correct in\n"
                     "\n"
                     "    John Clements, Matthew Flatt, Matthias Felleisen\n"
                     "    Modeling an Algebraic Stepper\n"
                     "    European Symposium on Programming, 2001\n")]))
  (send dlg show #t))

(define (go drracket-tab program-expander selection-start selection-end)
  
  ;; get the language-level:
  (define language-settings 
    (definitions-text->settings
      (send drracket-tab get-defs)))
  
  (define language-level 
    (drracket:language-configuration:language-settings-language 
     language-settings))
  
  (define simple-settings
    (drracket:language-configuration:language-settings-settings
     language-settings))
  
  ;; VALUE CONVERSION CODE:
  
  ;; render-to-string : TST -> string
  (define (render-to-string val)
    (let ([string-port (open-output-string)])
      (send language-level render-value 
            val simple-settings string-port)
      (get-output-string string-port)))
  
  ;; render-to-sexp : TST -> sexp
  (define (render-to-sexp val)
    (send language-level stepper:render-to-sexp
          val simple-settings language-level))
  
  ;; channel for incoming views
  (define view-channel (make-async-channel))
  
  ;; the first-step semaphore
  (define first-step-sema (make-semaphore 0))
  
  ;; the list of available views
  (define view-history null)
  
  ;; the number of available steps
  (define num-steps-available 0)
  
  ;; the view in the stepper window
  (define view #f)
  
  ;; wait for steps to show up on the channel.  
  ;; When they do, add them to the list.
  (define (start-listener-thread stepper-frame-eventspace)
    ;; as of 2012-06-20, I no longer believe there's any
    ;; need for this thread, as the queue-callback handles
    ;; the needed separation.
    (thread
     (lambda ()
       (let loop ()
         (define new-result (async-channel-get view-channel))
         (receive-result-from-target new-result)
         (loop)))))
  
  ;; handles an incoming result. Either adds it to the list of 
  ;; steps, or prompts user to see whether to continue running.
  (define (receive-result-from-target result)
    (cond [(runaway-process? result)
           (parameterize ([current-eventspace stepper-frame-eventspace])
             (queue-callback
              (lambda ()
                (when (confirm-running)
                  (semaphore-post (runaway-process-sema result)))
                (void))))]
          [else
           (define new-step (format-result result))
           (parameterize ([current-eventspace stepper-frame-eventspace])
             (queue-callback
              (lambda ()
                (set! view-history (append view-history (list new-step)))
                (set! num-steps-available (length view-history))
                ;; this is only necessary the first time, but it's cheap:
                (semaphore-post first-step-sema)
                (update-status-bar))))]))
    
  
  ;; find-later-step : given a predicate on history-entries, search through
  ;; the history for the first step that satisfies the predicate and whose 
  ;; number is greater than n (or -1 if n is #f), return # of step on success,
  ;; on failure return (list 'nomatch last-step) or (list 'nomatch/seen-final
  ;; last-step) if we went past the final step
  (define (find-later-step p n)
    (let* ([n-as-num (or n -1)])
      (let loop ([step 0] 
                 [remaining view-history]
                 [seen-final? #f])
        (cond [(null? remaining) 
               (cond [seen-final? (list `nomatch/seen-final (- step 1))]
                     [else (list `nomatch (- step 1))])]
              [(and (> step n-as-num) (p (car remaining))) step]
              [else (loop (+ step 1)
                          (cdr remaining)
                          (or seen-final? (finished-stepping-step? (car remaining))))]))))
  
  ;; find-earlier-step : like find-later-step, but searches backward from
  ;; the given step.
  (define (find-earlier-step p n)
    (unless (number? n)
      (error 'find-earlier-step 
             "can't find earlier step when no step is displayed."))
    (let* ([to-search (reverse (take view-history n))])
      (let loop ([step (- n 1)]
                 [remaining to-search])
        (cond [(null? remaining) `nomatch]
              [(p (car remaining)) step]
              [else (loop (- step 1) (cdr remaining))]))))
  
  
  ;; STEP PREDICATES:
  
  ;; is this an application step?
  (define (application-step? history-entry)
    (match history-entry
      [(struct step (text (or 'user-application 'finished-or-error) posns)) #t]
      [else #f]))
  
  ;; is this the finished-stepping step?
  (define (finished-stepping-step? history-entry)
    (match (step-kind history-entry)
      ['finished-or-error #t]
      [else #f]))
  
  ;; is this step on the selected expression?
  (define (selected-exp-step? history-entry)
    (ormap (span-overlap selection-start selection-end) (step-posns history-entry)))
    
  ;; build gui object:
  

  ;; next-of-specified-kind : starting at the current view, search forward for the
  ;; desired step or wait for it if not found
  (define (next-of-specified-kind right-kind? msg)
    (next-of-specified-kind/helper right-kind? view msg))
  
  ;; first-of-specified-kind : similar to next-of-specified-kind, but 
  ;; always start at zero
  (define (first-of-specified-kind right-kind? msg)
    (next-of-specified-kind/helper right-kind? #f msg))
  
  ;; next-of-specified-kind/helper : if the desired step 
  ;; is already in the list, display it; otherwise, give up.
  (define (next-of-specified-kind/helper right-kind? starting-step msg)
    (match (find-later-step right-kind? starting-step)
      [(? number? n)
       (update-view/existing n)]
      [(list `nomatch step)
       (message-box (string-constant stepper-no-such-step/title) msg)
       (when (>= num-steps-available 0)
         (update-view/existing step))]
      [(list `nomatch/seen-final step)
       (message-box (string-constant stepper-no-such-step/title) msg)
       (when (>= num-steps-available 0)
         (update-view/existing step))]))
  
  ;; prior-of-specified-kind: if the desired step is already in the list, display
  ;; it; otherwise, put up a dialog and jump to the first step.
  (define (prior-of-specified-kind right-kind? msg)
    (match (find-earlier-step right-kind? view)
      [(? number? found-step)
       (update-view/existing found-step)]
      [`nomatch
       (message-box (string-constant stepper-no-such-step/title) msg)
       (when (>= num-steps-available 0)
         (update-view/existing 0))]))
  
  ;; BUTTON/CHOICE BOX PROCEDURES
 
  
  ;; respond to a click on the "next" button
  (define (next)
    (next-of-specified-kind (lambda (x) #t) 
                            (string-constant stepper-no-later-step)))
  
  ;; previous : the action of the 'previous' button
  (define (previous)
    (prior-of-specified-kind (lambda (x) #t)
                             (string-constant stepper-no-earlier-step)))
  
  ;; respond to a click on the "Jump To..." choice
  (define (jump-to control event)
    ((second (list-ref pulldown-choices (send control get-selection)))))
  
  ;; jump-to-beginning : the action of the choice menu entry
  (define (jump-to-beginning)
    (first-of-specified-kind (lambda (x) #t)
                             ;; I don't believe this can fail...
                             "internal error 2010-01-10 21:48"))
  
  ;; jump-to-end : the action of the jump-to-end choice box option
  (define (jump-to-end)
    (first-of-specified-kind finished-stepping-step?
                             (string-constant stepper-no-last-step)))
  
  ;; jump-to-selected : the action of the jump to selected choice box option
  (define (jump-to-selected)
    (first-of-specified-kind selected-exp-step?
                             (string-constant stepper-no-selected-step)))
  
  ;; jump-to-next-application : the action of the jump to next application
  ;; choice box option
  (define (jump-to-next-application)
    (next-of-specified-kind application-step?
                            (string-constant stepper-no-later-application-step)))
  
  ;; jump-to-prior-application : the action of the "jump to prior application"
  ;; choice box option
  (define (jump-to-prior-application)
    (prior-of-specified-kind application-step?
                             (string-constant 
                              stepper-no-earlier-application-step)))
  
  
  ;; GUI ELEMENTS:
  (define s-frame
    (make-object stepper-frame% drracket-tab))
  
  (define top-panel
    (new horizontal-panel% [parent (send s-frame get-area-container)] [horiz-margin 5]
         ;[style '(border)]  ; for layout testing only
         [stretchable-width #t]
         [stretchable-height #f]))
  
  (define button-panel
    (new horizontal-panel% [parent top-panel] [alignment '(center top)]
         ;[style '(border)]  ; for layout testing only
         [stretchable-width #t]
         [stretchable-height #f]))
  
  (define logo-canvas
    (new (class bitmap-canvas%
           (super-new [parent top-panel] [bitmap (compiled-bitmap (stepper-logo 32))])
           (define/override (on-event evt)
             (when (eq? (send evt get-event-type) 'left-up)
               (show-about-dialog s-frame))))))
  
  (define prev-img (compiled-bitmap (step-back-icon run-icon-color (toolbar-icon-height))))
  (define previous-button (new button%
                               [label (list prev-img (string-constant stepper-previous) 'left)]
                               [parent button-panel]
                               [callback (λ (_1 _2) (previous))]
                               [enabled #f]))
  
  (define next-img (compiled-bitmap (step-icon run-icon-color (toolbar-icon-height))))
  (define next-button (new button%
                           [label (list next-img (string-constant stepper-next) 'right)]
                           [parent button-panel]
                           [callback (λ (_1 _2) (next))]
                           [enabled #f]))
  
  (define pulldown-choices
    `((,(string-constant stepper-jump-to-beginning)            ,jump-to-beginning)
      (,(string-constant stepper-jump-to-end)                  ,jump-to-end)
      (,(string-constant stepper-jump-to-selected)             ,jump-to-selected)
      (,(string-constant stepper-jump-to-next-application)     ,jump-to-next-application)
      (,(string-constant stepper-jump-to-previous-application) ,jump-to-prior-application)))
  
  (define jump-button (new choice%
                           [label (string-constant stepper-jump)]
                           [choices (map first pulldown-choices)]
                           [parent button-panel]
                           [callback jump-to]
                           [enabled #f]))
  
  (define canvas
    (make-object x:stepper-canvas% (send s-frame get-area-container)))
  
  ;; counting steps...
  (define status-text
    (new text%))
  
  (define status-canvas
    (new editor-canvas%
         [parent button-panel]
         [editor status-text]
         [stretchable-width #f]
         [style '(transparent no-border no-hscroll no-vscroll)]
         ;; some way to get the height of a line of text?
         [min-width 100]))

  
  ;; update-view/existing : set an existing step as the one shown in the
  ;; frame
  (define (update-view/existing new-view)
    (set! view new-view)
    (let ([e (step-text (list-ref view-history view))])
      (send e begin-edit-sequence)
      (send canvas set-editor e)
      (send e reset-width canvas)
      ;; why set the position within the step? I'm confused by this.--JBC
      (send e set-position (send e last-position))
      (send e end-edit-sequence))
    (update-status-bar))
  
  ;; set the status bar to the correct m/n text.
  (define (update-status-bar)
    (send status-text begin-edit-sequence)
    (send status-text lock #f)
    (send status-text delete 0 (send status-text last-position))
    ;; updated to yield 1-based step numbering rather than 0-based numbering.
    (send status-text insert 
          (format "~a/~a" (if view (+ 1 view) "none") (length view-history)))
    (send status-text lock #t)
    (send status-text end-edit-sequence))
  
  (define update-status-bar-semaphore (make-semaphore 1))
  
  (define (enable-all-buttons)
    (send previous-button enable #t)
    (send next-button enable #t)
    (send jump-button enable #t))

  
  (define (print-current-view item evt)
    (send (send canvas get-editor) print))
  
  ;; code for dealing with runaway processes:
  
  (define runaway-counter-limit 500)
  (define disable-runaway-counter #f)
  (define runaway-counter 0)

  ;; runs on the stepped-process side.
  ;; checks to see if the process has taken too
  ;; many steps. If so, send a message and block
  ;; for a response, then send the result. Otherwise,
  ;; just send the result.
  (define (deliver-result-to-gui result)
    (when (not disable-runaway-counter)
      (set! runaway-counter (+ runaway-counter 1)))
    (when (= runaway-counter runaway-counter-limit)
      (define runaway-semaphore (make-semaphore 0))
      (async-channel-put view-channel 
                         (runaway-process runaway-semaphore))
      ;; wait for a signal to continue running:
      (semaphore-wait runaway-semaphore))
    (async-channel-put view-channel result))
  
  (define keep-running-message
    (string-append 
     "The program running in the stepper has taken a whole bunch of steps. "
     "Do you want to continue running it for now, halt, or let it run "
     "without asking again?"))
  
  (define (confirm-running)
    (define message-box-result
      (message-box/custom
       "Keep Running Program?"
       keep-running-message
       "Continue for now"
       "Halt"
       "Continue uninterrupted"
       #f ;; use the stepper window instead?
       '(stop disallow-close default=1)
       ))
    (match message-box-result
      ;; continue-for-now:
      [1 (set! runaway-counter 0)
         #t]
      ;; halt:
      [2 #f]
      ;; continue-forever:
      [3 (set! runaway-counter 0)
         (set! disable-runaway-counter #t)
         #t]))
  
  

  ;; translates a result into a step
  ;; format-result : step-result -> step?
  (define (format-result result)
    (match result
      [(struct before-after-result (pre-exps post-exps kind pre-src post-src))
       (make-step (new x:stepper-text% 
                       [left-side pre-exps]
                       [right-side post-exps]
                       [show-inexactness? 
                        (send language-level stepper:show-inexactness?)])
                  kind 
                  (list pre-src post-src))]
      [(struct before-error-result (pre-exps err-msg pre-src))
       (make-step (new x:stepper-text%
                       [left-side pre-exps] 
                       [right-side err-msg]
                       [show-inexactness?
                        (send language-level stepper:show-inexactness?)])
                  'finished-or-error 
                  (list pre-src))]
      [(struct error-result (err-msg))
       (make-step (new x:stepper-text% 
                       [left-side null]
                       [right-side err-msg]
                       [show-inexactness?
                        (send language-level stepper:show-inexactness?)]) 
                  'finished-or-error 
                  (list))]
      [(struct finished-stepping ())
       (make-step x:finished-text 'finished-or-error (list))]))
  
  ;; program-expander-prime : wrap the program-expander for a couple of reasons:
  ;; 1) we need to capture the custodian as the thread starts up:
  ;; ok, it was just one.
  ;; 
  (define (program-expander-prime init iter)
    (program-expander
     (lambda args
       (send s-frame set-custodian! (current-custodian))
       (apply init args))
     iter))
  
  ;; CONFIGURE GUI ELEMENTS
  (send s-frame set-printing-proc print-current-view)
  (send canvas stretchable-height #t)
  (send (send s-frame edit-menu:get-undo-item) enable #f)
  (send (send s-frame edit-menu:get-redo-item) enable #f)
  
  (define stepper-frame-eventspace (send s-frame get-eventspace))
  ;; START THE MODEL
  (start-listener-thread stepper-frame-eventspace)
  (model:go
   program-expander-prime 
   ;; what do do with the results:
   deliver-result-to-gui
   (get-render-settings render-to-string
                        render-to-sexp 
                        (send language-level stepper:enable-let-lifting?)
			(send language-level stepper:show-consumed-and/or-clauses?)
                        (send language-level stepper:show-lambdas-as-lambdas?)))
  
  (send s-frame show #t)
  
  ;; turn on the buttons and display the first step when it shows up:
  (thread
   (lambda ()
     (semaphore-wait first-step-sema)
     (parameterize
         ([current-eventspace stepper-frame-eventspace])
       (queue-callback
        (lambda ()
          (jump-to-beginning)
          (enable-all-buttons))))))
  
  s-frame)



;; UTILITY FUNCTIONS:

;; span-overlap : number number -> posn-info -> boolean
;; return true if the selection is of zero length and precedes a char of the 
;; stepping expression, *or* if the selection has positive overlap with the 
;; stepping expression.
(define ((span-overlap selection-start selection-end) source-posn-info)
  (match source-posn-info
    [#f #f]
    [(struct model:posn-info (posn span))
     (let ([end (+ posn span)])
       (and posn
            ;; you can *almost* combine these two, but not quite.
            (cond [(= selection-start selection-end)
                   (and (<= posn selection-start) (< selection-start end))]
                  [else 
                   (let ([overlap-begin (max selection-start posn)]
                         ;; nb: we don't want zero-length overlaps at the end.
                         ;; compensate by subtracting one from the end of the 
                         ;; current expression.
                         [overlap-end (min selection-end end)])
                     ;; #t if there's positive overlap:
                     (< overlap-begin overlap-end))])))]))

;; a few unit tests.  Use them if changing span-overlap.
;; ...oops, can't use module+ inside of a unit.
#;(module+ test
  (require rackunit)
  ;; zero-length selection cases:
  (check-equal? ((span-overlap 13 13) (model:make-posn-info 14 4)) #f)
  (check-equal? ((span-overlap 14 14) (model:make-posn-info 14 4)) #t)
  (check-equal? ((span-overlap 18 18) (model:make-posn-info 14 4)) #f)
  ;; nonzero-length selection cases:
  (check-equal? ((span-overlap 13 14) (model:make-posn-info 14 4)) #f)
  (check-equal? ((span-overlap 13 15) (model:make-posn-info 14 4)) #t)
  (check-equal? ((span-overlap 13 23) (model:make-posn-info 14 4)) #t)
  (check-equal? ((span-overlap 16 17) (model:make-posn-info 14 4)) #t)
  (check-equal? ((span-overlap 16 24)  (model:make-posn-info 14 4)) #t)
  (check-equal? ((span-overlap 18 21)  (model:make-posn-info 14 4)) #f))
