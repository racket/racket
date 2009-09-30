#lang scheme/unit

(require scheme/class
         scheme/match
         scheme/list
         drscheme/tool
         mred
         string-constants
         scheme/async-channel
         (prefix-in model: "private/model.ss")
         (prefix-in x: "private/mred-extensions.ss")
         "private/shared.ss"
         "private/model-settings.ss"
         "xml-sig.ss"
         (only-in scheme/pretty pretty-print-show-inexactness))


(import drscheme:tool^ xml^ stepper-frame^)
(export view-controller^)

(define drscheme-eventspace (current-eventspace))

(define (definitions-text->settings definitions-text)
  (send definitions-text get-next-settings))
  
;; the stored representation of a step
(define-struct step (text kind posns) #:transparent)

(define (go drscheme-frame program-expander selection-start selection-end)
  
  ;; get the language-level:
  (define language-settings (definitions-text->settings (send drscheme-frame get-definitions-text)))
  (define language-level (drscheme:language-configuration:language-settings-language language-settings))
  (define simple-settings (drscheme:language-configuration:language-settings-settings language-settings))
  
  ;; VALUE CONVERSION CODE:
  
  ;; render-to-string : TST -> string
  (define (render-to-string val)
    (let ([string-port (open-output-string)])
      (send language-level render-value val simple-settings string-port)
      (get-output-string string-port)))
  
  ;; render-to-sexp : TST -> sexp
  (define (render-to-sexp val)
    (send language-level stepper:render-to-sexp val simple-settings language-level))
  
  ;; channel for incoming views
  (define view-channel (make-async-channel))
  
  ;; the semaphore associated with the view at the end of the
  ;; view-history note that because these are fresh semaphores for every
  ;; step, posting to a semaphore multiple times is no problem.
  (define release-for-next-step #f)
  
  ;; the list of available views
  (define view-history null)
  
  ;; the view in the stepper window
  (define view #f)
  
  ;; whether the stepper is waiting for a new view to become available
  ;; possible values: #f, or a predicate on steps.
  (define stepper-is-waiting? (lambda (x) #t))
  
  ;; hand-off-and-block : (-> text%? any (listof (or/c posn-info? false?)) void?)
  ;; hand-off-and-block generates a new semaphore, hands off a thunk to
  ;; drscheme's eventspace, and blocks on the new semaphore.  The thunk
  ;; adds the text% to the waiting queue, and checks to see if the
  ;; stepper is waiting for a new step.  If so, takes that new text% out
  ;; of the queue and puts it on the list of available ones.  If the
  ;; stepper is waiting for a new step, it checks to see whether this is
  ;; of the kind that the stepper wants.  If so, display it.  otherwise,
  ;; release the stepped program to continue execution.
  (define (hand-off-and-block text kind posns)
    (let ([new-semaphore (make-semaphore)])
      (run-on-drscheme-side
       (lambda ()
         (async-channel-put view-channel
                            (list (make-step text kind posns) new-semaphore))
         (match stepper-is-waiting?
           [#f (void)]
           [step-pred 
            (match (async-channel-try-get view-channel)
             [#f (error
                  'check-for-stepper-waiting
                  "queue is empty, even though a step was just added")]
             [(list new-step semaphore)
              (add-view-step new-step semaphore)
              (cond [(step-pred new-step)
                     ;; got the desired step; show the user:
                     (begin (set! stepper-is-waiting? #f)
                            (update-view/existing (- (length view-history) 1)))]
                    [else 
                     ;; nope, keep running:
                     (begin (if (finished-stepping-step? new-step)
                                (begin (message-box (string-constant stepper-no-such-step/title)
                                                    (string-constant stepper-out-of-steps))
                                       (update-view/existing (- (length view-history) 1)))
                                (semaphore-post semaphore)))])])])))
      (semaphore-wait new-semaphore)))
  
  ;; run-on-drscheme-side : runs a thunk in the drscheme eventspace.
  ;; Passed to 'go' so that display-break-stuff can work.  This would be
  ;; cleaner with two-way provides.
  (define (run-on-drscheme-side thunk)
    (parameterize ([current-eventspace drscheme-eventspace])
      (queue-callback thunk)))
  
  
  ;; add-view-triple : set the release-semaphore to be the new one, add
  ;; the view to the list.
  (define (add-view-step view-step semaphore)
    (set! release-for-next-step semaphore)
    (set! view-history (append view-history (list view-step)))
    (update-status-bar))
  
  ;; find-later-step : given a predicate on history-entries, search through
  ;; the history for the first step that satisfies the predicate and whose 
  ;; number is greater than n (or -1 if n is #f), return # of step on success,
  ;; on failure return 'nomatch or 'nomatch/seen-final if we went past the final step
  (define (find-later-step p n)
    (let* ([n-as-num (or n -1)])
      (let loop ([step 0] 
                 [remaining view-history]
                 [seen-final? #f])
        (cond [(null? remaining) (cond [seen-final? 'nomatch/seen-final]
                                       [else 'nomatch])]
              [(and (> step n-as-num) (p (car remaining))) step]
              [else (loop (+ step 1) 
                          (cdr remaining)
                          (or seen-final? (finished-stepping-step? (car remaining))))]))))
  
  ;; find-later-step/boolean : similar, but just return #f or #t.
  (define (find-later-step/boolean p n)
    (number? (find-later-step p n)))
  
  ;; find-earlier-step : like find-later-step, but searches backward from
  ;; the given step.
  (define (find-earlier-step p n)
    (unless (number? n)
      (error 'find-earlier-step "can't find earlier step when no step is displayed."))
    (let* ([to-search (reverse (take view-history n))])
      (let loop ([step (- n 1)]
                 [remaining to-search])
        (cond [(null? remaining) #f]
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
  (define (next-of-specified-kind right-kind?)
    (next-of-specified-kind/helper right-kind? view))
  
  ;; first-of-specified-kind : similar to next-of-specified-kind, but always start at zero
  (define (first-of-specified-kind right-kind?)
    (next-of-specified-kind/helper right-kind? #f))
  
  ;; next-of-specified-kind/helper : if the desired step is already in the list, display
  ;; it; otherwise, wait for it.
  (define (next-of-specified-kind/helper right-kind? starting-step)
    (set! stepper-is-waiting? #f)
    (match (find-later-step right-kind? starting-step)
      [(? number? n)
       (update-view/existing n)]
      ['nomatch
       (begin
            ;; each step has its own semaphore, so releasing one twice is
            ;; no problem.
            (when release-for-next-step
              (semaphore-post release-for-next-step))
            (when stepper-is-waiting?
              (error 'try-to-get-view
                     "try-to-get-view should not be reachable when already waiting for new step"))
            (let ([wait-for-it (lambda ()
                                 (set! stepper-is-waiting? right-kind?)
                                 (en/dis-able-buttons))])
              (match (async-channel-try-get view-channel)
                [(list new-step semaphore)
                 (add-view-step new-step semaphore)
                 (if (right-kind? (list-ref view-history (+ view 1)))
                     (update-view/existing (+ view 1))
                     (wait-for-it))]
                [#f (wait-for-it)])))]
      ['nomatch/seen-final
       (message-box (string-constant stepper-no-such-step/title)
                    (string-constant stepper-no-such-step))
       (update-view/existing (- (length view-history) 1))]))
  
  ;; prior-of-specified-kind: if the desired step is already in the list, display
  ;; it; otherwise, put up a dialog and jump to the first step.
  (define (prior-of-specified-kind right-kind?)
    (set! stepper-is-waiting? #f)
    (let* ([found-step (find-earlier-step right-kind? view)])
      (if found-step
          (update-view/existing found-step)
          (begin
            (message-box (string-constant stepper-no-such-step/title)
                         (string-constant stepper-no-such-step/earlier))
            (update-view/existing 0)))))
  
  ;; BUTTON/CHOICE BOX PROCEDURES
 
  
  ;; respond to a click on the "next" button
  (define (next)
    (next-of-specified-kind (lambda (x) #t)))
  
  ;; respond to a click on the "next application" button
  (define (next-application)
    (next-of-specified-kind application-step?))
  
  ;; respond to a click on the "Jump To..." choice
  (define (jump-to control event)
    ((second (list-ref pulldown-choices (send control get-selection)))))
  
  ;; previous : the action of the 'previous' button
  (define (previous)
    (prior-of-specified-kind (lambda (x) #t)))
  
  ;; previous-application : the action of the 'previous-application'
  ;; button
  (define (previous-application)
    (prior-of-specified-kind application-step?))

  ;; jump-to-beginning : the action of the choice menu entry
  (define (jump-to-beginning)
    (first-of-specified-kind (lambda (x) #t)))
  
  ;; jump-to-end : the action of the choice menu entry
  (define (jump-to-end)
    (next-of-specified-kind finished-stepping-step?))

  ;; jump-forward-to-selected : the action of the choice menu entry
  (define (jump-to-selected)
    (first-of-specified-kind selected-exp-step?))
  
  ;; jump-back-to-selection : the action of the choice menu entry
  (define (jump-back-to-selection)
    (prior-of-specified-kind selected-exp-step?))
  
  ;; GUI ELEMENTS:
  (define s-frame
    (make-object stepper-frame% drscheme-frame))
  (define button-panel
    (make-object horizontal-panel% (send s-frame get-area-container)))
  (define (add-button name fun)
    (make-object button% name button-panel (lambda (_1 _2) (fun))))
  (define (add-choice-box name fun)
    (new choice% [label name]
         [choices (map first pulldown-choices)]
         [parent button-panel]
         [callback fun]))
  
  (define pulldown-choices
    `((,(string-constant stepper-jump-to-beginning) ,jump-to-beginning)
      (,(string-constant stepper-jump-to-end)       ,jump-to-end)
      (,(string-constant stepper-jump-to-selected)  ,jump-to-selected)))
  
  (define previous-application-button (add-button (string-constant stepper-previous-application) previous-application))
  (define previous-button             (add-button (string-constant stepper-previous) previous))
  (define next-button                 (add-button (string-constant stepper-next) next))
  (define next-application-button     (add-button (string-constant stepper-next-application) next-application))
  (define jump-button                 (add-choice-box (string-constant stepper-jump) jump-to))
    
  (define canvas
    (make-object x:stepper-canvas% (send s-frame get-area-container)))
  
  ;; counting steps...
  (define status-text
    (new text%))
  (define _2 (send status-text insert ""))
  
  (define status-canvas
    (new editor-canvas%
         [parent button-panel]
         [editor status-text]
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
      (send e set-position (send e last-position))
      (update-status-bar)
      (send e end-edit-sequence))
    (en/dis-able-buttons))
  
  (define (update-status-bar)
    (send status-text delete 0 (send status-text last-position))
    (send status-text insert (format "~a/~a" view (length view-history))))
  
  ;; en/dis-able-buttons : set enable & disable the stepper buttons,
  ;; based on view-controller state
  (define (en/dis-able-buttons)
     ;; let's just leave all the buttons enabled...
     (void))
  
  (define (print-current-view item evt)
    (send (send canvas get-editor) print))
  
  ;; receive-result takes a result from the model and renders it
  ;; on-screen. Runs on the user thread.
  ;; : (step-result -> void)
  (define (receive-result result)
    ;; let's make sure this works:
    (parameterize ([pretty-print-show-inexactness #t])
      (match-let*
          ([(list step-text step-kind posns)
            (match result
              [(struct before-after-result (pre-exps post-exps kind pre-src post-src))
               (list (new x:stepper-text% 
                          [left-side pre-exps]
                          [right-side post-exps]
                          [show-inexactness? (send language-level stepper:show-inexactness?)])
                     kind (list pre-src post-src))]
              [(struct before-error-result (pre-exps err-msg pre-src))
               (list (new x:stepper-text%
                          [left-side pre-exps] 
                          [right-side err-msg]
                          [show-inexactness? (send language-level stepper:show-inexactness?)])
                     'finished-or-error (list pre-src))]
              [(struct error-result (err-msg))
               (list (new x:stepper-text% 
                          [left-side null]
                          [right-side err-msg]
                          [show-inexactness? (send language-level stepper:show-inexactness?)]) 
                     'finished-or-error (list))]
              [(struct finished-stepping ())
               (list x:finished-text 'finished-or-error (list))])])
        (hand-off-and-block step-text step-kind posns))))
  
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
                        (send language-level stepper:enable-let-lifting?)
			(send language-level stepper:show-consumed-and/or-clauses?))
   (send language-level stepper:show-lambdas-as-lambdas?)
   language-level
   run-on-drscheme-side
   #f)
  (send s-frame show #t)
  
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
#;(and
;; zero-length selection cases:
(equal? ((span-overlap 13 13) (model:make-posn-info 14 4)) #f)
(equal? ((span-overlap 14 14) (model:make-posn-info 14 4)) #t)
(equal? ((span-overlap 18 18) (model:make-posn-info 14 4)) #f)
;; nonzero-length selection cases:
(equal? ((span-overlap 13 14) (model:make-posn-info 14 4)) #f)
(equal? ((span-overlap 13 15) (model:make-posn-info 14 4)) #t)
(equal? ((span-overlap 13 23) (model:make-posn-info 14 4)) #t)
(equal? ((span-overlap 16 17) (model:make-posn-info 14 4)) #t)
(equal? ((span-overlap 16 24)  (model:make-posn-info 14 4)) #t)
(equal? ((span-overlap 18 21)  (model:make-posn-info 14 4)) #f))
