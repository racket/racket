#lang racket/unit

;; this version of the view-controller just collects the steps up front rather
;; than blocking until the user presses the "next" button.

(require racket/class
         racket/match
         racket/list
         drscheme/tool
         mred
         string-constants
         racket/async-channel
         (prefix-in model: "private/model.ss")
         (prefix-in x: "private/mred-extensions.ss")
         "private/shared.ss"
         "private/model-settings.ss"
         "xml-sig.ss")


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
  
  ;; the first-step semaphore
  (define first-step-sema (make-semaphore 0))
  
  ;; the list of available views
  (define view-history null)
  
  ;; the number of available steps
  (define num-steps-available 0)
  
  ;; the view in the stepper window
  (define view #f)
  
  ;; wait for steps to show up on the channel.  When they do, add them to the list.
  (define (start-listener-thread)
    (thread
     (lambda ()
       (let loop ()
         (let* ([new-result (async-channel-get view-channel)]
                [new-step (format-result new-result)])
           (set! view-history (append view-history (list new-step)))
           (set! num-steps-available (length view-history))
           ;; this is only necessary the first time, but it's cheap:
           (semaphore-post first-step-sema))
         (update-status-bar)
         (loop)))))
    
  
  ;; find-later-step : given a predicate on history-entries, search through
  ;; the history for the first step that satisfies the predicate and whose 
  ;; number is greater than n (or -1 if n is #f), return # of step on success,
  ;; on failure return (list 'nomatch last-step) or (list 'nomatch/seen-final last-step) 
  ;; if we went past the final step
  (define (find-later-step p n)
    (let* ([n-as-num (or n -1)])
      (let loop ([step 0] 
                 [remaining view-history]
                 [seen-final? #f])
        (cond [(null? remaining) (cond [seen-final? (list 'nomatch/seen-final (- step 1))]
                                       [else (list 'nomatch (- step 1))])]
              [(and (> step n-as-num) (p (car remaining))) step]
              [else (loop (+ step 1)
                          (cdr remaining)
                          (or seen-final? (finished-stepping-step? (car remaining))))]))))
  
  ;; find-earlier-step : like find-later-step, but searches backward from
  ;; the given step.
  (define (find-earlier-step p n)
    (unless (number? n)
      (error 'find-earlier-step "can't find earlier step when no step is displayed."))
    (let* ([to-search (reverse (take view-history n))])
      (let loop ([step (- n 1)]
                 [remaining to-search])
        (cond [(null? remaining) 'nomatch]
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
  ;; it; otherwise, give up.
  (define (next-of-specified-kind/helper right-kind? starting-step)
    (match (find-later-step right-kind? starting-step)
      [(? number? n)
       (update-view/existing n)]
      [(list 'nomatch step)
       (message-box (string-constant stepper-no-such-step/title)
                    (string-constant stepper-no-such-step))
       (when (>= num-steps-available 0)
         (update-view/existing step))]
      [(list 'nomatch/seen-final step)
       (message-box (string-constant stepper-no-such-step/title)
                    (string-constant stepper-no-such-step))
       (when (>= num-steps-available 0)
         (update-view/existing step))]))
  
  ;; prior-of-specified-kind: if the desired step is already in the list, display
  ;; it; otherwise, put up a dialog and jump to the first step.
  (define (prior-of-specified-kind right-kind?)
    (match (find-earlier-step right-kind? view)
      [(? number? found-step)
       (update-view/existing found-step)]
      ['nomatch
       (message-box (string-constant stepper-no-such-step/title)
                    (string-constant stepper-no-such-step/earlier))
       (when (>= num-steps-available 0)
         (update-view/existing 0))]))
  
  ;; BUTTON/CHOICE BOX PROCEDURES
 
  
  ;; respond to a click on the "next" button
  (define (next)
    (next-of-specified-kind (lambda (x) #t)))
  
  ;; previous : the action of the 'previous' button
  (define (previous)
    (prior-of-specified-kind (lambda (x) #t)))
  
  ;; respond to a click on the "Jump To..." choice
  (define (jump-to control event)
    ((second (list-ref pulldown-choices (send control get-selection)))))
  
  ;; jump-to-beginning : the action of the choice menu entry
  (define (jump-to-beginning)
    (first-of-specified-kind (lambda (x) #t)))
  
  ;; GUI ELEMENTS:
  (define s-frame
    (make-object stepper-frame% drscheme-frame))
  (define button-panel
    (make-object horizontal-panel% (send s-frame get-area-container)))
  (define (add-button name fun)
    (new button% 
         [label name] 
         [parent button-panel] 
         [callback (lambda (_1 _2) (fun))]
         [enabled #f]))
  (define (add-choice-box name fun)
    (new choice% [label name]
         [choices (map first pulldown-choices)]
         [parent button-panel]
         [callback fun]
         [enabled #f]))
  
  (define pulldown-choices
    `((,(string-constant stepper-jump-to-beginning)            ,(lambda () (first-of-specified-kind (lambda (x) #t))))
      (,(string-constant stepper-jump-to-end)                  ,(lambda () (next-of-specified-kind finished-stepping-step?)))
      (,(string-constant stepper-jump-to-selected)             ,(lambda () (first-of-specified-kind selected-exp-step?)))
      (,(string-constant stepper-jump-to-next-application)     ,(lambda () (next-of-specified-kind application-step?)))
      (,(string-constant stepper-jump-to-previous-application) ,(lambda () (prior-of-specified-kind application-step?)))))
  
  (define previous-button             (add-button (string-constant stepper-previous) previous))
  (define next-button                 (add-button (string-constant stepper-next) next))
  (define jump-button                 (add-choice-box (string-constant stepper-jump) jump-to))
    
  
  (define canvas
    (make-object x:stepper-canvas% (send s-frame get-area-container)))
  
  ;; counting steps...
  (define status-text
    (new text%))
  
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
      (send e end-edit-sequence))
    (update-status-bar))
  
  
  ;; update the X/Y display in the upper right corner of the stepper;
  ;; this should be one-at-a-time.
  (define (update-status-bar)
    (call-with-semaphore update-status-bar-semaphore update-status-bar/inner))
    
  (define (update-status-bar/inner)
    (send status-text begin-edit-sequence)
    (send status-text lock #f)
    (send status-text delete 0 (send status-text last-position))
    (send status-text insert (format "~a/~a" view (length view-history)))
    (send status-text lock #t)
    (send status-text end-edit-sequence))
  
  (define update-status-bar-semaphore (make-semaphore 1))
  
  (define (enable-all-buttons)
    (send previous-button enable #t)
    (send next-button enable #t)
    (send jump-button enable #t))

  
  (define (print-current-view item evt)
    (send (send canvas get-editor) print))

  ;; translates a result into a step
  ;; format-result : result -> step?
  (define (format-result result)
    (match result
      [(struct before-after-result (pre-exps post-exps kind pre-src post-src))
       (make-step (new x:stepper-text% 
                       [left-side pre-exps]
                       [right-side post-exps]
                       [show-inexactness? (send language-level stepper:show-inexactness?)])
                  kind 
                  (list pre-src post-src))]
      [(struct before-error-result (pre-exps err-msg pre-src))
       (make-step (new x:stepper-text%
                       [left-side pre-exps] 
                       [right-side err-msg]
                       [show-inexactness? (send language-level stepper:show-inexactness?)])
                  'finished-or-error 
                  (list pre-src))]
      [(struct error-result (err-msg))
       (make-step (new x:stepper-text% 
                       [left-side null]
                       [right-side err-msg]
                       [show-inexactness? (send language-level stepper:show-inexactness?)]) 
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
  (send button-panel stretchable-width #f)
  (send button-panel stretchable-height #f)
  (send canvas stretchable-height #t)
  (send (send s-frame edit-menu:get-undo-item) enable #f)
  (send (send s-frame edit-menu:get-redo-item) enable #f)
  
  ;; START THE MODEL
  (start-listener-thread)
  (model:go
   program-expander-prime 
   ;; what do do with the results:
   (lambda (result) (async-channel-put view-channel result))
   (get-render-settings render-to-string render-to-sexp 
                        (send language-level stepper:enable-let-lifting?)
			(send language-level stepper:show-consumed-and/or-clauses?))
   (send language-level stepper:show-lambdas-as-lambdas?)
   language-level
   #f)
  
  (send s-frame show #t)
  
  ;; turn on the buttons and display the first step when it shows up:
  (thread
   (lambda ()
     (semaphore-wait first-step-sema)
     (jump-to-beginning)
     (enable-all-buttons)))
  
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
