(module debugger-vc mzscheme
  (require (lib "unitsig.ss")
           (lib "debugger-sig.ss" "stepper")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           "marks.ss"
           "debugger-bindings.ss")
  
  (provide debugger-vc@)
  
  (define debugger-vc@ 
    (unit/sig debugger-vc^
      (import debugger-model^
              (drs-window))
      
      (define debugger-eventspace 
        (parameterize ([current-custodian user-custodian])
          (make-eventspace)))
      
      (define (receive-result result)
        (set! event-list (append event-list (list result)))
        (parameterize ([current-eventspace debugger-eventspace])
          (queue-callback
           (lambda ()
             (namespace-set-variable-value! 'current-event-num (- (length event-list) 1))
             (namespace-set-variable-value! 'current-frame-num 0))))
        (send-output-to-debugger-window (format-event result) debugger-output))
      
      (define (format-event debugger-event)
        (cond [(normal-breakpoint-info? debugger-event) 
               (when (null? (normal-breakpoint-info-mark-list debugger-event))
                 (error 'format-event "mark list was empty")) ; should never happen; at-brpt mark should always be there
               (format "normal breakpoint\nsource:~v\n" (mark-source (car (normal-breakpoint-info-mark-list debugger-event))))]
              [(error-breakpoint-info? debugger-event)
               (format "error breakpoint\nmessage: ~v\n" (error-breakpoint-info-message debugger-event))]
              [(breakpoint-halt? debugger-event)
               (format "breakpoint halt\n")]
              [(expression-finished? debugger-event)
               (format "expression finished\nresults: ~v\n" (expression-finished-returned-value-list debugger-event))]))
      
      
      (define event-list null)
      
      (define (events) event-list)
      
      (thread 
       (lambda () 
         (graphical-read-eval-print-loop debugger-eventspace #t)))
      
      (define (highlight-source-position posn)
        (send (send drs-window get-definitions-text)
              set-position
              posn
              (+ 1 posn)))
      
      (define debugger-output (make-output-window drs-window user-custodian))
      
      ; set up debugger eventspace
      
      (parameterize ([current-eventspace debugger-eventspace])
        (queue-callback 
         (lambda ()
           (namespace-set-variable-value! 'go-semaphore go-semaphore)
           (namespace-set-variable-value! 'events events)
           (namespace-set-variable-value! 'user-custodian user-custodian)
           (namespace-set-variable-value! 'highlight-source-position highlight-source-position)
           (install-debugger-bindings))))))
  
  ;; Info functions:
  
  ;; Debugger Output Window:
  
  (define output-frame%
    (class frame:basic% ()
      
      (init-field drs-window)
      (init-field user-custodian)
      
      (define/override (on-close)
        (send drs-window on-debugger-close)
        (custodian-shutdown-all user-custodian))
      
      (super-instantiate ())))
  
  ; make-output-window : (-> text:basic%)
  (define (make-output-window drs-window cust)
    (let* ([frame (instantiate output-frame% () 
                    (label "Debugger Output")
                    (width 400)
                    (height 400)
                    (drs-window drs-window)
                    (user-custodian cust))]
           [canvas (instantiate canvas:basic% () (parent (send frame get-area-container)))]
           [text (instantiate text:basic% ())])
      (send canvas set-editor text)
      (send frame show #t)
      text))
  
  ; send-output-to-debugger-window : (string text:basic% -> void)
  (define (send-output-to-debugger-window str text)
    (send text insert str (send text last-position))))      
      
    