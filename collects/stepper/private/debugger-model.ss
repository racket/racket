(module debugger-model mzscheme
  (require (lib "unitsig.ss")
           (lib "contract.ss")
           (lib "etc.ss")
           (lib "mred.ss" "mred")
           (lib "debugger-sig.ss" "stepper")
           "my-macros.ss"
           "debugger-annotate.ss"
           "shared.ss"
           "marks.ss"
           "debugger-vc.ss"
           "debugger-bindings.ss")
 
  
  (define program-expander-contract
    (-> (-> void?) ; init
        (-> (union eof-object? syntax? (cons/c string? any/c)) (-> void?) void?) ; iter
        void?))
   
  (provide debugger-model@)

  ;(provide/contract [go (-> program-expander-contract ; program-expander
  ;                          void?)])
  
  (define (send-to-eventspace eventspace thunk)
    (parameterize ([current-eventspace eventspace])
      (queue-callback thunk)))

  (define debugger-debugger-error-port (current-error-port))

  (define debugger-model@   
    (unit/sig debugger-model^
      (import debugger-vc^
              (program-expander)
              (breakpoints breakpoint-origin))
      
      (define go-semaphore (make-semaphore))
      (define user-custodian (make-custodian))
      
      (define queue-eventspace (make-eventspace))
      
      (define (queue-result result)
        (send-to-eventspace 
         queue-eventspace
         (lambda ()
           (receive-result result))))
      
      (define basic-eval (current-eval))
      
      (define (break mark-set kind final-mark)
        (let ([mark-list (continuation-mark-set->list mark-set debug-key)])
          (queue-result (make-normal-breakpoint-info (cons final-mark mark-list) kind))
          (queue-result (make-breakpoint-halt))
          (semaphore-wait go-semaphore)))
      
      
      (define (step-through-expression expanded expand-next-expression)
        (let* ([annotated (annotate expanded breakpoints breakpoint-origin break)])
          ; (fprintf (current-error-port) "annotated: ~v\n" (syntax-object->datum annotated))
          (let ([expression-result
                 (parameterize ([current-eval basic-eval])
                   (eval annotated))])
            (queue-result (make-expression-finished (list expression-result)))
            (queue-result (make-breakpoint-halt))
            (semaphore-wait go-semaphore)
            (expand-next-expression))))
      
      (define (err-display-handler message exn)
        (queue-result (make-error-breakpoint-info message)))
      
      (define (go)
        (parameterize ([current-custodian user-custodian])
          (program-expander
           (lambda ()
             (error-display-handler err-display-handler)) ; init
           (lambda (expanded continue-thunk) ; iter
             (unless (eof-object? expanded)
               (step-through-expression expanded continue-thunk)))))))))

