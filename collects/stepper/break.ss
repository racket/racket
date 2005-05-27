(module break mzscheme

  (require (lib "contract.ss"))
  
  (provide current-breakpoint-handler)
  
  (define (default-current-breakpoint-handler)
    (error 'default-current-breakpoint-handler
           "The current-breakpoint-handler parameter has not yet been set in this thread."))
  
  (define current-breakpoint-handler
    (make-parameter default-current-breakpoint-handler
                    (lambda (new-handler)
                      (if (and (procedure? new-handler)
                               (procedure-arity-includes? new-handler 0))
                          new-handler
                          (error 'current-breakpoint-handler "Bad value for current-breakpoint-handler: ~e" new-handler)))))
  
  
  (provide/contract [break (-> any)])
  
  (define (break)
    ((current-breakpoint-handler))))
  
  