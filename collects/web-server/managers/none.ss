(module none mzscheme
  (require (lib "contract.ss"))
  (require "manager.ss")
  (require "../servlet-structs.ss")
  (provide/contract
   [create-none-manager (expiration-handler? . -> . manager?)])
  
  (define-struct (none-manager manager) (instance-expiration-handler))
  (define (create-none-manager
           instance-expiration-handler)
    (define the-data (make-thread-cell #f))
    
    (define (create-instance data expire-fn)
      (thread-cell-set! the-data data)
      0)
    (define (adjust-timeout! instance-id secs)
      (void))
    
    (define (instance-lookup instance-id)
      (raise (make-exn:fail:servlet-manager:no-instance
              (string->immutable-string
               (format "No instance for id: ~a" instance-id))
              (current-continuation-marks)
              instance-expiration-handler)))
    
    (define (instance-lock! instance-id)
      (void))
    (define (instance-unlock! instance-id)
      (void))
    
    (define (instance-lookup-data instance-id)
      (thread-cell-ref the-data))
    
    (define (clear-continuations! instance-id)
      (instance-lookup instance-id))
    
    (define (continuation-store! instance-id k expiration-handler)
      (instance-lookup instance-id))
    (define (continuation-lookup instance-id a-k-id a-salt)
      (instance-lookup instance-id))
    
    (make-none-manager create-instance 
                       adjust-timeout!
                       instance-lookup-data
                       instance-lock!
                       instance-unlock!
                       clear-continuations!
                       continuation-store!
                       continuation-lookup
                       ; Specific
                       instance-expiration-handler)))