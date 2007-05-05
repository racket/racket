(module servlet mzscheme
  (require (lib "contract.ss"))
  (require "../managers/manager.ss"
           "../servlet-structs.ss"
           "connection-structs.ss"
           "../request-structs.ss")
  
  (define-struct (exn:fail:servlet:instance exn:fail) ())
  (define-struct servlet (custodian namespace manager handler))
  (define-struct servlet-instance-data (mutex))  
  (define-struct execution-context (connection request suspend))
  
  (define current-servlet (make-thread-cell #f))
  (define current-servlet-instance-id (make-thread-cell #f))
  (define current-execution-context (make-thread-cell #f))
  
  (define (get-current-servlet-instance-id)
    (define instance-id (thread-cell-ref current-servlet-instance-id))
    (unless instance-id
      (raise (make-exn:fail:servlet:instance "" (current-continuation-marks))))
    instance-id)
  
  (define (current-servlet-manager)
    (define servlet (thread-cell-ref current-servlet))
    (unless servlet
      (raise (make-exn:fail:servlet:instance "" (current-continuation-marks))))
    (servlet-manager servlet))
  
  (define (current-servlet-instance-data)
    (define manager (current-servlet-manager))
    (define instance-id (thread-cell-ref current-servlet-instance-id))
    ((manager-instance-lookup-data manager) instance-id))
  
  (provide/contract
   [struct (exn:fail:servlet:instance exn:fail)
           ([message string?]
            [continuation-marks continuation-mark-set?])]
   [struct servlet 
           ([custodian custodian?]
            [namespace namespace?]
            [manager manager?]
            [handler (request? . -> . servlet-response?)])]
   [struct servlet-instance-data
           ([mutex semaphore?])]
   [struct execution-context 
           ([connection connection?]
            [request request?]
            [suspend procedure?])]
   ; XXX contract maybe
   [current-servlet thread-cell?]
   ; XXX contract maybe
   [current-servlet-instance-id thread-cell?]
   ; XXX contract maybe
   [current-execution-context thread-cell?]
   [get-current-servlet-instance-id (-> number?)]
   [current-servlet-manager (-> manager?)]
   [current-servlet-instance-data (-> servlet-instance-data?)]))