(module servlet mzscheme
  (require "../managers/manager.ss")
  
  (define-struct (exn:fail:servlet:instance exn:fail) ())
  (define-struct servlet (custodian namespace manager handler))
  (define-struct servlet-instance-data (mutex context))
  
  (define-struct execution-context (connection request suspend))
  
  (define current-servlet (make-thread-cell #f))
  (define current-servlet-instance-id (make-thread-cell #f))
  
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
  
  (provide (all-defined)))
