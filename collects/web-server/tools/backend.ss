(module backend mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "servlet-tables.ss" "web-server")
           (lib "timer.ss" "web-server")
           (lib "response.ss" "web-server")
           (all-except (lib "request-parsing.ss" "web-server") request-bindings)
           (lib "connection-manager.ss" "web-server"))

  (provide start-servlet resume-servlet)

  ;; make-servlet-custodian: -> custodian
  (define make-servlet-custodian
    (let ([cust (current-custodian)])
      (lambda () (make-custodian cust))))

  ;; start-servlet: connection request hash-table number (number->void request -> response) -> void
  ;; start a new instance of a servlet
  (define (start-servlet conn req instance-table instance-timeout svt)
    (let ([sema (make-semaphore 0)])
      (let/cc suspend
        (let* ([servlet-custodian (make-servlet-custodian)]
               [inst (create-new-instance!
                      instance-table servlet-custodian
                      (make-execution-context
                       conn req (lambda () (suspend #t)))
                      sema)]
               [servlet-exit-handler (make-servlet-exit-handler inst instance-table)]
               [time-bomb (start-timer instance-timeout
                                       (lambda () (servlet-exit-handler #f)))])
          (parameterize ([current-custodian servlet-custodian]
                         [current-servlet-instance inst]
                         [exit-handler servlet-exit-handler])
            (with-handlers ([(lambda (x) #t)
                             (make-servlet-exception-handler inst)])
              (let ([r (svt (lambda (secs)
                              (reset-timer time-bomb secs))
                            req)])
                (when (response? r)
                  (send/back r)))))))
      (semaphore-post sema)))

  ;; make-servlet-exit-handler: servlet-instance -> alpha -> void
  ;; exit handler for a servlet
  (define (make-servlet-exit-handler inst instance-table)
    (lambda (x)
      (remove-instance! instance-table inst)
      (kill-connection!
       (execution-context-connection
        (servlet-instance-context inst)))
      (custodian-shutdown-all (servlet-instance-custodian inst))))

  ;; make-servlet-exception-handler: host -> exn -> void
  ;; This exception handler traps all unhandled servlet exceptions
  (define (make-servlet-exception-handler inst)
    (lambda (the-exn)
      (let* ([ctxt (servlet-instance-context inst)]
             [req (execution-context-request ctxt)])
        (output-response/method
         (execution-context-connection ctxt)
         `(html (head (title "Error"))
                (body
                 (p "there was an error:")
                 (p ,(exn-message the-exn))))
         (request-method req))
        ((execution-context-suspend ctxt)))))

  ;; resume-servlet: connection request continuation-reference hash-table -> void
  ;; pull the continuation out of the table and apply it
  (define (resume-servlet conn req k-ref instance-table)
    (let* ([inst (hash-table-get instance-table (car k-ref)
                                 (lambda ()
                                   (raise
                                    (make-exn:servlet-instance
                                     "" (current-continuation-marks)))))]
           [k-table
            (servlet-instance-k-table inst)])
      (let/cc suspend
        (set-servlet-instance-context!
         inst
         (make-execution-context
          conn req (lambda () (suspend #t))))
        (semaphore-wait (servlet-instance-mutex inst))
        ((hash-table-get k-table (cadr k-ref)
                         (lambda ()
                           (raise
                            (make-exn:servlet-continuation
                             "" (current-continuation-marks)))))
         req))
      (semaphore-post (servlet-instance-mutex inst))))
  )

