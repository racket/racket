(module dispatch-servlets mzscheme
  (require (lib "kw.ss")
           (lib "plt-match.ss")
           (lib "contract.ss"))
  (require "dispatch.ss"
           "../private/web-server-structs.ss"
           "../private/connection-manager.ss"
           "../private/response.ss"
           "../private/request-structs.ss"
           "../private/response-structs.ss"
           "../servlet/web-cells.ss"
           "../servlet/web.ss"
           "../configuration/responders.ss"
           "../configuration/namespace.ss"
           "../managers/manager.ss"
           "../managers/timeouts.ss"
           "../private/servlet.ss"
           "../private/cache-table.ss"
           "../private/util.ss")  
  (provide/contract
   [interface-version dispatcher-interface-version?])
  (provide make)
  
  (define interface-version 'v1)
  (define/kw (make config:scripts 
                   #:key
                   url->path
                   [make-servlet-namespace
                    (make-make-servlet-namespace)]
                   [responders-servlet-loading
                    servlet-loading-responder]
                   [responders-servlet
                    (gen-servlet-responder "servlet-error.html")]
                   [timeouts-default-servlet 30])
    
    ;; servlet-content-producer: connection request -> void
    (define (servlet-content-producer conn req)
      (define meth (request-method req))
      (define uri (request-uri req))
      (cond
        [(continuation-url? uri)
         => (match-lambda
              [(list instance-id k-id salt)
               (invoke-servlet-continuation conn req instance-id k-id salt)])]
        [else
         (servlet-content-producer/path conn req uri)]))
    
    ;; servlet-content-producer/path: connection request url -> void
    (define (servlet-content-producer/path conn req uri)
      (define response
        (with-handlers ([exn:fail:filesystem:exists:servlet?
                         (lambda (the-exn) (next-dispatcher))]
                        [(lambda (x) #t)
                         (lambda (the-exn) (responders-servlet-loading uri the-exn))])
          (call-with-continuation-prompt
           (lambda ()
             ; Create the session frame
             (with-frame
              (define instance-custodian (make-servlet-custodian))
              (define-values (servlet-path _)
                (with-handlers
                    ([void (lambda (e)
                             (raise (make-exn:fail:filesystem:exists:servlet
                                     (exn-message e)
                                     (exn-continuation-marks e))))])
                  (url->path uri)))
              (parameterize ([current-directory (directory-part servlet-path)]
                             [current-custodian instance-custodian]
                             [exit-handler
                              (lambda _
                                (kill-connection! conn)
                                (custodian-shutdown-all instance-custodian))])
                ;; any resources (e.g. threads) created when the
                ;; servlet is loaded should be within the dynamic
                ;; extent of the servlet custodian
                (define the-servlet (cached-load servlet-path))
                (parameterize ([current-servlet the-servlet]
                               [current-namespace (servlet-namespace the-servlet)])
                  (define manager (servlet-manager the-servlet))
                  (parameterize ([current-execution-context (make-execution-context req)])
                    (define instance-id ((manager-create-instance manager) (exit-handler)))
                    (parameterize ([current-servlet-instance-id instance-id])
                      (with-handlers ([(lambda (x) #t)
                                       (lambda (exn)
                                         (responders-servlet
                                          (request-uri req)
                                          exn))])
                        ((servlet-handler the-servlet) req))))))))
           servlet-prompt)))
      (output-response conn response))
    
    ;; default-server-instance-expiration-handler : (request -> response)
    (define (default-servlet-instance-expiration-handler req)
      (next-dispatcher))
    
    (define (invoke-servlet-continuation conn req instance-id k-id salt)
      (define uri (request-uri req))
      (define-values (servlet-path _) (url->path uri))
      (define the-servlet (cached-load servlet-path))
      (define manager (servlet-manager the-servlet))
      (define response
        (parameterize ([current-servlet the-servlet]
                       [current-directory (directory-part servlet-path)]
                       [current-servlet-instance-id instance-id]
                       [current-custodian (servlet-custodian the-servlet)]
                       [current-namespace (servlet-namespace the-servlet)]
                       [exit-handler
                        (lambda _
                          (kill-connection! conn)
                          (custodian-shutdown-all (servlet-custodian the-servlet)))])
          (with-handlers ([exn:fail:servlet-manager:no-instance?
                           (lambda (the-exn)
                             ((exn:fail:servlet-manager:no-instance-expiration-handler the-exn) req))]
                          [exn:fail:servlet-manager:no-continuation?
                           (lambda (the-exn)
                             ((exn:fail:servlet-manager:no-continuation-expiration-handler the-exn) req))]
                          [exn:fail:servlet:instance?
                           (lambda (the-exn)
                             (default-servlet-instance-expiration-handler req))])
            (parameterize ([current-execution-context (make-execution-context req)])
              (call-with-continuation-prompt
               (lambda ()
                 (define kcb ((manager-continuation-lookup manager) instance-id k-id salt))
                 ((custodian-box-value kcb) req))
               servlet-prompt)))))
      (output-response conn response))
    
    ;; cached-load : path -> script, namespace
    (define (cached-load servlet-path)
      (cache-table-lookup! (unbox config:scripts)
                           (string->symbol (path->string servlet-path))
                           (lambda () (load-servlet/path servlet-path))))
    
    ;; exn:i/o:filesystem:servlet-not-found =
    ;; (make-exn:fail:filesystem:exists:servlet str continuation-marks str sym)
    (define-struct (exn:fail:filesystem:exists:servlet
                    exn:fail:filesystem:exists) ())
    
    (define (v0.response->v1.lambda response response-path)
      (define go
        (box
         (lambda ()
           (set-box! go (lambda () (load/use-compiled response-path)))
           response)))
      (lambda (initial-request)
        ((unbox go))))
    
    (define (v1.module->v1.lambda timeout start)
      (lambda (initial-request)
        (adjust-timeout! timeout)
        (start initial-request)))
    
    ;; load-servlet/path path -> servlet
    (define (load-servlet/path a-path)
      (parameterize ([current-namespace (make-servlet-namespace
                                         #:additional-specs
                                         '((lib "servlet.ss" "web-server")
                                           (lib "servlet.ss" "web-server" "private")
                                           (lib "web.ss" "web-server" "servlet")
                                           (lib "web-cells.ss" "web-server" "servlet")))]
                     [current-custodian (make-servlet-custodian)])
        ; XXX load/use-compiled breaks errortrace
        (define s (load/use-compiled a-path))
        (cond
          [(void? s)
           (let* ([module-name `(file ,(path->string a-path))]
                  [version (dynamic-require module-name 'interface-version)])
             (case version
               [(v1)
                (let ([timeout (dynamic-require module-name 'timeout)]
                      [start (dynamic-require module-name 'start)])
                  (make-servlet (current-custodian)
                                (current-namespace)
                                (create-timeout-manager
                                 default-servlet-instance-expiration-handler
                                 timeout
                                 timeout)
                                (v1.module->v1.lambda timeout start)))]
               [(v2)
                (let ([start (dynamic-require module-name 'start)]
                      [manager (dynamic-require module-name 'manager)])
                  (make-servlet (current-custodian)
                                (current-namespace)
                                manager
                                start))]
               [else
                (error 'load-servlet/path "unknown servlet version ~e, must be 'v1 or 'v2" version)]))]
          [(response? s)
           (make-servlet (current-custodian)
                         (current-namespace)
                         (create-timeout-manager
                          default-servlet-instance-expiration-handler
                          timeouts-default-servlet
                          timeouts-default-servlet)
                         (v0.response->v1.lambda s a-path))]
          [else
           (error 'load-servlet/path "Loading ~e produced ~n~e~n instead of either (1) a response or (2) nothing and exports 'interface-version" a-path s)])))
    
    (values (lambda ()
              ;; XXX - this is broken - only out of date or specifically mentioned scripts should be flushed.  This destroys persistent state!
              (cache-table-clear! (unbox config:scripts)))
            servlet-content-producer)))