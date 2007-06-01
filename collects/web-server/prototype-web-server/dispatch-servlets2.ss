(module dispatch-servlets2 mzscheme
  (require (lib "kw.ss")
           (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "request-structs.ss" "web-server")
           (lib "session.ss" "web-server" "prototype-web-server" "private")
           (only "private/web.ss"
                 initialize-servlet)           
           (lib "web-cells.ss" "web-server" "prototype-web-server" "lang-api")
           "../dispatchers/dispatch.ss"
           "../private/connection-manager.ss"
           "../private/util.ss"
           "../private/response.ss"
           "../configuration/namespace.ss"
           "../configuration/responders.ss"
           "private/utils.ss")
  
  (provide/contract
   [interface-version dispatcher-interface-version?])
  (provide make)
  
  (define top-cust (current-custodian))
  
  (define interface-version 'v1)
  (define/kw (make #:key
                   url->path
                   [make-servlet-namespace 
                    (make-make-servlet-namespace)]
                   [timeouts-servlet-connection (* 60 60 24)]
                   [responders-servlet-loading
                    servlet-loading-responder]
                   [responders-servlet
                    (gen-servlet-responder "servlet-error.html")])
    
    ;; dispatch : connection request -> void
    (define (dispatch conn req)
      (define uri (request-uri req))
      (adjust-connection-timeout! conn timeouts-servlet-connection)
      ;; XXX - make timeouts proportional to size of bindings
      (cond
        [(extract-session uri)
         => (lambda (session-id)
              (resume-session session-id conn req))]
        [else
         (begin-session conn req)]))
    
    ;; XXX Currently there are just sessions, should be servlets and sessions
    
    ;; begin-session: connection request
    (define (begin-session conn req)
      (define uri (request-uri req))
      (with-handlers ([void (lambda (exn) (next-dispatcher))])
        (define-values (a-path url-servlet-path) (url->path uri))
        (with-handlers ([void
                         (lambda (the-exn)
                           (output-response/method
                            conn
                            (responders-servlet-loading uri the-exn)
                            (request-method req)))])
          (parameterize ([current-directory (directory-part a-path)])
            (define cust (make-custodian top-cust))
            (define ns (make-servlet-namespace
                        #:additional-specs
                        '((lib "servlet.ss" "web-server")
                          (lib "web-cells.ss" "web-server" "prototype-web-server" "lang-api")
                          (lib "abort-resume.ss" "web-server" "prototype-web-server" "private")
                          (lib "session.ss" "web-server" "prototype-web-server" "private")
                          (lib "request.ss" "web-server" "private"))))
            (define ses (new-session cust ns (make-session-url uri (map path->string url-servlet-path))))
            (parameterize ([current-custodian cust]
                           [current-namespace ns]
                           [current-session ses])
              (define start
                (dynamic-require `(file ,(path->string a-path))
                                 'start))
              (set-session-servlet! ses (initialize-servlet start)))
            (resume-session (session-id ses)
                            conn req)))))
    
    ; same-servlet? : url? url? -> boolean?
    (define (same-servlet? req ses)
      (define (abstract-url u)
        (map path/param-path
             (url-path u)))
      (define ans (list-prefix (abstract-url ses) (abstract-url req)))
      #;(printf "~S => ~S~n" `(same-servlet? ,(url->string req) ,(url->string ses)) ans)
      (and ans #t))
    
    ;; resume-session: number connection request
    (define (resume-session ses-id conn req)
      (cond
        [(lookup-session ses-id)
         => (lambda (ses)
              (if (same-servlet? (request-uri req) (session-url ses))
                  (parameterize ([current-custodian (session-cust ses)]
                                 [current-session ses])
                    (with-handlers ([void
                                     (lambda (the-exn)
                                       (output-response/method
                                        conn
                                        (responders-servlet (request-uri req) the-exn)
                                        (request-method req)))])
                      (output-response conn ((session-servlet ses) req))))
                  (begin-session conn req)))]
        [else
         (begin-session conn req)]))
    
    dispatch))