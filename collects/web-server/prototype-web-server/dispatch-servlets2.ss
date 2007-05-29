(module dispatch-servlets2 mzscheme
  (require (lib "kw.ss")
           (lib "contract.ss")
           (lib "connection-manager.ss" "web-server" "private")
           (lib "request-structs.ss" "web-server")
           (lib "response-structs.ss" "web-server")
           (lib "response.ss" "web-server" "private")
           (lib "util.ss" "web-server" "private")
           (lib "url.ss" "net")
           (lib "plt-match.ss")
           (lib "dispatch.ss" "web-server" "dispatchers")
           (lib "session.ss" "web-server" "prototype-web-server" "private")
           (only "private/web.ss"
                 initialize-servlet)           
           (lib "web-cells.ss" "web-server" "prototype-web-server" "lang-api")
           "../private/configuration.ss"
           "private/utils.ss")
  
  (provide/contract
   [interface-version dispatcher-interface-version?])
  (provide make)
  
  (define myprint #;printf (lambda _ (void)))
  
  (define top-cust (current-custodian))
  
  (define make-servlet-namespace
    (make-make-servlet-namespace
     #:to-be-copied-module-specs
     '(mzscheme
       (lib "web-cells.ss" "web-server" "prototype-web-server" "lang-api")
       (lib "abort-resume.ss" "web-server" "prototype-web-server" "private")
       (lib "session.ss" "web-server" "prototype-web-server" "private")
       (lib "request.ss" "web-server" "private"))))
  
  (define interface-version 'v1)
  (define/kw (make #:key
                   [htdocs-path "servlets"]
                   [timeouts-servlet-connection (* 60 60 24)]
                   [responders-servlet-loading
                    servlet-loading-responder]
                   [responders-servlet
                    (gen-servlet-responder "servlet-error.html")]
                   [responders-file-not-found
                    (gen-file-not-found-responder "not-found.html")])
    
    ;; dispatch : connection request -> void
    (define (dispatch conn req)
      (adjust-connection-timeout! conn timeouts-servlet-connection)
      ;; XXX - make timeouts proportional to size of bindings
      (myprint "servlet-content-producer~n")
      (let ([meth (request-method req)])
        (if (eq? meth 'head)
            (output-response/method
             conn
             (make-response/full
              200 "Okay" (current-seconds) TEXT/HTML-MIME-TYPE
              '() (list "ignored"))
             meth)
            (let ([uri (request-uri req)])
              (with-handlers ([void
                               (lambda (the-exn)
                                 (output-response/method
                                  conn
                                  (responders-servlet-loading uri the-exn)
                                  (request-method req)))])
                (cond
                  [(extract-session uri)
                   => (lambda (session-id)
                        (resume-session session-id conn req))]
                  [else
                   (begin-session conn req)]))))))
    
    ;; XXX Currently there are just sessions, should be servlets and sessions
    
    ;; begin-session: connection request
    (define (begin-session conn req)
      (myprint "begin-session~n")
      (let ([uri (request-uri req)])
        (let-values ([(a-path url-servlet-path url-path-suffix)
                      (url->servlet-path htdocs-path uri)])
          (myprint "a-path = ~s~n" a-path)
          (if a-path
              (parameterize ([current-directory (directory-part a-path)])
                (let* ([cust (make-custodian top-cust)]
                       [ns (make-servlet-namespace)]
                       [ses (new-session cust ns (make-session-url uri url-servlet-path))])
                  (parameterize ([current-custodian cust]
                                 [current-namespace ns]
                                 [current-session ses])
                    (let ([module-name `(file ,(path->string a-path))])
                      (myprint "dynamic-require ...~n")
                      (let ([start (dynamic-require module-name 'start)])
                        (set-session-servlet! ses
                                              (initialize-servlet start)))))
                  (myprint "resume-session~n")
                  (resume-session (session-id ses)
                                  conn req)))
              (output-response/method
               conn
               (responders-file-not-found uri)
               (request-method req))))))
    
    ; same-servlet? : url? url? -> boolean?
    (define (same-servlet? req ses)
      (define (abstract-url u)
        (map path/param-path
             (url-path u)))
      (define ans
        (let loop ([rp (abstract-url req)]
                   [sp (abstract-url ses)])
          (match sp
            [(list)
             #t]
            [(list-rest s sp)
             (match rp
               [(list)
                #f]
               [(list-rest r rp)
                (if (string=? s r)
                    (loop rp sp)
                    #f)])])))
      (myprint "~S => ~S~n" `(same-servlet? ,(url->string req) ,(url->string ses)) ans)
      ans)
    
    ;; resume-session: number connection request
    (define (resume-session ses-id conn req)
      (myprint "resume-session: ses-id = ~s~n" ses-id)
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
                      (myprint "session-handler ~S~n" (session-servlet ses))
                      (output-response conn
                                       ((session-servlet ses) req))))
                  (begin-session conn req)))]
        [else
         (myprint "resume-session: Unknown ses~n")
         (begin-session conn req)]))
    
    dispatch))