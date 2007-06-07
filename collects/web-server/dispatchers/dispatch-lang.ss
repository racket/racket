(module dispatch-lang mzscheme
  (require (lib "kw.ss")
           (lib "list.ss")
           (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "session.ss" "web-server" "private")
           (only "../lang/web.ss"
                 initialize-servlet)           
           (lib "web-cells.ss" "web-server" "lang")
           "../private/request-structs.ss"
           "dispatch.ss"
           "../private/connection-manager.ss"
           "../private/util.ss"
           "../private/response.ss"
           "../configuration/namespace.ss"
           "../configuration/responders.ss")
  
  (provide/contract
   [interface-version dispatcher-interface-version?])
  (provide make)
  
  (define top-cust (current-custodian))
  
  ; same-servlet? : url? url? -> boolean?
  (define (same-servlet? req ses)
    (define (abstract-url u)
      (map path/param-path
           (url-path u)))
    #;(printf "~S => ~S~n" `(same-servlet? ,(url->string req) ,(url->string ses)) ans)
    (list-prefix? (abstract-url ses) (abstract-url req)))
  
  ;; make-session-url: url (listof string) -> url
  ;; produce a new url for this session:
  ;;   Minimal path to the servlet.
  ;;   No query.
  ;;   No fragment.
  (define (make-session-url uri new-path)
    (make-url
     (url-scheme uri)
     (url-user uri)
     (url-host uri)
     (url-port uri)
     #t
     (map (lambda (p) (make-path/param p empty))
          new-path)
     empty
     #f))
  
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
    ;; XXX Control extent of servlet data
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
                        '((lib "web-cells.ss" "web-server" "lang")
                          (lib "abort-resume.ss" "web-server" "lang")
                          (lib "session.ss" "web-server" "private")
                          (lib "request-structs.ss" "web-server" "private"))))
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