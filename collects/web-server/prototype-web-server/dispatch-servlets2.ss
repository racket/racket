(module dispatch-servlets2 mzscheme
  (require (lib "kw.ss")
           "../private/configuration.ss"           
           (lib "connection-manager.ss" "web-server" "private")
           (lib "request-structs.ss" "web-server")
           (lib "response-structs.ss" "web-server")
           (lib "response.ss" "web-server" "private")
           (lib "util.ss" "web-server" "private")
           (lib "url.ss" "net")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "dispatch.ss" "web-server" "dispatchers")
           (lib "session.ss" "web-server" "prototype-web-server" "private")
           (only (lib "abort-resume.ss" "web-server" "prototype-web-server" "private")
                 run-start)
           (only "private/web.ss"
                 start-servlet)           
           (lib "web-cells.ss" "web-server" "prototype-web-server" "lang-api")
           "private/utils.ss")
  
  (provide make)
  
  (define myprint printf #;(lambda _ (void)))
  
  (define top-cust (current-custodian))
  
  ;; Parameter Parsing
  
  ;; encodes a simple number:
  (define (match-url-params x) (regexp-match #rx"([0-9]+)" x))
  
  ;; resume-session? url -> (union number #f)
  ;; Determine if the url encodes a session-id and extract it
  (define (resume-session? a-url)
    (myprint "resume-session?: url-string = ~s~n" (url->string a-url))
    (let ([k-params (filter match-url-params
                            (apply append
                                   (map path/param-param (url-path a-url))))])
      (myprint "resume-session?: ~S~n" k-params)
      (if (empty? k-params)
          #f
          (match (match-url-params (first k-params))
            [(list _ n)
             (myprint "resume-session?: Found ~a~n" n)
             (string->number n)]
            [_
             #f]))))
    
  (define make-servlet-namespace
    (make-make-servlet-namespace
     #:to-be-copied-module-specs
     '(mzscheme
      (lib "web-cells.ss" "web-server" "prototype-web-server" "lang-api")
      (lib "abort-resume.ss" "web-server" "prototype-web-server" "private")
      (lib "session.ss" "web-server" "prototype-web-server" "private")
      (lib "request.ss" "web-server" "private"))))
  
  (define/kw (make #:key
                   [servlet-root "servlets"]
                   [timeouts-servlet-connection (* 60 60 24)]
                   [responders-servlet-loading
                    servlet-loading-responder]
                   [responders-servlet
                    (gen-servlet-responder "servlet-error.html")]
                   [responders-file-not-found
                    (gen-file-not-found-responder "not-found.html")])
    
    ;; ************************************************************
    ;; dispatch: connection request -> void
    ;; trivial dispatcher
    (define (dispatch conn req)
      (define-values (uri method path) (decompose-request req))
      (myprint "dispatch~n")
      (if (regexp-match #rx"^/servlets" path)
          (begin
            (adjust-connection-timeout! conn timeouts-servlet-connection)
            ;; more here - make timeouts proportional to size of bindings
            (servlet-content-producer conn req))    
          (next-dispatcher)))
    
    ;; ************************************************************
    ;; ************************************************************
    ;; SERVING SERVLETS
    
    ;; servlet-content-producer: connection request -> void
    (define (servlet-content-producer conn req)
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
                  [(resume-session? uri)
                   => (lambda (session-id)
                        (resume-session session-id conn req))]
                  [else
                   (begin-session conn req)]))))))
    
    ;; begin-session: connection request
    (define (begin-session conn req)
      (myprint "begin-session~n")
      (let ([uri (request-uri req)])
        (let-values ([(a-path url-servlet-path url-path-suffix)
                      (url->servlet-path servlet-root uri)])
          (myprint "a-path = ~s~n" a-path)
          (if a-path
              (parameterize ([current-directory (directory-part a-path)])
                (let* ([cust (make-custodian top-cust)]
                       [ns (make-servlet-namespace)]
                       [ses (new-session cust ns (make-session-url uri url-servlet-path) a-path)])
                  (parameterize ([current-custodian cust]
                                 [current-namespace ns]
                                 [current-session ses])
                    (let ([module-name `(file ,(path->string a-path))])
                      (myprint "dynamic-require ...~n")
                      (with-handlers ([exn:fail:contract?
                                       (lambda _
                                         (dynamic-require module-name #f))])
                        (let ([start (dynamic-require module-name 'start)])
                          (run-start start-servlet start)))))
                  (myprint "resume-session~n")
                  (resume-session (session-id ses)
                                  conn req)))
              (output-response/method
               conn
               (responders-file-not-found uri)
               (request-method req))))))
    
    ;; resume-session: number connection request
    (define (resume-session ses-id conn req)
      ; XXX Check if session is for same servlet!
      (myprint "resume-session: ses-id = ~s~n" ses-id)
      (cond
        [(lookup-session ses-id)
         => (lambda (ses)
              (parameterize ([current-custodian (session-cust ses)]
                             [current-session ses])
                (with-handlers ([void
                                 (lambda (the-exn)
                                   (output-response/method
                                    conn
                                    (responders-servlet (request-uri req) the-exn)
                                    (request-method req)))])
                  #;(printf "session-handler ~S~n" (session-handler ses))
                  (output-response conn
                                   ((session-handler ses) req)))))]
        [else
         (myprint "resume-session: Unknown ses~n")
         ;; TODO: should just start a new session here.
         (begin-session conn req)]))
    
    dispatch))