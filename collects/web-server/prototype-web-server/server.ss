(module server mzscheme
  (require (lib "connection-manager.ss" "web-server" "private")
           (lib "request.ss" "web-server" "private")
           (lib "response.ss" "web-server")
           (lib "servlet-helpers.ss" "web-server" "private")
           (lib "response.ss" "web-server" "private")
           (lib "util.ss" "web-server" "private")
           (lib "url.ss" "net")
           (lib "list.ss")
           (lib "plt-match.ss")           
           (lib "configuration-structures.ss" "web-server" "private")
           (lib "dispatch.ss" "web-server" "dispatchers")
           (lib "session.ss" "web-server" "prototype-web-server")
           (only (lib "abort-resume.ss" "web-server" "prototype-web-server")
                 abort/cc
                 safe-call?
                 the-cont-key)
           (only "persistent-web-interaction.ss"
                 start-servlet)           
           (lib "web-cells.ss" "web-server" "prototype-web-server" "newcont")
           "xexpr-extras.ss"
           "utils.ss"
           "hardcoded-configuration.ss")
  
  (provide serve dispatch)
  
  (define myprint printf #;(lambda _ (void)))
  
  (define thread-connection-state (make-thread-cell #f))
  (define-struct connection-state (conn req))
  (define top-cust (current-custodian))
  
  ;; ************************************************************
  ;; serve: -> -> void
  ;; start the server and return a thunk to shut it down
  (define (serve . port)
    (let ([the-server-custodian (make-custodian)])
      (start-connection-manager the-server-custodian)
      (parameterize ([current-custodian the-server-custodian])
        (let ([get-ports
               (let ([listener (tcp-listen (if (not (null? port))
                                               (car port)
                                               config:port)
                                           config:max-waiting
                                           #t config:listen-ip)])
                 (lambda () (tcp-accept listener)))])
          (thread
           (lambda ()
             (server-loop get-ports)))))
      (lambda ()
        (custodian-shutdown-all the-server-custodian))))
  
  ;; ************************************************************
  ;; server-loop: (-> i-port o-port) -> void
  ;; start a thread to handle each incoming connection
  (define (server-loop get-ports)
    (let loop ()
      (let ([connection-cust (make-custodian)])
        (parameterize ([current-custodian connection-cust])
          (let-values ([(ip op) (get-ports)])
            (thread
             (lambda ()
               (serve-connection
                (new-connection config:initial-connection-timeout
                                ip op (current-custodian) #f)))))))
      (loop)))
  
  ;; ************************************************************
  ;; serve-connection: connection -> void
  ;; respond to all requests on this connection
  (define (serve-connection conn)
    (myprint "serve-connection~n")
    (let connection-loop ()
      (let-values ([(req close?) (read-request (connection-i-port conn))])
        (let* ([host (get-host (request-uri req) (request-headers req))]
               [host-conf (config:virtual-hosts host)])
          (set-connection-close?! conn close?)
          (dispatch conn req host-conf)
          (adjust-connection-timeout! conn config:initial-connection-timeout)
          ; TODO: track down bus-error here
          ;   1. uncomment next line
          ;   2. comment-out cond expression
          ;   3. use error-servlet01.ss
          ;; TODO: while I think of it. The session object needs
          ;;  to be guarded by a mutex.
          ;(kill-connection! conn)
          (cond
            [close? (kill-connection! conn)]
            [else (connection-loop)])))))
  
  ;; ************************************************************
  ;; dispatch: connection request host -> void
  ;; trivial dispatcher
  (define (dispatch conn req host-info)
    (define-values (uri method path) (decompose-request req))
    (myprint "dispatch~n")
    (if (regexp-match #rx"^/servlets" path)
        (begin
          (adjust-connection-timeout!
           conn
           (timeouts-servlet-connection (host-timeouts host-info)))
          ;; more here - make timeouts proportional to size of bindings
          (servlet-content-producer conn req host-info))    
        (next-dispatcher)))
  
  ;; ************************************************************
  ;; ************************************************************
  ;; SERVING SERVLETS
  
  ;; servlet-content-producer: connection request host -> void
  (define (servlet-content-producer conn req host-info)
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
            (thread-cell-set! thread-connection-state
                              (make-connection-state conn req))
            (with-handlers ([void
                             (lambda (the-exn)
                               (output-response/method
                                (connection-state-conn (thread-cell-ref thread-connection-state))
                                ((responders-servlet-loading (host-responders host-info))
                                 uri the-exn)
                                (request-method
                                 (connection-state-req
                                  (thread-cell-ref thread-connection-state)))))])
              (cond
                [(resume-session? uri)
                 => (lambda (session-id)
                      (resume-session session-id host-info))]
                [else
                 (begin-session host-info)]))))))
  
  ;; Parameter Parsing
  
  ;; encodes a simple number:
  (define URL-PARAMS:REGEXP (regexp "([0-9]+)"))
  
  (define (match-url-params x) (regexp-match URL-PARAMS:REGEXP x))
  
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
  
  ;; url->param: url -> (union string #f)
  (define (url->param a-url)
    (let ([l (filter path/param? (url-path a-url))])
      (and (not (null? l))
           (path/param-param (car l)))))
  
  ;(resume-session? (string->url "http://localhost:9000/;123"))
  ;(resume-session? (string->url "http://localhost:9000/;foo"))
  ;(resume-session? (string->url "http://localhost:9000/foo/bar"))
  
  ;; ************************************************************
  
  ;; begin-session: connection request host-info
  (define (begin-session host-info)
    (myprint "begin-session~n")
    (let ([uri (request-uri (connection-state-req (thread-cell-ref thread-connection-state)))])
      (let-values ([(a-path url-servlet-path url-path-suffix)
                    (url->servlet-path
                     (paths-servlet (host-paths host-info))
                     uri)])
        (myprint "a-path = ~s~n" a-path)
        (if a-path
            (parameterize ([current-directory (directory-part a-path)])
              (let* ([cust (make-custodian top-cust)]
                     [ns (make-servlet-namespace)]
                     [ses (new-session cust ns (make-session-url uri url-servlet-path) a-path)])
                (parameterize ([current-custodian cust]
                               [current-namespace ns]
                               [current-session ses])
                  (let* ([module-name `(file ,(path->string a-path))])
                    (myprint "dynamic-require ...~n")
                    (with-handlers ([exn:fail:contract?
                                     (lambda _
                                       (dynamic-require module-name #f))])
                      (let ([start (dynamic-require module-name 'start)])
                        (abort/cc 
                         (with-continuation-mark safe-call? '(#t start)
                           (start
                            (with-continuation-mark the-cont-key start
                              (start-servlet)))))))))
                (myprint "resume-session~n")
                (resume-session (session-id ses) host-info)))
            (output-response/method
             (connection-state-conn (thread-cell-ref thread-connection-state))
             ((responders-file-not-found (host-responders host-info))  uri)
             (request-method (connection-state-req (thread-cell-ref thread-connection-state))))))))
  
  (define to-be-copied-module-specs
    '(mzscheme
      (lib "web-cells.ss" "web-server" "prototype-web-server" "newcont")
      (lib "abort-resume.ss" "web-server" "prototype-web-server")
      (lib "session.ss" "web-server" "prototype-web-server")
      (lib "request.ss" "web-server" "private")))
  
  ;; get the names of those modules.
  (define to-be-copied-module-names
    (let ([get-name
           (lambda (spec)
             (if (symbol? spec)
                 spec
                 ((current-module-name-resolver) spec #f #f)))])
      (map get-name to-be-copied-module-specs)))
  
  (define (make-servlet-namespace)
    (let ([server-namespace (current-namespace)]
          [new-namespace (make-namespace)])
      (parameterize ([current-namespace new-namespace])
        (for-each (lambda (name) (namespace-attach-module server-namespace name))
                  to-be-copied-module-names)
        new-namespace)))
  
  ;; ************************************************************
  ;; resume-session: connection request number host-info
  (define (resume-session ses-id host-info)
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
                                  (connection-state-conn (thread-cell-ref thread-connection-state))
                                  ((responders-servlet (host-responders host-info))
                                   (request-uri
                                    (connection-state-req
                                     (thread-cell-ref thread-connection-state)))
                                   the-exn)
                                  (request-method
                                   (connection-state-req (thread-cell-ref thread-connection-state)))))])
                (printf "session-handler ~S~n" (session-handler ses))
                (output-response
                 (connection-state-conn (thread-cell-ref thread-connection-state))
                 (xexpr+extras->xexpr
                  ((session-handler ses)
                   (connection-state-req (thread-cell-ref thread-connection-state))))))))]
      [else
       (myprint "resume-session: Unknown ses~n")
       ;; TODO: should just start a new session here.
       (begin-session host-info)])))