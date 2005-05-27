(module web-server-unit mzscheme
  (require "sig.ss"
           "connection-manager.ss"
           (all-except "request-parsing.ss" request-bindings)
           "configuration-structures.ss"
           "util.ss"
           "response.ss"
           "servlet-tables.ss"
           "servlet.ss"
           "servlet-helpers.ss"
           "timer.ss"
           (lib "tcp-sig.ss" "net")
           (lib "unitsig.ss")
           (lib "string.ss")
           (lib "url.ss" "net")
           (lib "xml.ss" "xml")
           )
  (provide web-server@)

  (define myprint
    (lambda args
      (apply fprintf (cons (current-error-port) args))))


  ;; ****************************************
  ;; stick this auxilliary outside the unit so
  ;; I can get at it with require/expose

  ;; get-host : Url (listof (cons Symbol String)) -> String
  ;; host names are case insesitive---Internet RFC 1034
  (define DEFAULT-HOST-NAME "<none>")
  (define (get-host uri headers)
    (let ([lower!
           (lambda (s)
             (string-lowercase! s)
             s)])
      (cond
        [(url-host uri) => lower!]
        [(assq 'host headers)
         =>
         (lambda (h) (lower! (bytes->string/utf-8 (cdr h))))]
        [else DEFAULT-HOST-NAME])))

  ;; ****************************************

  (define web-server@
    (unit/sig web-server^
      (import net:tcp^ (config : web-config^))

      (define current-server-custodian (make-parameter #f))

      ;; make-servlet-custodian: -> custodian
      ;; create a custodian for the dynamic extent of a servlet continuation
      (define (make-servlet-custodian)
        (make-custodian (current-server-custodian)))

      ;; serve: -> -> void
      ;; start the server and return a thunk to shut it down
      (define (serve)
        (let ([the-server-custodian (make-custodian)])
          (start-connection-manager the-server-custodian)
          (parameterize ([current-custodian the-server-custodian]
                         [current-server-custodian the-server-custodian])
            (let ([get-ports
                   (let ([listener (tcp-listen config:port config:max-waiting
                                               #t config:listen-ip)])
                     (lambda () 
                       (let-values ([(ip op) (tcp-accept listener)])
                         (file-stream-buffer-mode op 'none)
                         (values ip op))))])
              (thread
               (lambda ()
                 (server-loop get-ports)))))
          (lambda ()
            (custodian-shutdown-all the-server-custodian))))

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
                                    ip op (current-custodian) #f))))))
            (loop))))

      ;; serve-ports : input-port output-port -> void
      ;; returns immediately, spawning a thread to handle
      ;; the connection
      ;; NOTE: this doesn't use a connection manager since
      ;;       connection managers don't do anything anyways. -robby
      ;; NOTE: (GregP) should allow the user to pass in a connection-custodian
      (define (serve-ports ip op)
        (let ([connection-cust (make-custodian)]
              [server-cust (make-custodian)])
          (parameterize ([current-custodian connection-cust]
                         [current-server-custodian server-cust])
            (thread
             (lambda ()
               (serve-connection
                (new-connection config:initial-connection-timeout
                                ip op connection-cust #f)))))))

      ;; serve-connection: connection -> void
      ;; respond to all requests on this connection
      (define (serve-connection conn)
        (let connection-loop ()
          (let-values ([(req close?) (read-request (connection-i-port conn))])
            (let* ([host (get-host (request-uri req) (request-headers req))]
                   [host-conf (config:virtual-hosts host)])
              ((host-log-message host-conf) (request-host-ip req)
               (request-client-ip req) (request-method req) (request-uri req) host)
              (set-connection-close?! conn close?)
              (dispatch conn req host-conf)
              (adjust-connection-timeout! conn config:initial-connection-timeout)
              (cond
                [close? (kill-connection! conn)]
                [else (connection-loop)])))))



      ;; dispatch: connection request host -> void
      ;; NOTE: (GregP) I'm going to use the dispatch logic out of v208 for now.
      ;;       I will move the other  dispatch logic out of the prototype
      ;;       at a later time.
      (define (dispatch conn req host-info)
        (let* ([uri (request-uri req)]
               [method (request-method req)]
               [path (translate-escapes (url-path->string (url-path uri)))])
          (cond
           [(access-denied? method path (request-headers req) host-info
                            config:access)
            => (lambda (realm)
                 (adjust-connection-timeout! conn (timeouts-password
                                                   (host-timeouts host-info)))
                 (request-authentication conn method uri host-info realm))]
           [(conf-prefix? path)
            (cond
             [(string=? "/conf/refresh-servlets" path)
              ;; more here - this is broken - only out of date or specifically mentioned
              ;; scripts should be flushed.  This destroys persistent state!
              (set-box! config:scripts (make-hash-table 'equal))
              (output-response/method
               conn
               ((responders-servlets-refreshed (host-responders host-info)))
                method)]
             [(string=? "/conf/refresh-passwords" path)
              ;; more here - send a nice error page
              (hash-table-put! config:access host-info
                               (read-passwords host-info))
              (output-response/method
               conn
               ((responders-passwords-refreshed (host-responders host-info)))
               method)
              ]
             [else
              (output-response/method
               conn
               ((responders-file-not-found (host-responders host-info))  uri)
               method)])]
           [(servlet-bin? path)
            (adjust-connection-timeout!
             conn
             (timeouts-servlet-connection (host-timeouts host-info)))
            ;; more here - make timeouts proportional to size of bindings
            (servlet-content-producer conn req host-info)]

           [else (file-content-producer conn req host-info)])))


      ;; conf-prefix?: string -> (union (listof string) #f)
      ;; does the path string have "/conf/" as a prefix?
      (define conf-prefix?
        (let ([conf-re (regexp "^/conf/.*")])
          (lambda (str)
            (regexp-match conf-re str))))

      (define servlet-bin?
        (let ([svt-bin-re (regexp "^/servlets/.*")])
          (lambda (str)
            (regexp-match svt-bin-re str))))

      ;; ripped this off from url-unit.ss
      (define (url-path->string strs)
        (apply
         string-append
         (let loop ([strs strs])
           (cond
            [(null? strs) '()]
            [else (list* "/"
                         (maybe-join-params (car strs))
                         (loop (cdr strs)))]))))

      ;; needs to unquote things!
      (define (maybe-join-params s)
        (cond
         [(string? s) s]
         [else (path/param-path s)]))

      ;; ****************************************
      ;; ****************************************
      ;; ACCESS CONTROL

      ;; pass-entry = (make-pass-entry str regexp (list sym str))
      (define-struct pass-entry (domain pattern users))

      ;; access-denied? : Method string x-table host Access-table -> (+ false str)
      ;; the return string is the prompt for authentication
      (define (access-denied? method uri-str headers host-info access-table)
        ;; denied?: str sym str -> (U str #f)
        ;; a function to authenticate the user
        (let ([denied?

               ;; GregP lookup the authenticator function, if you can't find it, then try to load the
               ;; passwords file for this host.
               (hash-table-get
                access-table host-info
                (lambda ()
                  ; more here - a malformed password file will kill the connection
                  (let ([f (read-passwords host-info)])
                    (hash-table-put! access-table host-info f)
                    f)))])
          (let ([user-pass (extract-user-pass headers)])
            (if user-pass
                (denied? uri-str (lowercase-symbol! (car user-pass)) (cdr user-pass))
                (denied? uri-str fake-user "")))))

      (define-struct (exn:password-file exn) ())

      ;; : host -> (str sym str -> (U str #f))
      ;; to produce a function that checks if a given url path is accessible by a given user with a given
      ;; password.  If not, the produced function returns a string, prompting for the password.
      ;; If the password file does not exist, all accesses are allowed.  If the file is malformed, an
      ;; exn:password-file is raised.
      (define (read-passwords host-info)
        (let ([password-path (host-passwords host-info)])
          (with-handlers ([void (lambda (exn)
                                  (raise (make-exn:password-file (format "could not load password file ~a" password-path)
                                                                 (current-continuation-marks))))])
            (if (and (file-exists? password-path) (memq 'read (file-or-directory-permissions password-path)))
                (let ([passwords
                       (let ([raw (load password-path)])
                         (unless (password-list? raw)
                           (raise "malformed passwords"))
                         (map (lambda (x) (make-pass-entry (car x) (regexp (cadr x)) (cddr x)))
                              raw))])

                  ;; string symbol bytes -> (union #f string)
                  (lambda (request-path user-name password)
                    (ormap (lambda (x)
                             (and (regexp-match (pass-entry-pattern x) request-path)
                                  (let ([name-pass (assq user-name (pass-entry-users x))])
                                    (if (and name-pass
                                             (string=?
                                              (cadr name-pass)
                                              (bytes->string/utf-8 password)))
                                        #f
                                        (pass-entry-domain x)))))
                           passwords)))
                (lambda (req user pass) #f)))))

      (define fake-user (gensym))

      ;; password-list? : TST -> bool

      ;; Note: andmap fails for dotted pairs at end.
      ;; This is okay, since #f ends up raising a caught exception anyway.
      (define (password-list? passwords)
        (and (list? passwords)
             (andmap (lambda (domain)
                       (and (pair? domain) (pair? (cdr domain)) (list (cddr domain))
                            (string? (car domain))
                            (string? (cadr domain))
                            (andmap (lambda (x)
                                      (and (pair? x) (pair? (cdr x)) (null? (cddr x))
                                           (symbol? (car x)) (string? (cadr x))))
                                    (cddr domain))))
                     passwords)))

      ;; request-authentication : connection Method URL iport oport host str bool -> bool
      ;; GregP: at first look, it seems that this gets called when the user
      ;; has supplied bad authentication credentials.
      (define (request-authentication conn method uri host-info realm)
        (output-response/method
         conn
         ((responders-authentication (host-responders host-info))
          uri `(WWW-Authenticate . ,(string-append " Basic
                       realm=\"" realm "\"")))
         method))


      ;; ************************************************************
      ;; ************************************************************
      ;; SERVING FILES

      ;; file-content-producer: connection request host -> void
      (define (file-content-producer conn req host-info)
        (serve-file conn (request-method req) (request-uri req) host-info))

      ;; serve-file : connection symbol uri host -> void
      ;; to find the file, including searching for implicit index files, and serve it out
      (define (serve-file conn method uri host-info)
        (let ([path (url-path->path (paths-htdocs (host-paths host-info))
                                    (translate-escapes (url-path->string (url-path uri))))])
          (cond
            [(file-exists? path)
             (output-file conn path method (get-mime-type path))]
            [(directory-exists? path)
             (let loop ([dir-defaults (host-indices host-info)])
               (cond
                [(pair? dir-defaults)
                 (let ([full-name (build-path path (car dir-defaults))])
                   (if (file-exists? full-name)
                       (cond
                        [(looks-like-directory? (url-path->string (url-path uri)))
                         (output-file conn full-name method (get-mime-type full-name))]
                        [else
                         (output-slash-message conn method (url-path->string (url-path uri)))])
                       (loop (cdr dir-defaults))))]
                [else
                 (output-response/method
                  conn
                  ((responders-file-not-found
                    (host-responders host-info)) uri)
                  method)]))]
            [else
             (output-response/method
              conn ((responders-file-not-found (host-responders host-info))
                    uri)
              method)])))

      ;; looks-like-directory : str -> bool
      ;; to determine if is url style path looks like it refers to a directory
      (define (looks-like-directory? path)
        (eq? #\/ (string-ref path (sub1 (string-length path)))))

      ;; output-slash-message: connection symbol string -> void
      ;; basically this is just a special error response
      (define (output-slash-message conn method url-path-str)
        (output-response/method
         conn
         (make-response/full
          301 "Moved Permanently"
          (current-seconds)
          TEXT/HTML-MIME-TYPE
          `([Location . ,(string-append url-path-str "/")])
          (list
           (xml->string
            (xexpr->xml
             `(html
               (head (title "Add a Slash"))
               (body "Please use "
                     (a ([href ,(string-append
                                 url-path-str "/")])
                        "this url") " instead."))))))
         method))

      ;; xml->string: xml -> string
      (define (xml->string some-xml)
        (let ([o-port (open-output-string)])
          (write-xml/content some-xml o-port)
          (get-output-string o-port)))


      ;; ************************************************************
      ;; ************************************************************
      ;; SERVING SERVLETS

      ;; servlet-content-producer: connection request host -> void
      (define (servlet-content-producer conn req host-info)
        (let ([meth (request-method req)])
          (if (eq? meth 'head)
              (output-response/method
               conn
               (make-response/full
                200 "Okay" (current-seconds) TEXT/HTML-MIME-TYPE
                '() (list "ignored"))
               meth)
              (let ([uri (request-uri req)])
                (set-request-bindings!
                 req
                 (read-bindings/handled conn meth uri (request-headers req)
                                        host-info))

                (cond
                 [(continuation-url? uri)
                  => (lambda (k-ref)
                       (invoke-servlet-continuation conn req k-ref host-info))]
                 [else
                  (servlet-content-producer/path conn req host-info uri)])))))

      ;; read-bindings/handled: connection symbol url headers host -> (listof (list (symbol string))
      ;; read the bindings and handle any exceptions
      (define (read-bindings/handled conn meth uri headers host-info)
        (with-handlers ([exn? (lambda (e)
                                (output-response/method
                                 conn
                                 ;((responders-protocol (host-responders host-info))
                                                        ; (exn-message e))
                                 ((responders-servlet-loading (host-responders
                                                               host-info))
                                  uri e)


                                 meth)
                                '())])
          (read-bindings conn meth uri headers)))

      ;; servlet-content-producer/path: connection request host url -> void
      ;; This is not a continuation url so the loading behavior is determined
      ;; by the url path. Build the servlet path and then load the servlet
      (define (servlet-content-producer/path conn req host-info uri)
        (with-handlers (;; couldn't find the servlet
                        [exn:fail:filesystem:exists:servlet?
                        (lambda (the-exn)
                          (output-response/method
                            conn
                            ((responders-file-not-found (host-responders
                                                          host-info))
                             (request-uri req))
                            (request-method req)))]
                        ;; servlet won't load (e.g. syntax error)
                        [(lambda (x) #t)
                         (lambda (the-exn)
                           (output-response/method
                            conn
                            ((responders-servlet-loading (host-responders
                                                          host-info)) uri
                                                          the-exn)
                            (request-method req)))])

          (let ([sema (make-semaphore 0)])
            (let/cc suspend
              (let* ([servlet-custodian (make-servlet-custodian)]
                     [inst (create-new-instance!
                            config:instances servlet-custodian
                            (make-execution-context
                             conn req (lambda () (suspend #t)))
                           sema)]
                     [real-servlet-path (url-path->path
                                         (paths-servlet (host-paths host-info))
                                         (url-path->string (url-path uri)))]
                     [servlet-exit-handler (make-servlet-exit-handler inst)]
                     )

                (parameterize ([current-directory (get-servlet-base-dir real-servlet-path)]
                               [current-custodian servlet-custodian]
                               [current-servlet-instance inst]
                               [exit-handler servlet-exit-handler])


                  (let (;; timer thread must be within the dynamic extent of
                        ;; servlet custodian
                        [time-bomb (start-timer (timeouts-default-servlet
                                                 (host-timeouts host-info))
                                                (lambda ()
                                                  (servlet-exit-handler #f)))]
                        ;; any resources (e.g. threads) created when the
                        ;; servlet is loaded should be within the dynamic
                        ;; extent of the servlet custodian
                        [servlet-program (cached-load real-servlet-path)])

                    (with-handlers ([(lambda (x) #t)
                                     (make-servlet-exception-handler inst
                                                                     host-info)])


                      ;; The following bindings need to be in scope for the
                      ;; invoke-unit/sig
                      (let ([adjust-timeout!
                             (lambda (secs) (reset-timer time-bomb secs))]
                            [initial-request req])

                        ;; Two possibilities:
                        ;; - module servlet. start : Request -> Void handles
                        ;;   output-response via send/finish, etc.
                        ;; - unit/sig or simple xexpr servlet. These must produce a
                        ;;   response, which is then output by the server.
                        ;; Here, we do not know if the servlet was a module,
                        ;; unit/sig, or Xexpr; we do know whether it produces a
                        ;; response.
                        (let ([r (invoke-unit/sig servlet-program servlet^)])
                          (when (response? r)
                            (send/back r)))))))))
            (semaphore-post sema))))

      ;; make-servlet-exit-handler: servlet-instance -> alpha -> void
      ;; exit handler for a servlet
      (define (make-servlet-exit-handler inst)
        (lambda (x)
          (remove-instance! config:instances inst)
          (kill-connection!
           (execution-context-connection
            (servlet-instance-context inst)))
          (custodian-shutdown-all (servlet-instance-custodian inst))))

      ;; make-servlet-exception-handler: host -> exn -> void
      ;; This exception handler traps all unhandled servlet exceptions
      ;; * Must occur within the dynamic extent of the servlet
      ;;   custodian since several connection custodians will typically
      ;;   be shutdown during the dynamic extent of a continuation
      ;; * Use the connection from the current-servlet-context in case
      ;;   the exception is raised while invoking a continuation.
      ;; * Use the suspend from the servlet-instanct-context which is
      ;;   closed over the current tcp ports which may need to be
      ;;   closed for an http 1.0 request.
      ;; * Also, suspend will post to the semaphore so that future
      ;;   requests won't be blocked.
      ;; * This fixes PR# 7066
      (define (make-servlet-exception-handler inst host-info)
        (lambda (the-exn)
          (let* ([ctxt (servlet-instance-context inst)]
                 [req (execution-context-request ctxt)]
                 [resp ((responders-servlet (host-responders
                                             host-info))
                        (request-uri req)
                        the-exn)])
            (output-response/method
             (execution-context-connection ctxt)
             resp (request-method req))
            ((execution-context-suspend ctxt)))))

      ;; path -> path
      ;; The actual servlet's parent directory.
      (define (get-servlet-base-dir servlet-path)
        (let loop ((path servlet-path))
          (let-values ([(base name must-be-dir?) (split-path path)])
            (if must-be-dir?
              (or (and (directory-exists? path) path)
                  (loop base))
              (or (and (directory-exists? base) base)
                  (loop base))))))


      ;; invoke-servlet-continuation: connection request continuation-reference
      ;;                              host -> void
      ;; pull the continuation out of the table and apply it
      (define (invoke-servlet-continuation conn req k-ref host-info)
        (with-handlers ([exn:servlet-instance?
                         (lambda (the-exn)
                           (output-response/method
                            conn
                            ((responders-file-not-found (host-responders
                                                         host-info))
                             (request-uri req))
                            (request-method req)))]
                        [exn:servlet-continuation?
                         (lambda (the-exn)
                           (output-response/method
                            conn
                            ((responders-file-not-found (host-responders
                                                         host-info))
                             (request-uri req))
                            (request-method req)))])
          (let* ([inst (hash-table-get config:instances (car k-ref)
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
              (let ((k*salt
                     (hash-table-get k-table (cadr k-ref)
                                     (lambda ()
                                       (raise
                                        (make-exn:servlet-continuation
                                         "" (current-continuation-marks)))))))
                (if (= (cadr k*salt) (caddr k-ref))
                    ((car k*salt) req)
                    (raise
                     (make-exn:servlet-continuation
                      "" (current-continuation-marks))))))
        (semaphore-post (servlet-instance-mutex inst)))))

      ;; ************************************************************
      ;; ************************************************************
      ;; Paul's ugly loading code:

      ;; cached-load : str -> script
      ;; timestamps are no longer checked for performance.  The cache must be explicitly
      ;; refreshed (see dispatch).
      (define (cached-load name)
        (hash-table-get (unbox config:scripts)
                        name
                        (lambda () (reload-servlet-script name))))

      ;; exn:i/o:filesystem:servlet-not-found =
      ;; (make-exn:fail:filesystem:exists:servlet str continuation-marks str sym)
      (define-struct (exn:fail:filesystem:exists:servlet
                      exn:fail:filesystem:exists) ())

      ;; reload-servlet-script : str -> script
      ;; The servlet is not cached in the servlet-table, so reload it from the filesystem.
      (define (reload-servlet-script servlet-filename)
        (cond
          [(load-servlet/path servlet-filename)
           => (lambda (svlt)
                (hash-table-put! (unbox config:scripts) servlet-filename svlt)
                svlt)]
          [else
           (raise (make-exn:fail:filesystem:exists:servlet
                   (string->immutable-string (format "Couldn't find ~a" servlet-filename))
                   (current-continuation-marks) ))]))

       ;; load-servlet/path path -> (union #f signed-unit)
      ;; given a string path to a filename attempt to load a servlet
      ;; A servlet-file will contain either
      ;;;; A signed-unit-servlet
      ;;;; A module servlet
      ;;;;;; (two versions, 'v1 and I don't know what 'typed-model-split-store0 is)
      ;;;; A response
      (define (load-servlet/path a-path)
        (and (file-exists? a-path)
             (let ([s (load/use-compiled a-path)])
               (cond
                 ;; signed-unit servlet
                 ; MF: I'd also like to test that s has the correct import signature.
                 [(unit/sig? s) s]
                 ; FIX - reason about exceptions from dynamic require (catch and report if not already)
                 ;; module servlet
                 [(void? s)
                  (parameterize ([current-namespace (config:make-servlet-namespace)])
                    (let* ([module-name `(file ,(path->string a-path))]
                           [version (dynamic-require module-name 'interface-version)])
                      (case version
                        [(v1)
                         (let ([timeout (dynamic-require module-name 'timeout)]
                               [start (dynamic-require module-name 'start)])
                           (unit/sig ()
                             (import servlet^)
                             (adjust-timeout! timeout)
                             (start initial-request)))]
                        [(typed-model-split-store-0)
                         (let ([constrained (dynamic-require module-name 'type)]
                               [the-servlet (dynamic-require module-name 'servlet)])
                           ; more here - check constraints
                           the-servlet)]
                        [else
                         (raise (format "unknown sevlet version ~e" version))])))]
                 ;; response
                 [(response? s)
                  (letrec ([go (lambda ()
                                 (begin
                                   (set! go (lambda () (load/use-compiled a-path)))
                                   s))])
                    (unit/sig () (import servlet^) (go)))]
                 [else
                  (raise (format "Loading ~e produced ~n~e~n instead of a servlet." a-path s))]))))

      )))