(module dispatch-servlets mzscheme
  (require (lib "url.ss" "net")
           (lib "kw.ss")
           (lib "plt-match.ss")
           (lib "unitsig.ss"))
  (require "dispatch.ss"
           "../web-server-structs.ss"
           "../connection-manager.ss"
           "../response.ss"
           "../servlet.ss"
           "../sig.ss"
           "../configuration.ss"
           (all-except "../util.ss" translate-escapes)
           "../managers/manager.ss"
           "../managers/timeouts.ss"
           "../private/url.ss"
           "../private/servlet.ss"
           "../private/cache-table.ss")  
  (provide interface-version
           make)
  
  (define interface-version 'v1)
  (define/kw (make config:instances config:scripts config:make-servlet-namespace
                   #:key
                   [servlet-root "servlets"]
                   [responders-servlets-refreshed
                    (gen-servlets-refreshed "servlet-refresh.html")]
                   [responders-servlet-loading
                    servlet-loading-responder]
                   [responders-servlet
                    (gen-servlet-responder "servlet-error.html")]
                   [responders-file-not-found
                    (gen-file-not-found-responder "not-found.html")]
                   [timeouts-servlet-connection (* 60 60 24)]
                   [timeouts-default-servlet 30])
    ;; ************************************************************
    ;; ************************************************************
    ;; SERVING SERVLETS
    
    ;; servlet-content-producer: connection request -> void
    (define (servlet-content-producer conn req)
      (define meth (request-method req))
      (define uri (request-uri req))
      (case meth
        [(head)
         (output-response/method
          conn
          (make-response/full
           200 "Okay" (current-seconds) TEXT/HTML-MIME-TYPE
           '() (list "ignored"))
          meth)]
        [else
         (cond
           [(continuation-url? uri)
            => (match-lambda
                 [(list instance-id k-id salt)
                  (invoke-servlet-continuation conn req instance-id k-id salt)])]
           [else
            (servlet-content-producer/path conn req uri)])]))
    
    ;; servlet-content-producer/path: connection request url -> void
    ;; This is not a continuation url so the loading behavior is determined
    ;; by the url path. Build the servlet path and then load the servlet
    (define (servlet-content-producer/path conn req uri)
      (with-handlers (;; couldn't find the servlet
                      [exn:fail:filesystem:exists:servlet?
                       (lambda (the-exn)
                         (output-response/method conn (responders-file-not-found (request-uri req)) (request-method req)))]
                      ;; servlet won't load (e.g. syntax error)
                      [(lambda (x) #t)
                       (lambda (the-exn)
                         (output-response/method conn (responders-servlet-loading uri the-exn) (request-method req)))])
        (define servlet-mutex (make-semaphore 0))
        (define last-servlet (thread-cell-ref current-servlet))
        (define last-servlet-instance-id (thread-cell-ref current-servlet-instance-id))
        (let/cc suspend
          ; Create the session frame
          (with-frame
           (define instance-custodian (make-servlet-custodian))
           (define servlet-path 
             (with-handlers
                 ([void (lambda (e)
                          (raise (make-exn:fail:filesystem:exists:servlet
                                  (exn-message e)
                                  (exn-continuation-marks e))))])
               (url-path->path
                servlet-root
                (url-path->string (url-path uri)))))
           (parameterize ([current-directory (get-servlet-base-dir servlet-path)]
                          [current-custodian instance-custodian]
                          [exit-handler
                           (lambda _
                             (kill-connection! conn)
                             (custodian-shutdown-all instance-custodian))])
             ;; any resources (e.g. threads) created when the
             ;; servlet is loaded should be within the dynamic
             ;; extent of the servlet custodian
             (define the-servlet (cached-load servlet-path))
             (thread-cell-set! current-servlet the-servlet)
             (parameterize ([current-namespace (servlet-namespace the-servlet)])
               (define manager (servlet-manager the-servlet))
               (define data
                 (make-servlet-instance-data
                  servlet-mutex
                  (make-execution-context
                   conn req (lambda () (suspend #t)))))
               (define the-exit-handler
                 (lambda _
                   (kill-connection!
                    (execution-context-connection
                     (servlet-instance-data-context
                      data)))
                   (custodian-shutdown-all instance-custodian)))
               (parameterize ([exit-handler the-exit-handler])
                 (define instance-id ((manager-create-instance manager) data the-exit-handler))
                 (thread-cell-set! current-servlet-instance-id instance-id)
                 (with-handlers ([(lambda (x) #t)
                                  (make-servlet-exception-handler data)])
                   ;; Two possibilities:
                   ;; - module servlet. start : Request -> Void handles
                   ;;   output-response via send/finish, etc.
                   ;; - unit/sig or simple xexpr servlet. These must produce a
                   ;;   response, which is then output by the server.
                   ;; Here, we do not know if the servlet was a module,
                   ;; unit/sig, or Xexpr; we do know whether it produces a
                   ;; response.
                   (define r ((servlet-handler the-servlet) req))
                   (when (response? r)
                     (send/back r))))))))
        (thread-cell-set! current-servlet last-servlet)
        (thread-cell-set! current-servlet-instance-id last-servlet-instance-id)
        (semaphore-post servlet-mutex)))
    
    ;; default-server-instance-expiration-handler : (request -> response)
    (define (default-servlet-instance-expiration-handler req)
      (responders-file-not-found
       (request-uri req)))
    
    ;; make-servlet-exception-handler: servlet-instance -> exn -> void
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
    (define ((make-servlet-exception-handler inst-data) the-exn)
      (define context (servlet-instance-data-context inst-data))
      (define request (execution-context-request context))
      (define resp 
        (responders-servlet
         (request-uri request)
         the-exn))
      ;; Don't handle twice
      (with-handlers ([exn:fail? (lambda (exn) (void))])
        (output-response/method
         (execution-context-connection context)
         resp (request-method request)))
      ((execution-context-suspend context)))
    
    ;; path -> path
    ;; The actual servlet's parent directory.
    (define (get-servlet-base-dir servlet-path)
      (let loop ([path servlet-path])
        (define-values (base name must-be-dir?) (split-path path))
        (or (if must-be-dir?
                (and (directory-exists? path) path)
                (and (directory-exists? base) base))
            (loop base))))
    
    ;; invoke-servlet-continuation: connection request continuation-reference -> void
    ;; pull the continuation out of the table and apply it
    (define (invoke-servlet-continuation conn req instance-id k-id salt)
      (define uri (request-uri req))
      (define servlet-path 
        (url-path->path
         servlet-root
         (url-path->string (url-path uri))))
      (define last-servlet (thread-cell-ref current-servlet))
      (define last-servlet-instance-id (thread-cell-ref current-servlet-instance-id))
      (define the-servlet (cached-load servlet-path))
      (define manager (servlet-manager the-servlet))
      (thread-cell-set! current-servlet the-servlet)
      (thread-cell-set! current-servlet-instance-id instance-id)
      (parameterize ([current-custodian (servlet-custodian the-servlet)])
        (with-handlers ([exn:fail:servlet-manager:no-instance?
                         (lambda (the-exn)
                           (output-response/method
                            conn
                            ((exn:fail:servlet-manager:no-instance-expiration-handler the-exn)
                             req)
                            (request-method req)))]
                        [exn:fail:servlet-manager:no-continuation?
                         (lambda (the-exn)
                           (output-response/method
                            conn
                            ((exn:fail:servlet-manager:no-continuation-expiration-handler the-exn)
                             req)
                            (request-method req)))]
                        [exn:fail:servlet:instance?
                         (lambda (the-exn)
                           (output-response/method
                            conn
                            (default-servlet-instance-expiration-handler
                              req)
                            (request-method req)))])
          (define data ((manager-instance-lookup-data manager) instance-id))
          ; We don't use call-with-semaphore or dynamic-wind because we
          ; always call a continuation. The exit-handler above ensures that
          ; the post is done.
          (semaphore-wait (servlet-instance-data-mutex data))
          (let/cc suspend
            (define k ((manager-continuation-lookup manager) instance-id k-id salt))
            (set-servlet-instance-data-context!
             data
             (make-execution-context
              conn req (lambda () (suspend #t))))
            (k req))
          (semaphore-post (servlet-instance-data-mutex data))))
      (thread-cell-set! current-servlet-instance-id last-servlet-instance-id)
      (thread-cell-set! current-servlet last-servlet))
    
    ;; ************************************************************
    ;; ************************************************************
    ;; Paul's ugly loading code:
    
    ;; cached-load : path -> script, namespace
    ;; timestamps are no longer checked for performance.  The cache must be explicitly
    ;; refreshed (see dispatch).
    (define (cached-load servlet-path)
      (define entry-id (string->symbol (path->string servlet-path)))
      (cache-table-lookup! 
       (unbox config:scripts)
       entry-id
       (lambda ()
         (reload-servlet-script servlet-path))))
    
    ;; exn:i/o:filesystem:servlet-not-found =
    ;; (make-exn:fail:filesystem:exists:servlet str continuation-marks str sym)
    (define-struct (exn:fail:filesystem:exists:servlet
                    exn:fail:filesystem:exists) ())
    
    ;; reload-servlet-script : str -> cache-entry
    ;; The servlet is not cached in the servlet-table, so reload it from the filesystem.
    (define (reload-servlet-script servlet-filename)
      (cond
        [(load-servlet/path servlet-filename)
         => (lambda (entry)
              entry)]
        [else
         (raise (make-exn:fail:filesystem:exists:servlet
                 (string->immutable-string (format "Couldn't find ~a" servlet-filename))
                 (current-continuation-marks) ))]))
    
    ;; load-servlet/path path -> (or/c #f cache-entry)
    ;; given a string path to a filename attempt to load a servlet
    ;; A servlet-file will contain either
    ;;;; A signed-unit-servlet
    ;;;; A module servlet, currently only 'v1
    ;;;;;; (XXX: I don't know what 'typed-model-split-store0 was, so it was removed.)
    ;;;; A response
    (define (load-servlet/path a-path)
      (define (v0.servlet->v1.lambda servlet)
        (lambda (initial-request)
          (invoke-unit/sig servlet servlet^)))
      (define (v0.response->v1.lambda response-path response)
        (letrec ([go (lambda ()
                       (begin
                         (set! go (lambda () (load/use-compiled a-path)))
                         response))])
          (lambda (initial-request) (go))))
      (define (v1.module->v1.lambda timeout start)
        (lambda (initial-request)
          (adjust-timeout! timeout)
          (start initial-request)))
      (parameterize ([current-namespace (config:make-servlet-namespace)]
                     [current-custodian (make-servlet-custodian)])
        ; XXX load/use-compiled breaks errortrace
        (define s (load/use-compiled a-path))
        (cond
          ;; signed-unit servlet
          ; MF: I'd also like to test that s has the correct import signature.
          [(unit/sig? s) 
           (make-servlet (current-custodian)
                         (current-namespace)
                         (create-timeout-manager
                          default-servlet-instance-expiration-handler
                          timeouts-servlet-connection
                          timeouts-default-servlet)
                         (v0.servlet->v1.lambda s))]
          ; FIX - reason about exceptions from dynamic require (catch and report if not already)
          ;; module servlet
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
                                 timeouts-servlet-connection
                                 timeouts-default-servlet)
                                (v1.module->v1.lambda timeout start)))]
               [(v2-transitional) ; XXX: Undocumented
                (let ([start (dynamic-require module-name 'start)]
                      [manager (with-handlers
                                   ([exn:fail:contract?
                                     (lambda (exn)
                                       (define timeout (dynamic-require module-name 'timeout))
                                       (define instance-expiration-handler
                                         (dynamic-require module-name 'instance-expiration-handler))
                                       (create-timeout-manager
                                        instance-expiration-handler
                                        timeouts-servlet-connection
                                        timeout))])
                                 (dynamic-require module-name 'manager))])
                  (make-servlet (current-custodian)
                                (current-namespace)
                                manager
                                start))]
               [else
                (error 'load-servlet/path "unknown servlet version ~e" version)]))]
          ;; response
          [(response? s)
           (make-servlet (current-custodian)
                         (current-namespace)
                         (create-timeout-manager
                          default-servlet-instance-expiration-handler
                          timeouts-servlet-connection
                          timeouts-default-servlet)
                         (v0.response->v1.lambda s a-path))]
          [else
           (error 'load-servlet/path "Loading ~e produced ~n~e~n instead of a servlet." a-path s)])))
    
    (define svt-bin-re (regexp "^/servlets(;.*\\*.*\\*.*)?/.*"))
    (define (servlet-bin? str)
      (regexp-match svt-bin-re str))
    
    ;; return dispatcher
    (lambda (conn req)
      (define-values (uri method path) (decompose-request req))
      (cond [(string=? "/conf/refresh-servlets" path)
             ;; more here - this is broken - only out of date or specifically mentioned
             ;; scripts should be flushed.  This destroys persistent state!
             (cache-table-clear! (unbox config:scripts))
             (output-response/method
              conn
              (responders-servlets-refreshed)
              method)]
            [(servlet-bin? path)
             (adjust-connection-timeout!
              conn
              timeouts-servlet-connection)
             ;; more here - make timeouts proportional to size of bindings
             (servlet-content-producer conn req)]
            [else
             (next-dispatcher)]))))