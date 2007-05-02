(module dispatch-servlets mzscheme
  (require (lib "url.ss" "net")
           (lib "kw.ss")
           (lib "plt-match.ss")
           (lib "string.ss")
           (lib "contract.ss"))
  (require "dispatch.ss"
           "../private/web-server-structs.ss"
           "../private/connection-manager.ss"
           "../private/response.ss"
           "../response-structs.ss"
           "../servlet.ss"
           "../private/configuration.ss"
           "../private/util.ss"
           "../managers/manager.ss"
           "../managers/timeouts.ss"
           "../managers/lru.ss"
           "../managers/none.ss"
           "../private/url.ss"
           "../private/servlet.ss"
           "../private/cache-table.ss")  
  (provide/contract
   [interface-version dispatcher-interface-version?])
  (provide ; XXX contract improve
   ; XXX contract kw
   make)
  
  (define (url-path->path base p)
    (path->complete-path
     (let ([path-elems (regexp-split #rx"/" p)])
       ;; Servlets can have extra stuff after them
       (let ([build-path
              (lambda (b p)
                (if (string=? p "")
                    b
                    (build-path b p)))])
         (let loop
           ([p-e (if (string=? (car path-elems) "")
                     (cddr path-elems)
                     (cdr path-elems))]
            [f (build-path base
                           (if (string=? (car path-elems) "")
                               (cadr path-elems)
                               (car path-elems)))])
           (cond
             [(null? p-e)
              f]
             [(directory-exists? f)
              (loop (cdr p-e) (build-path f (car p-e)))]
             [(file-exists? f)
              f]
             [else
              ;; Don't worry about e.g. links for now
              f]))))))
  
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
        (define response
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
                 (define ctxt                   
                   (make-execution-context
                    conn req suspend))
                 (define data
                   (make-servlet-instance-data
                    servlet-mutex))
                 (define the-exit-handler
                   (lambda _
                     (define ectxt
                       (thread-cell-ref current-execution-context))
                     (when ectxt
                       (kill-connection!
                        (execution-context-connection ectxt)))
                     (custodian-shutdown-all instance-custodian)))
                 (thread-cell-set! current-execution-context ctxt)
                 (parameterize ([exit-handler the-exit-handler])
                   (define instance-id ((manager-create-instance manager) data the-exit-handler))
                   (thread-cell-set! current-servlet-instance-id instance-id)
                   ((manager-instance-lock! manager) instance-id)
                   (parameterize ([exit-handler (lambda x
                                                  ((manager-instance-unlock! manager) instance-id)
                                                  (the-exit-handler x))])
                     (with-handlers ([(lambda (x) #t)
                                      (make-servlet-exception-handler)])
                       ;; Two possibilities:
                       ;; - module servlet. start : Request -> Void handles
                       ;;   output-response via send/finish, etc.
                       ;; - unit/sig or simple xexpr servlet. These must produce a
                       ;;   response, which is then output by the server.
                       ;; Here, we do not know if the servlet was a module,
                       ;; unit/sig, or Xexpr; we do know whether it produces a
                       ;; response.
                       (send/back ((servlet-handler the-servlet) req)))
                     ((manager-instance-unlock! manager) instance-id))))))))
        (output-response conn response)
        (semaphore-post servlet-mutex)
        (thread-cell-set! current-execution-context #f)
        (thread-cell-set! current-servlet #f)
        (thread-cell-set! current-servlet-instance-id #f)))
    
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
    (define ((make-servlet-exception-handler) the-exn)
      (define context (thread-cell-ref current-execution-context))
      (define request (execution-context-request context))
      (define resp 
        (responders-servlet
         (request-uri request)
         the-exn))
      ((execution-context-suspend context) resp))
    
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
          ((manager-instance-lock! manager) instance-id)
          ; We don't use call-with-semaphore or dynamic-wind because we
          ; always call a continuation. The exit-handler above ensures that
          ; the post is done.
          (semaphore-wait (servlet-instance-data-mutex data))
          (with-handlers ([exn? (lambda (exn)
                                  (semaphore-post (servlet-instance-data-mutex data))
                                  (raise exn))])
            (let ([response
                   (let/cc suspend
                     (thread-cell-set! current-execution-context
                                       (make-execution-context
                                        conn req suspend))
                     (let ([kcb ((manager-continuation-lookup manager) instance-id k-id salt)])
                       ((custodian-box-value kcb) req)))])
              (output-response conn response))
            (semaphore-post (servlet-instance-data-mutex data)))))
      ((manager-instance-unlock! manager) instance-id)
      (thread-cell-set! current-execution-context #f)
      (thread-cell-set! current-servlet-instance-id #f)
      (thread-cell-set! current-servlet #f))
    
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
                 (format "Couldn't find ~a" servlet-filename)
                 (current-continuation-marks) ))]))
    
    ;; load-servlet/path path -> (or/c #f cache-entry)
    ;; given a string path to a filename attempt to load a servlet
    ;; A servlet-file will contain either
    ;;;; A signed-unit-servlet
    ;;;; A module servlet, currently only 'v1
    ;;;;;; (XXX: I don't know what 'typed-model-split-store0 was, so it was removed.)
    ;;;; A response
    (define (load-servlet/path a-path)
      (define (v0.response->v1.lambda response-path response)
        (define go
          (box
           (lambda ()
             (set-box! go (lambda () (load/use-compiled a-path)))
             response)))
        (lambda (initial-request)
          ((unbox go))))
      (define (v1.module->v1.lambda timeout start)
        (lambda (initial-request)
          (adjust-timeout! timeout)
          (start initial-request)))
      (parameterize ([current-namespace (config:make-servlet-namespace)]
                     [current-custodian (make-servlet-custodian)])
        ; XXX load/use-compiled breaks errortrace
        (define s (load/use-compiled a-path))
        (cond
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
                                 timeout)
                                (v1.module->v1.lambda timeout start)))]
               [(v2 v2-transitional) ; XXX: Undocumented
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
