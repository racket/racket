#lang scheme/base
(require mzlib/plt-match
         scheme/contract)
(require "dispatch.ss"
         "../private/web-server-structs.ss"
         "../private/connection-manager.ss"
         web-server/managers/none
         (only-in "../lang/web.ss"
                  initialize-servlet)
         "../private/response.ss"
         "../private/request-structs.ss"
         "../private/response-structs.ss"
         "../servlet/web-cells.ss"
         "../servlet/web.ss"
         net/url
         "../dispatchers/filesystem-map.ss"
         "../configuration/responders.ss"
         "../configuration/namespace.ss"
         "../managers/manager.ss"
         "../managers/timeouts.ss"
         (except-in "../private/servlet.ss"
                    servlet-prompt)
         "../private/cache-table.ss"
         "../private/util.ss")
(provide/contract
 [interface-version dispatcher-interface-version/c])
(define interface-version 'v1)

; -----
(define path->servlet/c (path? . -> . servlet?))
(provide/contract
 [path->servlet/c contract?]
 [make-default-path->servlet
  (->* ()
       (#:make-servlet-namespace make-servlet-namespace/c
                                 #:timeouts-default-servlet number?)
       path->servlet/c)])

(define (v0.response->v1.lambda response response-path)
  (define go
    (box
     (lambda ()
       (set-box! go (lambda () (load/use-compiled response-path)))
       response)))
  (lambda (initial-request)
    ((unbox go))))

(define (make-v1.servlet directory timeout start)
  (make-v2.servlet 
   directory                   
   (create-timeout-manager
    default-servlet-instance-expiration-handler
    timeout
    timeout)
   (lambda (initial-request)
     (adjust-timeout! timeout)
     (start initial-request))))

(define (make-v2.servlet directory manager start)
  (make-servlet 
   (current-custodian)
   (current-namespace)
   manager
   directory                
   (lambda (req)
     (define uri (request-uri req))
     
     (define-values (instance-id handler)
       (cond
         [(continuation-url? uri)
          => (match-lambda
               [(list instance-id k-id salt)
                (values instance-id
                        (custodian-box-value ((manager-continuation-lookup manager) instance-id k-id salt)))])]
         [else
          (values ((manager-create-instance manager) (exit-handler))
                  start)]))
     
     (parameterize ([current-servlet-instance-id instance-id])
       (handler req)))))

(define (make-stateless.servlet directory start)
  (define ses 
    (make-servlet
     (current-custodian) (current-namespace)
     (create-none-manager (lambda (req) (error "No continuations!")))
     directory
     (lambda (req) (error "Session not initialized"))))
  (parameterize ([current-directory directory]
                 [current-servlet ses])    
    (set-servlet-handler! ses (initialize-servlet start)))
  ses)

(define common-module-specs
  '(web-server/private/servlet
    web-server/private/request-structs
    web-server/private/response-structs))

(define servlet-module-specs
  '(web-server/servlet/web
    web-server/servlet/web-cells))
(define lang-module-specs
  '(web-server/lang/web-cells
    web-server/lang/abort-resume))
(define default-module-specs
  (append common-module-specs
          servlet-module-specs
          lang-module-specs))
(provide/contract
 [make-v1.servlet (path? integer? (request? . -> . response?) . -> . servlet?)]
 [make-v2.servlet (path? manager? (request? . -> . response?) . -> . servlet?)]
 [make-stateless.servlet (path? (request? . -> . response?) . -> . servlet?)]
 [default-module-specs (listof module-path?)])

(define (make-default-path->servlet #:make-servlet-namespace [make-servlet-namespace (make-make-servlet-namespace)]
                                    #:timeouts-default-servlet [timeouts-default-servlet 30])
  (lambda (a-path)
    (parameterize ([current-namespace (make-servlet-namespace
                                       #:additional-specs
                                       default-module-specs)]
                   [current-custodian (make-servlet-custodian)])
      (define s (load/use-compiled a-path))
      (cond
        [(void? s)
         (let* ([module-name `(file ,(path->string a-path))]
                [version (dynamic-require module-name 'interface-version)])
           (case version
             [(v1)
              (let ([timeout (dynamic-require module-name 'timeout)]
                    [start (dynamic-require module-name 'start)])
                (make-v1.servlet (directory-part a-path) timeout start))]
             [(v2)
              (let ([start (dynamic-require module-name 'start)]
                    [manager (dynamic-require module-name 'manager)])
                (make-v2.servlet (directory-part a-path) manager start))]
             [(stateless)
              (let ([start (dynamic-require module-name 'start)])
                (make-stateless.servlet (directory-part a-path) start))]
             [else
              (error 'path->servlet "unknown servlet version ~e, must be 'v1, 'v2, or 'stateless" version)]))]
        [(response? s)
         (make-v1.servlet (directory-part a-path) timeouts-default-servlet
                          (v0.response->v1.lambda s a-path))]
        [else
         (error 'path->servlet
                "Loading ~e produced ~n~e~n instead of either (1) a response or (2) nothing and exports 'interface-version" a-path s)]))))

; -----
(define url->servlet/c (url? . -> . servlet?))
(provide/contract
 [url->servlet/c contract?]
 [make-cached-url->servlet
  (-> url->path/c
      path->servlet/c
      (values (-> void)
              url->servlet/c))])

(define (make-cached-url->servlet         
         url->path 
         path->servlet)
  (define config:scripts (make-cache-table))
  (values (lambda ()
            ;; This is broken - only out of date or specifically mentioned scripts should be flushed.  This destroys persistent state!
            (cache-table-clear! config:scripts))
          (lambda (uri)
            (define-values (servlet-path _)
              (with-handlers
                  ([void (lambda (e)
                           (raise (make-exn:fail:filesystem:exists
                                   (exn-message e)
                                   (exn-continuation-marks e))))])
                (url->path uri)))
            (cache-table-lookup! config:scripts
                                 (string->symbol (path->string servlet-path))
                                 (lambda () (path->servlet servlet-path))))))

; -----
(provide/contract
 [make (->* (url->servlet/c)
            (#:responders-servlet-loading (url? any/c . -> . response?)
                                          #:responders-servlet (url? any/c . -> . response?))
            dispatcher/c)])

;; default-server-instance-expiration-handler : (request -> response)
(define (default-servlet-instance-expiration-handler req)
  (next-dispatcher))

(define (make url->servlet
              #:responders-servlet-loading [responders-servlet-loading servlet-loading-responder]
              #:responders-servlet [responders-servlet servlet-error-responder])
  (lambda (conn req)
    (define uri (request-uri req))
    (define instance-custodian (make-servlet-custodian))      
    (parameterize ([current-custodian instance-custodian]
                   [current-execution-context (make-execution-context req)]
                   [exit-handler
                    (lambda _
                      (kill-connection! conn)
                      (custodian-shutdown-all instance-custodian))])
      (define response
        (with-handlers ([exn:fail:filesystem:exists?
                         (lambda (the-exn) (next-dispatcher))]
                        [exn:fail:servlet-manager:no-instance?
                         (lambda (the-exn)
                           ((exn:fail:servlet-manager:no-instance-expiration-handler the-exn) req))]
                        [exn:fail:servlet-manager:no-continuation?
                         (lambda (the-exn)
                           ((exn:fail:servlet-manager:no-continuation-expiration-handler the-exn) req))]
                        [exn:fail:servlet:instance?
                         (lambda (the-exn)
                           (default-servlet-instance-expiration-handler req))]
                        [(lambda (x) #t)
                         (lambda (the-exn) (responders-servlet-loading uri the-exn))])
          (define the-servlet (url->servlet uri))
          (parameterize ([current-servlet the-servlet]
                         [current-custodian (servlet-custodian the-servlet)]
                         [current-directory (servlet-directory the-servlet)]
                         [current-namespace (servlet-namespace the-servlet)])
            (with-handlers ([(lambda (x) #t)
                             (lambda (exn)
                               (responders-servlet
                                (request-uri req)
                                exn))])
              (call-with-continuation-barrier 
               (lambda ()
                 (call-with-continuation-prompt
                  (lambda ()
                    ((servlet-handler the-servlet) req))
                  servlet-prompt)))))))
      
      (output-response conn response))))