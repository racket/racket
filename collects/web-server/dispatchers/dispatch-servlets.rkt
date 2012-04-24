#lang racket/base
(require racket/contract)
(require web-server/servlet/setup
         web-server/servlet/servlet-structs
         web-server/managers/manager
         web-server/http
         web-server/http/response
         net/url
         web-server/dispatchers/dispatch
         web-server/dispatchers/filesystem-map
         web-server/configuration/responders
         web-server/private/connection-manager
         web-server/private/web-server-structs
         web-server/private/servlet
         web-server/private/cache-table)
(provide/contract
 [interface-version dispatcher-interface-version/c])
(define interface-version 'v1)

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
            (#:responders-servlet-loading (url? any/c . -> . can-be-response?)
                                          #:responders-servlet (url? any/c . -> . can-be-response?))
            dispatcher/c)])

(define (make url->servlet
          #:responders-servlet-loading [responders-servlet-loading servlet-loading-responder]
          #:responders-servlet [responders-servlet servlet-error-responder])
  (lambda (conn req)
    (define uri (request-uri req))
    (define instance-custodian (make-servlet-custodian))      
    (parameterize ([current-custodian instance-custodian]
                   [current-execution-context (make-execution-context req)]
                   [exit-handler
                    (lambda (r)
                      (kill-connection! conn)
                      (custodian-shutdown-all instance-custodian))])
      (define maybe-response
        (with-handlers ([exn:fail:filesystem:exists?
                         (lambda (the-exn) (next-dispatcher))]
                        [exn:dispatcher? raise]
                        [(lambda (x) #t)
                         (lambda (the-exn) (responders-servlet-loading uri the-exn))])
          (define the-servlet (url->servlet uri))
          (parameterize ([current-servlet the-servlet]
                         [current-custodian (servlet-custodian the-servlet)]
                         [current-directory (servlet-directory the-servlet)]
                         [current-namespace (servlet-namespace the-servlet)])
            (with-handlers ([exn:dispatcher? raise]
                            [(lambda (x) #t)
                             (lambda (exn) (responders-servlet uri exn))])
              (call-with-continuation-barrier 
               (lambda ()
                 (call-with-continuation-prompt
                  (lambda ()
                    ((servlet-handler the-servlet) req))
                  servlet-prompt)))))))
      
      (output-response conn (any->response maybe-response)))))
