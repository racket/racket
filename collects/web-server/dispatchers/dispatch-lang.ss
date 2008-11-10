#lang scheme/base
(require mzlib/list
         scheme/contract
         (only-in "../lang/web.ss"
                  initialize-servlet)           
         web-server/lang/web-cells
         web-server/managers/none
         web-server/private/servlet
         "../private/request-structs.ss"
         "../private/response-structs.ss"
         "dispatch.ss"
         net/url
         "../private/web-server-structs.ss"
         "../private/util.ss"
         "../private/response.ss"
         "../dispatchers/filesystem-map.ss"
         "../configuration/namespace.ss"
         "../configuration/responders.ss")

(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make
  (->* (#:url->path url->path/c)
       (#:make-servlet-namespace make-servlet-namespace/c
                                 #:responders-servlet-loading (url? any/c . -> . response?)
                                 #:responders-servlet (url? any/c . -> . response?))
       dispatcher/c)])

;; HACK
(define the-session-table (make-weak-hash))

(define (install-session ses paths)
  (hash-set! the-session-table paths ses))

;; lookup-session : (listof string) -> (union session #f)
(define (lookup-session paths)
  (hash-ref the-session-table paths 
            (lambda () #f)))
;; /HACK


(define interface-version 'v1)
(define (make #:url->path url->path
              #:make-servlet-namespace [make-servlet-namespace (make-make-servlet-namespace)]
              #:responders-servlet-loading [responders-servlet-loading servlet-loading-responder]
              #:responders-servlet [responders-servlet servlet-error-responder])
  (lambda (conn req)
    (define uri (request-uri req))
    (with-handlers ([void (lambda (exn) (next-dispatcher))])
      (define-values (a-path url-servlet-path) (url->path uri))
      (define url-servlet-paths (map path->string url-servlet-path))
      (with-handlers ([exn?
                       (lambda (the-exn)
                         (output-response/method
                          conn
                          (responders-servlet-loading uri the-exn)
                          (request-method req)))])
        
        (define ses 
          (cond
            [(lookup-session url-servlet-paths)
             => (lambda (ses) ses)]
            [else
             (let ()
               (define cust (make-servlet-custodian))
               (define ns (make-servlet-namespace
                           #:additional-specs
                           '(web-server/lang/web-cells
                             web-server/lang/abort-resume                             
                             web-server/private/servlet
                             web-server/private/request-structs)))
               (define dir (directory-part a-path))
               (define ses 
                 (make-servlet
                  cust ns
                  (create-none-manager (lambda (req) (error "No continuations!")))
                  dir
                  (lambda (req) (error "session not initialized"))))
               (parameterize ([current-custodian cust]
                              [current-directory dir]
                              [current-namespace ns]
                              [current-execution-context (make-execution-context req)]
                              [current-servlet ses])
                 (define start
                   (dynamic-require `(file ,(path->string a-path))
                                    'start))
                 (set-servlet-handler! ses (initialize-servlet start)))
               (install-session ses url-servlet-paths)
               ses)]))
        (parameterize ([current-custodian (servlet-custodian ses)]
                       [current-directory (servlet-directory ses)]
                       [current-namespace (servlet-namespace ses)]
                       [current-execution-context (make-execution-context req)]
                       [current-servlet ses])
          (with-handlers ([exn?
                           (lambda (the-exn)
                             (output-response/method
                              conn
                              (responders-servlet uri the-exn)
                              (request-method req)))])
            (output-response conn ((servlet-handler ses) req))))))))