#lang scheme/base
(require mzlib/list
         scheme/contract
         web-server/private/session
         (only-in "../lang/web.ss"
                  initialize-servlet)           
         web-server/lang/web-cells
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
 [interface-version dispatcher-interface-version?]
 [make
  (->* (#:url->path url-path?)
       (#:make-servlet-namespace make-servlet-namespace?
                                  #:responders-servlet-loading (url? any/c . -> . response?)
                                  #:responders-servlet (url? any/c . -> . response?))
       dispatcher?)])

; XXX url->servlet
; XXX optional session manager
(define interface-version 'v1)
(define (make #:url->path url->path
              #:make-servlet-namespace [make-servlet-namespace (make-make-servlet-namespace)]
              #:responders-servlet-loading [responders-servlet-loading servlet-loading-responder]
              #:responders-servlet [responders-servlet (gen-servlet-responder "servlet-error.html")])
  
  ;; dispatch : connection request -> void
  (define (dispatch conn req)
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
               (define cust (make-custodian (current-server-custodian)))
               (define ns (make-servlet-namespace
                           #:additional-specs
                           '(web-server/lang/web-cells
                             web-server/lang/abort-resume
                             web-server/private/session
                             web-server/private/request-structs)))
               (define ses (new-session cust ns uri url-servlet-paths))
               (parameterize ([current-custodian cust]
                              [current-directory (directory-part a-path)]
                              [current-namespace ns]
                              [current-session ses])
                 (define start
                   (dynamic-require `(file ,(path->string a-path))
                                    'start))
                 (set-session-servlet! ses (initialize-servlet start)))
               (install-session ses url-servlet-paths)
               ses)]))
        (parameterize ([current-custodian (session-cust ses)]
                       [current-namespace (session-namespace ses)]
                       [current-session ses])
          (with-handlers ([exn?
                           (lambda (the-exn)
                             (output-response/method
                              conn
                              (responders-servlet uri the-exn)
                              (request-method req)))])
            (output-response conn ((session-servlet ses) req)))))))
  
  dispatch)
