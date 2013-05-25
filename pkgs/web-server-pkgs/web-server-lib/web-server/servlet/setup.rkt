#lang racket/base
(require racket/contract
         racket/match
         racket/serialize
         web-server/managers/manager
         web-server/managers/timeouts
         web-server/managers/none
         web-server/lang/stuff-url
         web-server/stuffers/stuffer
         (only-in web-server/lang/web
                  initialize-servlet
                  make-stateless-servlet)
         web-server/http
         web-server/servlet/web
         web-server/configuration/namespace
         web-server/servlet/servlet-structs
         web-server/private/web-server-structs
         web-server/private/servlet
         web-server/private/util)

(define path->servlet/c (path? . -> . servlet?))
(provide/contract
 [path->servlet/c contract?]
 [make-default-path->servlet
  (->* ()
       (#:make-servlet-namespace make-servlet-namespace/c
                                 #:timeouts-default-servlet number?)
       path->servlet/c)])

(define (v0.response->v1.lambda response response-path)
  (lambda (initial-request)
    response))

(define (make-v1.servlet directory timeout start)
  (make-v2.servlet 
   directory                   
   (create-timeout-manager
    #f timeout timeout)
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
     
     (with-handlers ([exn:fail:servlet-manager:no-instance?
                      (lambda (the-exn)
                        ((exn:fail:servlet-manager:no-instance-expiration-handler the-exn) req))]
                     [exn:fail:servlet-manager:no-continuation?
                      (lambda (the-exn)
                        ((exn:fail:servlet-manager:no-continuation-expiration-handler the-exn) req))])
       
       (define-values (instance-id handler)
         (cond
           [(continuation-url? uri)
            => (match-lambda
                 [(list instance-id k-id salt)
                  (values instance-id
                          (custodian-box-value ((manager-continuation-lookup manager) instance-id k-id salt)))])]
           [else
            (define eh (exit-handler))
            (values ((manager-create-instance manager) (λ () (eh #f)))
                    start)]))
       
       (parameterize ([current-servlet-instance-id instance-id])
         (handler req))))))

(define (make-stateless.servlet directory stuffer manager start)
  (define eh (exit-handler))
  (define instance-id
    ((manager-create-instance manager) (λ () (eh #f))))
  (define ses 
    (make-stateless-servlet
     (current-custodian) (current-namespace)
     manager
     directory
     (lambda (req) (error "Session not initialized"))
     stuffer))
  (parameterize ([current-directory directory]
                 [current-servlet-instance-id instance-id]
                 [current-servlet ses])    
    (set-servlet-handler! ses (initialize-servlet start)))
  ses)

(require racket/runtime-path)
(define-runtime-module-path web-server/private/servlet:module-path web-server/private/servlet)
(define-runtime-module-path web-server/http:module-path web-server/http)
(define common-module-specs
  (list web-server/private/servlet:module-path
        web-server/http:module-path))

(define-runtime-module-path web-server/servlet/web:module-path web-server/servlet/web)
(define-runtime-module-path web-server/servlet/web-cells:module-path web-server/servlet/web-cells)
(define servlet-module-specs
  (list 'web-server/servlet/web
        #;web-server/servlet/web:module-path ; XXX Enabling results in error
        web-server/servlet/web-cells:module-path))

(define-runtime-module-path web-server/lang/web-cells:module-path web-server/lang/web-cells)
(define-runtime-module-path web-server/lang/web:module-path web-server/lang/web)
(define-runtime-module-path web-server/lang/abort-resume:module-path web-server/lang/abort-resume)
(define lang-module-specs
  (list web-server/lang/web-cells:module-path 
        #;web-server/lang/abort-resume:module-path ; XXX Enabling results in error
        'web-server/lang/abort-resume
        #;web-server/lang/web:module-path ; XXX Enabling results in error
        'web-server/lang/web))
(define default-module-specs
  (append common-module-specs
          servlet-module-specs
          lang-module-specs))
(provide/contract
 [make-v1.servlet (path-string? integer? (request? . -> . can-be-response?) . -> . servlet?)]
 [make-v2.servlet (path-string? manager? (request? . -> . can-be-response?) . -> . servlet?)]
 [make-stateless.servlet (path-string? (stuffer/c serializable? bytes?) manager? (request? . -> . can-be-response?) . -> . servlet?)]
 [default-module-specs (listof (or/c resolved-module-path? module-path?))])

(define (make-default-path->servlet #:make-servlet-namespace [make-servlet-namespace (make-make-servlet-namespace)]
                                    #:timeouts-default-servlet [timeouts-default-servlet 30])
  (lambda (a-path)
    (parameterize ([current-namespace (make-servlet-namespace
                                       #:additional-specs
                                       default-module-specs)]
                   [current-custodian (make-servlet-custodian)])
      (let* ([path-string (path->string a-path)]
             [path-sym (string->symbol path-string)]
             [neg-blame 'web-server]
             [pos-blame path-sym]
             [module-name `(file ,path-string)]
             [loc (make-srcloc a-path #f #f #f #f)]
             [s (load/use-compiled a-path)])
        (cond
          [(void? s)
           (let ([version 
                  (contract (symbols 'v1 'v2 'stateless) 
                            (dynamic-require module-name 'interface-version)
                            pos-blame neg-blame
                            "interface-version" loc)])
             (case version
               [(v1)
                (let ([timeout (contract number?
                                         (dynamic-require module-name 'timeout)
                                         pos-blame neg-blame
                                         "timeout" loc)]
                      [start (contract (request? . -> . can-be-response?)
                                       (dynamic-require module-name 'start)
                                       pos-blame neg-blame
                                       "start" loc)])
                  (make-v1.servlet (directory-part a-path) timeout start))]
               [(v2)
                (let ([start (contract (request? . -> . can-be-response?)
                                       (dynamic-require module-name 'start)
                                       pos-blame neg-blame
                                       "start" loc)]
                      [manager (contract manager?
                                         (dynamic-require module-name 'manager)
                                         pos-blame neg-blame
                                         "manager" loc)])
                  (make-v2.servlet (directory-part a-path) manager start))]
               [(stateless)
                (let ([start (contract (request? . -> . can-be-response?)
                                       (dynamic-require module-name 'start)
                                       pos-blame neg-blame
                                       "start" loc)]
                      [manager (contract manager?
                                         (dynamic-require module-name 'manager 
                                                          (lambda () (create-none-manager (lambda (req) (error "No continuations!")))))
                                         pos-blame neg-blame
                                         "manager" loc)]
                      [stuffer (contract (stuffer/c serializable? bytes?)
                                         (dynamic-require module-name 'stuffer (lambda () default-stuffer))
                                         pos-blame neg-blame
                                         "stuffer" loc)])
                  (make-stateless.servlet (directory-part a-path) stuffer manager start))]))]
          [else
           (make-v1.servlet (directory-part a-path) timeouts-default-servlet
                            (v0.response->v1.lambda 
                             (contract response? (response/xexpr s)
                                       pos-blame neg-blame
                                       path-string loc)
                             a-path))])))))
