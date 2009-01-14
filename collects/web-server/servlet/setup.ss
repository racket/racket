#lang scheme/base
(require mzlib/plt-match
         scheme/contract)
(require web-server/managers/manager
         web-server/managers/timeouts
         web-server/managers/none
         (only-in web-server/lang/web
                  initialize-servlet)
         web-server/http
         web-server/servlet/web
         web-server/configuration/namespace
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
            (values ((manager-create-instance manager) (exit-handler))
                    start)]))
       
       (parameterize ([current-servlet-instance-id instance-id])
         (handler req))))))

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
    web-server/http))

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
 [make-v1.servlet (path-string? integer? (request? . -> . response?) . -> . servlet?)]
 [make-v2.servlet (path-string? manager? (request? . -> . response?) . -> . servlet?)]
 [make-stateless.servlet (path-string? (request? . -> . response?) . -> . servlet?)]
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
         (let* ([path-string (path->string a-path)]
                [path-sym (string->symbol path-string)]
                [neg-blame 'web-server]
                [pos-blame path-sym]
                [module-name `(file ,path-string)]
                [mk-loc
                 (lambda (name)
                   (list (make-srcloc a-path #f #f #f #f)
                         name))]
                [version 
                 (contract (symbols 'v1 'v2 'stateless) 
                           (dynamic-require module-name 'interface-version)
                           pos-blame neg-blame
                           (mk-loc "interface-version"))])
           (case version
             [(v1)
              (let ([timeout (contract number?
                                       (dynamic-require module-name 'timeout)
                                       pos-blame neg-blame
                                       (mk-loc "timeout"))]
                    [start (contract (request? . -> . response?)
                                     (dynamic-require module-name 'start)
                                     pos-blame neg-blame
                                     (mk-loc "start"))])
                (make-v1.servlet (directory-part a-path) timeout start))]
             [(v2)
              (let ([start (contract (request? . -> . response?)
                                     (dynamic-require module-name 'start)
                                     pos-blame neg-blame
                                     (mk-loc "start"))]
                    [manager (contract manager?
                                       (dynamic-require module-name 'manager)
                                       pos-blame neg-blame
                                       (mk-loc "manager"))])
                (make-v2.servlet (directory-part a-path) manager start))]
             [(stateless)
              (let ([start (contract (request? . -> . response?)
                                     (dynamic-require module-name 'start)
                                     pos-blame neg-blame
                                     (mk-loc "start"))])
                (make-stateless.servlet (directory-part a-path) start))]))]
        [(response? s)
         (make-v1.servlet (directory-part a-path) timeouts-default-servlet
                          (v0.response->v1.lambda s a-path))]
        [else
         (error 'path->servlet
                "Loading ~e produced ~n~e~n instead of either (1) a response or (2) nothing and exports 'interface-version" a-path s)]))))
