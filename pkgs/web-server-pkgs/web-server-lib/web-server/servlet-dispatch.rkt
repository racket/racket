;; Derived from plai/web/server, which was based on an older version
;; of this Also derived from planet/untyped/instaservlet
#lang racket/base
(require (prefix-in net: net/sendurl)
         racket/match
         racket/local
         racket/contract
         racket/async-channel
         racket/list
         racket/unit
         racket/serialize
         net/tcp-unit
         net/tcp-sig
         net/url
         unstable/contract
         net/ssl-tcp-unit)
(require web-server/web-server
         web-server/managers/lru
         web-server/managers/manager
         web-server/configuration/namespace
         web-server/configuration/responders
         web-server/http
         web-server/stuffers
         web-server/servlet/setup
         web-server/servlet/servlet-structs
         web-server/dispatchers/dispatch
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets))

(define send-url (make-parameter net:send-url))

(provide/contract
 [dispatch/servlet (((request? . -> . can-be-response?))
                    (#:regexp regexp?
                              #:current-directory path-string?
                              #:stateless? boolean?
                              #:stuffer (stuffer/c serializable? bytes?)
                              #:manager manager?
                              #:responders-servlet-loading (url? any/c . -> . can-be-response?)
                              #:responders-servlet (url? any/c . -> . can-be-response?))
                    . ->* .
                    dispatcher/c)]
 [serve/launch/wait (((semaphore? . -> . dispatcher/c))
                     (#:launch-path (or/c false/c string?)
                                    #:connection-close? boolean?
                                    #:banner? boolean?
                                    #:listen-ip (or/c false/c string?)
                                    #:port tcp-listen-port?
                                    #:max-waiting exact-nonnegative-integer?
                                    #:ssl-cert (or/c false/c path-string?)
                                    #:ssl-key (or/c false/c path-string?))
                     . ->* .
                     void)])

(define (dispatch/servlet 
         start
         #:regexp
         [servlet-regexp #rx""]
         #:current-directory 
         [servlet-current-directory (current-directory)]              
         #:stateless? 
         [stateless? #f]
         #:stuffer
         [stuffer default-stuffer]
         #:responders-servlet-loading
         [responders-servlet-loading servlet-loading-responder]
         #:responders-servlet 
         [responders-servlet servlet-error-responder]
         #:manager
         [manager
          (make-threshold-LRU-manager
           (lambda (request)
             `(html (head (title "Page Has Expired."))
                    (body (p "Sorry, this page has expired. Please go back."))))
           (* 64 1024 1024))])
  (define servlet-box (box #f))
  (define namespace-now (current-namespace))
  (filter:make
   servlet-regexp
   (servlets:make
    #:responders-servlet-loading responders-servlet-loading
    #:responders-servlet responders-servlet
    (lambda (url)
      (or (unbox servlet-box)
          (let ([servlet
                 (parameterize ([current-custodian (make-custodian)]
                                [current-namespace namespace-now])
                   (if stateless?
                       (make-stateless.servlet servlet-current-directory stuffer manager start)
                       (make-v2.servlet servlet-current-directory manager start)))])
            (set-box! servlet-box servlet)
            servlet))))))

(define (serve/launch/wait
         dispatcher
         
         #:connection-close?
         [connection-close? #f]
         #:launch-path
         [launch-path #f]          
         #:banner?
         [banner? #t]
         
         #:listen-ip
         [listen-ip "127.0.0.1"]
         #:port
         [port-arg 8000]
         #:max-waiting
         [max-waiting 511]
         #:ssl-cert
         [ssl-cert #f]
         #:ssl-key
         [ssl-key #f])
  (define ssl? (and ssl-cert ssl-key))
  (define sema (make-semaphore 0))
  (define confirm-ch (make-async-channel 1))
  (define shutdown-server
    (serve #:confirmation-channel confirm-ch
           #:connection-close? connection-close?
           #:dispatch (dispatcher sema)
           #:listen-ip listen-ip
           #:port port-arg
           #:max-waiting max-waiting
           #:tcp@ (if ssl?
                      (let ()
                        (define-unit-binding ssl-tcp@
                          (make-ssl-tcp@
                           ssl-cert ssl-key
                           #f #f #f #f #f)
                          (import) (export tcp^))
                        ssl-tcp@)
                      tcp@)))
  (define serve-res (async-channel-get confirm-ch))
  (if (exn? serve-res)
      (begin
        (when banner? (eprintf "There was an error starting the Web server.\n"))
        (match serve-res
          [(app exn-message (regexp "tcp-listen: listen on .+ failed \\(Address already in use; errno=.+\\)" (list _)))
           (when banner? (eprintf "\tThe TCP port (~a) is already in use.\n" port-arg))]
          [_
           (void)]))
      (local [(define port serve-res)
              (define server-url
                (string-append (if ssl? "https" "http")
                               "://localhost"
                               (if (and (not ssl?) (= port 80))
                                   "" (format ":~a" port))))]
        (when launch-path
          ((send-url) (string-append server-url launch-path) #t))
        (when banner?
          (printf "Your Web application is running at ~a.\n" 
                  (if launch-path 
                      (string-append server-url launch-path)
                      server-url))
          (printf "Stop this program at any time to terminate the Web Server.\n")
          (flush-output))
        (let ([bye (lambda ()
                     (when banner? (printf "\nWeb Server stopped.\n"))
                     (shutdown-server))])
          (with-handlers ([exn:break? (lambda (exn) (bye))])
            (semaphore-wait/enable-break sema)
            ; Give the final response time to get there
            (sleep 2)
            ;; We can get here if a /quit url is visited
            (bye))))))
