(module web-server-unit mzscheme
  (require "sig.ss"
           "web-server-structs.ss"
           "connection-manager.ss"
           "configuration-structures.ss"
           "servlet.ss"
           "cache-table.ss"
           (rename "request-parsing.ss" 
                   the-read-request read-request))
  (require (prefix sequencer: "dispatch-sequencer.ss")
           (prefix passwords: "dispatch-passwords.ss")
           (prefix files: "dispatch-files.ss")
           (prefix servlets: "dispatch-servlets.ss")
           (prefix path-procedure: "dispatch-pathprocedure.ss"))
  (require (lib "tcp-sig.ss" "net")
           (lib "unitsig.ss")
           (lib "string.ss")
           (lib "url.ss" "net"))
  (provide web-server@) 
  
  ;; ****************************************
  ;; stick this auxilliary outside the unit so
  ;; I can get at it with require/expose
  
  ;; get-host : Url (listof (cons Symbol String)) -> Symbol
  ;; host names are case insesitive---Internet RFC 1034
  (define DEFAULT-HOST-NAME '<none>)
  (define (get-host uri headers)
    (cond
      [(url-host uri) => string->symbol]
      [(assq 'host headers)
       =>
       (lambda (h) (string->symbol (bytes->string/utf-8 (cdr h))))]
      [else DEFAULT-HOST-NAME]))
  
  ;; ****************************************
  
  (define dispatch-server@
    (unit/sig dispatch-server^
      (import net:tcp^ (config : dispatch-server-config^))
      
      ;; serve: -> -> void
      ;; start the server and return a thunk to shut it down
      (define (serve)
        (let ([the-server-custodian (make-custodian)])
          (start-connection-manager the-server-custodian)
          (parameterize ([current-custodian the-server-custodian]
                         [current-server-custodian the-server-custodian])
            (thread
             (lambda ()
               (listener-loop))))
          (lambda ()
            (custodian-shutdown-all the-server-custodian))))
      
      ;; listener-loop : -> void
      ;; loops around starting a listener if the current listener dies
      (define (listener-loop)
        (let ([sema (make-semaphore 0)])
          (let loop ()
            (let ([listener (tcp-listen config:port config:max-waiting
                                        #t config:listen-ip)])
              (let ([get-ports
                     (lambda () 
                       (let-values ([(ip op) (tcp-accept listener)])
                         ;; Try to set buffer mode, and if it can't be set,
                         ;;  assume that it doesn't matter. (Only happens
                         ;;  when tcp-accept is not MzScheme's version.)
                         (with-handlers ([exn:fail? void])
                           (file-stream-buffer-mode op 'none))
                         (values ip op)))])
                (thread
                 (lambda ()
                   (with-handlers ([void (lambda (e)
                                           ; If the exception did not kill the listener
                                           (with-handlers ([void void])
                                             (tcp-close listener))
                                           (semaphore-post sema)
                                           ; Rethrow the error to this thread's error printer
                                           (raise e))])
                     (server-loop get-ports))))))
            (semaphore-wait sema)
            (loop))))
      
      ;; server-loop: (-> i-port o-port) -> void
      ;; start a thread to handle each incoming connection
      (define (server-loop get-ports)
        (let loop ()
          (let ([connection-cust (make-custodian)])
            (parameterize ([current-custodian connection-cust])
              (let-values ([(ip op) (get-ports)])
                (serve-ports/inner ip op))))
          (loop)))
      
      ;; serve-ports : input-port output-port -> void
      ;; returns immediately, spawning a thread to handle
      ;; the connection
      ;; NOTE: (GregP) should allow the user to pass in a connection-custodian
      (define (serve-ports ip op)
        (let ([server-cust (make-custodian)])
          (start-connection-manager server-cust)
          (parameterize ([current-custodian server-cust]
                         [current-server-custodian server-cust])
            (let ([connection-cust (make-custodian)])
              (parameterize ([current-custodian connection-cust])
                (serve-ports/inner ip op))))))
      
      ;; serve-ports/inner : input-port output-port -> void
      ;; returns immediately, spawning a thread to handle
      (define (serve-ports/inner ip op)
        (thread
         (lambda ()
           (let ([conn (new-connection config:initial-connection-timeout
                                       ip op (current-custodian) #f)])
             (with-handlers ([exn:fail:network?
                              (lambda (e)
                                (set-connection-close?! conn #t)
                                (kill-connection! conn)
                                (raise e))])
               (serve-connection conn))))))
      
      ;; serve-connection: connection -> void
      ;; respond to all requests on this connection
      (define (serve-connection conn)
        (let connection-loop ()
          (let-values ([(req close?) (config:read-request conn)])
            (set-connection-close?! conn close?)
            (adjust-connection-timeout! conn config:initial-connection-timeout)
            (config:dispatch conn req)
            (cond
              [(connection-close? conn) (kill-connection! conn)]
              [else (connection-loop)]))))))
  
  (define web-config@->dispatch-server-config@
    (unit/sig dispatch-server-config^
      (import (config : web-config^))
      (define read-request the-read-request)
      
      (define port config:port)
      (define listen-ip config:listen-ip)
      (define max-waiting config:max-waiting)
      (define initial-connection-timeout config:initial-connection-timeout)
      
      ;; dispatch: connection request host -> void
      ;; NOTE: (Jay) First step towards a different way of doing dispatching. Initially,
      ;;       the dispatchers will be hard-coded based on the configuration file.
      ;;       Eventually, they will be more configurable and extensible.
      ;; NOTE: (GregP) I'm going to use the dispatch logic out of v208 for now.
      ;;       I will move the other  dispatch logic out of the prototype
      ;;       at a later time.
      (define dispatch 
        (let* ([cache (make-cache-table)]
               [lookup-dispatcher
                (lambda (host host-info)
                  (cache-table-lookup!
                   cache host
                   (lambda ()
                     (host-info->dispatcher host-info))))])
          (lambda (conn req)
            (let* ([host (get-host (request-uri req) (request-headers req))]
                   [host-info (config:virtual-hosts host)])
              ((host-log-message host-info) (request-host-ip req)
                                            (request-client-ip req) (request-method req) (request-uri req) host)
              ((lookup-dispatcher host host-info)
               conn req)))))
      
      (define (host-info->dispatcher host-info)
        (sequencer:gen-dispatcher
         (passwords:gen-dispatcher host-info config:access)
         (path-procedure:gen-dispatcher "/conf/collect-garbage"
                                        (lambda ()
                                          (collect-garbage)
                                          ((responders-collect-garbage (host-responders host-info)))))
         (servlets:gen-dispatcher host-info
                                  config:instances config:scripts config:make-servlet-namespace)
         (files:gen-dispatcher host-info)))))
  
  (define web-server@
    (compound-unit/sig 
      (import (TCP : net:tcp^)
              (CONFIG : web-config^))
      (link (DISPATCH-CONFIG : dispatch-server-config^
                             (web-config@->dispatch-server-config@ CONFIG))
            (DISPATCH : dispatch-server^
                      (dispatch-server@ TCP DISPATCH-CONFIG)))
      (export (open (DISPATCH : web-server^))))))
