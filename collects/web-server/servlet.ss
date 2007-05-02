(module servlet mzscheme
  (require (lib "contract.ss")
           (lib "etc.ss")
           (lib "xml.ss" "xml"))
  (require "managers/manager.ss"
           "private/servlet.ss"
           "private/url.ss"
           "private/servlet-helpers.ss"
           "private/web-cells.ss"
           "servlet-structs.ss")  
  
  ;; ************************************************************
  ;; HELPERS
  
  ;; replace-procedures : (proc -> url) xexpr/callbacks? -> xexpr?
  ;; Change procedures to the send/suspend of a k-url
  (define (xexpr/callback->xexpr p->a p-exp)
    (cond
      [(list? p-exp) (map (lambda (p-e) (xexpr/callback->xexpr p->a p-e))
                          p-exp)]
      [(procedure? p-exp) (p->a p-exp)]
      [else p-exp])) 
  
  ;; Weak contracts: the input is checked in output-response, and a message is
  ;; sent directly to the client (Web browser) instead of the terminal/log.
  (provide/contract
   [xexpr/callback->xexpr (embed/url? xexpr/callback? . -> . xexpr?)]
   ; XXX contract
   [current-url-transform parameter?]
   ; XXX contract
   [current-servlet-continuation-expiration-handler parameter?]
   [redirect/get (-> request?)]
   [redirect/get/forget (-> request?)]
   [adjust-timeout! (number? . -> . void?)]
   [clear-continuation-table! (-> void?)]
   [send/back (any/c . -> . void?)]
   [send/finish (any/c . -> . void?)]
   [send/suspend ((response-generator?) (expiration-handler?) . opt-> . request?)]
   [send/forward ((response-generator?) (expiration-handler?) . opt-> . request?)]
   [send/suspend/dispatch ((embed/url? . -> . servlet-response?) . -> . any/c)]
   [send/suspend/callback (xexpr/callback? . -> . any/c)])
  
  (require "private/servlet-url.ss")
  (provide (all-from "private/web-cells.ss")
           (all-from "private/servlet-helpers.ss")
           (all-from "private/servlet-url.ss")
           (all-from "servlet-structs.ss"))
  
  ;; ************************************************************
  ;; EXPORTS
  
  ;; current-url-transform : string? -> string?
  (define (default-url-transformer x) x)
  (define current-url-transform
    (make-parameter default-url-transformer))
  
  ;; current-servlet-continuation-expiration-handler : request -> response
  (define current-servlet-continuation-expiration-handler
    (make-parameter #f))
  
  ;; adjust-timeout! : sec -> void
  ;; adjust the timeout on the servlet
  (define (adjust-timeout! secs)
    ((manager-adjust-timeout! (current-servlet-manager)) (get-current-servlet-instance-id) secs))
  
  ;; ext:clear-continuations! -> void
  (define (clear-continuation-table!)
    ((manager-clear-continuations! (current-servlet-manager)) (get-current-servlet-instance-id)))
  
  ;; send/back: response -> void
  ;; send a response and don't clear the continuation table
  (define (send/back resp)
    (define ctxt (thread-cell-ref current-execution-context))
    ((execution-context-suspend ctxt) resp))
  
  ;; send/finish: response -> void
  ;; send a response and clear the continuation table
  (define (send/finish resp)
    (clear-continuation-table!)
    ; If we readjust the timeout to something small, the session will expire shortly
    ; we cannot wait for send/back to return, because it doesn't
    ; Also, we cannot get the initial-connection-timeout variable from here
    ; In the future, we should use the servlet's specific default-timeout
    (adjust-timeout! 10)
    (send/back resp))
  
  ;; send/suspend: (url -> response) [(request -> response)] -> request
  ;; send a response and apply the continuation to the next request
  (define send/suspend
    (opt-lambda (response-generator [expiration-handler (current-servlet-continuation-expiration-handler)])
      (with-frame-after
       (let/cc k
         (define instance-id (get-current-servlet-instance-id))
         (define ctxt (thread-cell-ref current-execution-context))
         (define k-embedding ((manager-continuation-store! (current-servlet-manager))
                              instance-id
                              (make-custodian-box (current-custodian) k)
                              expiration-handler))
         (define k-url ((current-url-transform)
                        (embed-ids 
                         (list* instance-id k-embedding)
                         (request-uri (execution-context-request ctxt)))))
         (send/back (response-generator k-url))))))
  
  ;; send/forward: (url -> response) [(request -> response)] -> request
  ;; clear the continuation table, then behave like send/suspend
  (define send/forward
    (opt-lambda (response-generator [expiration-handler (current-servlet-continuation-expiration-handler)])
      (clear-continuation-table!)
      (send/suspend response-generator expiration-handler)))
  
  ;; send/suspend/dispatch : ((proc -> url) -> response) [(request -> response)] -> request
  ;; send/back a response generated from a procedure that may convert
  ;; procedures to continuation urls
  (define (send/suspend/dispatch response-generator)
    ; This restores the tail position.
    ; Note: Herman's syntactic strategy would fail without the new-request capture.
    ;       (Moving this to the tail-position is not possible anyway, by the way.)
    (let ([thunk 
           (let/cc k0
             (send/back
              (response-generator
               (opt-lambda (proc [expiration-handler (current-servlet-continuation-expiration-handler)])
                 (let/ec k1 
                   (let ([new-request (send/suspend k1 expiration-handler)])
                     (k0 (lambda () (proc new-request)))))))))])
      (thunk)))
  
  ;; send/suspend/callback : xexpr/callback? -> void
  ;; send/back a response with callbacks in it; send/suspend those callbacks.
  (define (send/suspend/callback p-exp)
    (send/suspend/dispatch
     (lambda (embed/url)
       (xexpr/callback->xexpr embed/url p-exp))))
  
  ;; ************************************************************
  ;; HIGHER-LEVEL EXPORTS
  
  (define ((make-redirect/get send/suspend))
    (send/suspend (lambda (k-url) (redirect-to k-url temporarily))))
  
  ; redirect/get : -> request
  (define redirect/get (make-redirect/get send/suspend))
  (define redirect/get/forget (make-redirect/get send/forward)))
