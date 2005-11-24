;; Default choice for writing module servlets
(module servlet mzscheme
  (require (lib "contract.ss")
           (lib "etc.ss")
           (lib "xml.ss" "xml"))
  (require "servlet-tables.ss"
           "response.ss"
           "servlet-helpers.ss"
           "timer.ss"
           "web-cells.ss")
  
  ;; ************************************************************
  ;; HELPERS
  
  ;; Is it a Xexpr, or an Xexpr with procedures?
  (define (xexpr/callback? x)
    (correct-xexpr? x 
                    (lambda () #t)
                    (lambda (exn)
                      (if (procedure? (exn:invalid-xexpr-code exn))
                          #t
                          (begin ((error-display-handler) (exn-message exn) exn)
                                 #f)))))
  
  ;; replace-procedures : (proc -> url) xexpr/callbacks? -> xexpr?
  ;; Change procedures to the send/suspend of a k-url
  (define (xexpr/callback->xexpr p->a p-exp)
    (cond
      [(list? p-exp) (map (lambda (p-e) (xexpr/callback->xexpr p->a p-e))
                          p-exp)]
      [(procedure? p-exp) (p->a p-exp)]
      [else p-exp]))
  
  ;; get-current-servlet-instance : -> servlet
  (define (get-current-servlet-instance)
    (let ([inst (thread-cell-ref current-servlet-instance)])
      (unless inst
        (raise (make-exn:servlet:no-current-instance "" (current-continuation-marks))))
      inst))
  
  ;; Weak contracts: the input is checked in output-response, and a message is
  ;; sent directly to the client (Web browser) instead of the terminal/log.
  (provide/contract
   [redirect/get (-> request?)]
   [redirect/get/forget (-> request?)]
   [adjust-timeout! (number? . -> . any)]
   [send/back (any/c . -> . any)]
   [send/finish (any/c . -> . any)]
   [send/suspend (((string? . -> . any/c)) ((request? . -> . any/c)) . opt-> . request?)]
   [send/forward (((string? . -> . any/c)) ((request? . -> . any/c)) . opt-> . request?)]
   ;;; validate-xexpr/callback is not checked anywhere:
   [send/suspend/callback (xexpr/callback? . -> . any)])
  
  (provide
   clear-continuation-table!
   send/suspend/dispatch
   current-servlet-continuation-expiration-handler
   xexpr/callback?
   xexpr/callback->xexpr
   (all-from "web-cells.ss")
   (all-from "servlet-helpers.ss"))    
  
  ;; ************************************************************
  ;; EXPORTS
  
  ;; current-servlet-continuation-expiration-handler : request -> response
  (define current-servlet-continuation-expiration-handler
    (make-parameter #f))
  
  ;; adjust-timeout! : sec -> void
  ;; adjust the timeout on the servlet
  (define (adjust-timeout! secs)
    (reset-timer (servlet-instance-timer (get-current-servlet-instance))
                 secs))
  
  ;; ext:clear-continuations! -> void
  (define (clear-continuation-table!)
    (clear-continuations! (get-current-servlet-instance)))
  
  ;; send/back: response -> void
  ;; send a response and don't clear the continuation table
  (define (send/back resp)
    (let ([ctxt (servlet-instance-context (get-current-servlet-instance))])
      (output-response (execution-context-connection ctxt) resp)
      ((execution-context-suspend ctxt))))
  
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
         (let* ([inst (get-current-servlet-instance)]
                [ctxt (servlet-instance-context inst)]
                [k-url (store-continuation!
                        k expiration-handler
                        (request-uri (execution-context-request ctxt))
                        inst)]
                [response (response-generator k-url)])
           (output-response (execution-context-connection ctxt) response)
           ((execution-context-suspend ctxt)))))))
  
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
           (let/ec k0
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
