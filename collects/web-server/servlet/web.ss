(module web mzscheme
  (require (lib "url.ss" "net")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "contract.ss")
           (lib "etc.ss"))
  (require "../managers/manager.ss"
           "../private/util.ss"
           "../private/servlet.ss"
           "../servlet/helpers.ss"
           "../servlet/web-cells.ss"
           "../servlet/servlet-structs.ss"
           "../private/response-structs.ss"
           "../private/request-structs.ss")
  
  ;; ************************************************************
  ;; HELPERS
  (provide/contract
   [continuation-url? (url? . -> . (or/c false/c (list/c number? number? number?)))]
   [embed-ids ((list/c number? number? number?) url? . -> . string?)])
  
  ;; ********************************************************************************
  ;; Parameter Embedding
  
  ;; embed-ids: (list number number number) url -> string
  (define embed-ids
    (match-lambda*
      [(list (list inst-id k-id salt) in-url)
       (insert-param
        in-url
        (format "~a*~a*~a" inst-id k-id salt))]))
  
  ;; continuation-url?: url -> (or/c (list number number number) #f)
  ;; determine if this url encodes a continuation and extract the instance id and
  ;; continuation id.
  (define (continuation-url? a-url)
    (define (match-url-params x) (regexp-match #rx"([^\\*]*)\\*([^\\*]*)\\*([^\\*]*)" x))
    (let ([k-params (filter match-url-params
                            (apply append (map path/param-param (url-path a-url))))])
      (if (empty? k-params)
          #f
          (match (match-url-params (first k-params))
            [(list s instance k-id salt)
             (let ([instance/n (string->number instance)]
                   [k-id/n (string->number k-id)]
                   [salt/n (string->number salt)])
               (if (and (number? instance/n)
                        (number? k-id/n)
                        (number? salt/n))
                   (list instance/n
                         k-id/n
                         salt/n)
                   #f))]))))
  
  ;; insert-param: url string -> string
  ;; add a path/param to the path in a url
  ;; (assumes that there is only one path/param)
  (define (insert-param in-url new-param-str)
    (url->string
     (url-replace-path
      (lambda (old-path)        
        (if (empty? old-path)
            (list (make-path/param "" (list new-param-str)))
            (list* (make-path/param (path/param-path (first old-path))
                                    (list new-param-str))
                   (rest old-path))))
      in-url)))   
  
  (provide/contract
   [current-url-transform parameter?]
   [current-servlet-continuation-expiration-handler parameter?]
   [redirect/get (-> request?)]
   [redirect/get/forget (-> request?)]
   [adjust-timeout! (number? . -> . void?)]
   [clear-continuation-table! (-> void?)]
   [send/back (response? . -> . void?)]
   [send/finish (response? . -> . void?)]
   [send/suspend ((response-generator?) (expiration-handler?) . opt-> . request?)]
   [send/forward ((response-generator?) (expiration-handler?) . opt-> . request?)]
   [send/suspend/dispatch ((embed/url? . -> . response?) . -> . any/c)])
  
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
    ((manager-adjust-timeout! (current-servlet-manager)) (current-servlet-instance-id) secs))
  
  ;; ext:clear-continuations! -> void
  (define (clear-continuation-table!)
    ((manager-clear-continuations! (current-servlet-manager)) (current-servlet-instance-id)))
  
  ;; send/back: response -> void
  ;; send a response and don't clear the continuation table
  (define (send/back resp)
    (abort-current-continuation servlet-prompt (lambda () resp)))
  
  ;; send/finish: response -> void
  ;; send a response and clear the continuation table
  (define (send/finish resp)
    (clear-continuation-table!)
    (send/back resp))
  
  ;; send/suspend: (url -> response) [(request -> response)] -> request
  ;; send a response and apply the continuation to the next request
  (define send/suspend
    (opt-lambda (response-generator [expiration-handler (current-servlet-continuation-expiration-handler)])
      (with-frame-after
       (call-with-composable-continuation
        (lambda (k)
          (define instance-id (current-servlet-instance-id))
          (define ctxt (current-execution-context))
          (define k-embedding ((manager-continuation-store! (current-servlet-manager))
                               instance-id
                               (make-custodian-box (current-custodian) k)
                               expiration-handler))
          (define k-url ((current-url-transform)
                         (embed-ids 
                          (list* instance-id k-embedding)
                          (request-uri (execution-context-request ctxt)))))
          (send/back (response-generator k-url)))
        servlet-prompt))))
  
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
           (call-with-current-continuation
            (lambda (k0)
              (send/back
               (response-generator
                (opt-lambda (proc [expiration-handler (current-servlet-continuation-expiration-handler)])
                  (let/ec k1 
                    (let ([new-request (send/suspend k1 expiration-handler)])
                      (k0 (lambda () (proc new-request)))))))))
            servlet-prompt)])
      (thunk)))
    
  ;; ************************************************************
  ;; HIGHER-LEVEL EXPORTS
  
  (define ((make-redirect/get send/suspend))
    (send/suspend (lambda (k-url) (redirect-to k-url temporarily))))
  
  ; redirect/get : -> request
  (define redirect/get (make-redirect/get send/suspend))
  (define redirect/get/forget (make-redirect/get send/forward)))