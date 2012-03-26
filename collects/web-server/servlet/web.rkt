#lang racket/base
(require racket/contract
         racket/list
         net/url)
(require web-server/managers/manager
         web-server/private/util
         web-server/private/servlet
         web-server/servlet/web-cells
         web-server/servlet/servlet-structs
         web-server/http)

(provide servlet-prompt)

;; ********************************************************************************
;; Parameter Embedding
(require web-server/private/url-param)

;; embed-ids: (list number number number) url -> string
(define (embed-ids v u)
  (url->string
   (insert-param 
    u "k" (write/string v))))

;; continuation-url?: url -> (or/c (list number number number) #f)
;; determine if this url encodes a continuation and extract the instance id and
;; continuation id.
(define (continuation-url? a-url)
  (parameterize ([read-accept-dot #t])
    (cond 
      [(extract-param a-url "k")
       => read/string]
      [else
       #f])))

(provide/contract
 [continuation-url? (url? . -> . (or/c false/c (list/c number? number? number?)))])

;; ********************************************************************************

(provide/contract
 [current-servlet-continuation-expiration-handler 
  (parameter/c 
   (or/c false/c
         (request? . -> . can-be-response?)))]
 [redirect/get (() (#:headers (listof header?)) . ->* . request?)]
 [redirect/get/forget (() (#:headers (listof header?)) . ->* . request?)]
 [adjust-timeout! (number? . -> . void?)]
 [clear-continuation-table! (-> void?)]
 [send/back (can-be-response? . -> . void?)]
 [send/finish (can-be-response? . -> . void?)]
 [send/forward ((string? . -> . can-be-response?) . -> . request?)]
 [send/suspend ((string? . -> . can-be-response?) . -> . request?)]
 [send/suspend/dispatch ((((request? . -> . any) . -> . string?) . -> . can-be-response?) . -> . any)]
 [send/suspend/url ((url? . -> . can-be-response?) . -> . request?)]
 [send/suspend/url/dispatch ((((request? . -> . any) . -> . url?) . -> . can-be-response?) . -> . any)])

;; ************************************************************
;; EXPORTS

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

;; send/suspend: (url -> response) -> request
;; send a response and apply the continuation to the next request
(define (send/suspend response-generator)
  (define wcs (capture-web-cell-set))
  (begin0
    (call-with-composable-continuation
     (lambda (k)
       (define instance-id (current-servlet-instance-id))
       (define ctxt (current-execution-context))
       (define k-embedding ((manager-continuation-store! (current-servlet-manager))
                            instance-id
                            (make-custodian-box (current-custodian) k)
                            (current-servlet-continuation-expiration-handler)))
       (define k-url (embed-ids 
                      (list* instance-id k-embedding)
                      (request-uri (execution-context-request ctxt))))
       (send/back (response-generator k-url)))
     servlet-prompt)
    (restore-web-cell-set! wcs)))

(define (send/suspend/url response-generator)
  (send/suspend
   (lambda (k-url)
     (response-generator (string->url k-url)))))

;; send/forward: (url -> response) [(request -> response)] -> request
;; clear the continuation table, then behave like send/suspend
(define (send/forward response-generator)
  (clear-continuation-table!)
  (send/suspend response-generator))

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
              (lambda (proc)
                (let/ec k1 
                  ; This makes the second continuation captured by send/suspend smaller
                  (call-with-continuation-prompt
                   (lambda ()
                     (let ([new-request (send/suspend k1)])
                       (k0 (lambda () (proc new-request)))))
                   servlet-prompt))))))
          servlet-prompt)])
    (thunk)))

(define (send/suspend/url/dispatch response-generator)
  (send/suspend/dispatch
   (lambda (embed/url)
     (response-generator
      (lambda (proc)
        (string->url (embed/url proc)))))))

;; ************************************************************
;; HIGHER-LEVEL EXPORTS

(define ((make-redirect/get send/suspend) #:headers [hs empty])
  (send/suspend (lambda (k-url) (redirect-to k-url temporarily #:headers hs))))

; redirect/get : -> request
(define redirect/get (make-redirect/get send/suspend))
(define redirect/get/forget (make-redirect/get send/forward))

(define (with-errors-to-browser send/finish-or-back thunk)
  (with-handlers 
      ([exn:fail?
        (lambda (exn)
          (send/finish-or-back
           (response/xexpr
            #:code 500
            `(html (head (title "Servlet Error"))
                   (body ([bgcolor "white"])
                         (p "The following error occured: "
                            (pre ,(exn->string exn))))))))])
    (thunk)))

(provide/contract
 [with-errors-to-browser
  ((can-be-response? . -> . request?)
   (-> any)
   . -> .
   any)])
