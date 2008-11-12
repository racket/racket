#lang scheme
(require net/url
         scheme/contract
         scheme/serialize
         web-server/http
         web-server/private/define-closure
         web-server/private/servlet         
         "abort-resume.ss"
         "stuff-url.ss"
         "../private/url-param.ss")

(provide
 ;; Server Interface
 initialize-servlet
 
 ;; Servlet Interface
 send/suspend/hidden
 send/suspend/url
 send/suspend/dispatch)

; These contracts interfere with the continuation safety marks
#;(provide/contract
 ;; Server Interface
 [initialize-servlet ((request? . -> . response?) . -> . (request? . -> . response?))]
 
 ;; Servlet Interface
 [send/suspend/hidden ((url? list? . -> . response?) . -> . request?)]
 [send/suspend/url ((url? . -> . response?) . -> . request?)]
 [send/suspend/dispatch ((((request? . -> . any/c) . -> . url?) . -> . response?)
                         . -> . any/c)])

;; initial-servlet : (request -> response) -> (request -> response?)
(define (initialize-servlet start)
  (let ([params (current-parameterization)])
    (lambda (req0)
      (call-with-parameterization 
       params
       (lambda ()
         (dispatch
          (lambda (req1)
            (or (request->continuation req1)
                ; Try to decode a continuation from the request,
                ; or, use the start procedure to initialize a session
                (lambda (req2) (dispatch-start start req2))))
          req0))))))

;; send/suspend/hidden: (url input-field -> response) -> request
;; like send/suspend except the continuation is encoded in a hidden field
(define (send/suspend/hidden page-maker)
  (send/suspend
   (lambda (k)
     (let ([p-cont (serialize k)])
       (page-maker
        (request-uri (execution-context-request (current-execution-context)))
        `(input ([type "hidden"] [name "kont"] [value ,(format "~s" p-cont)])))))))

;; send/suspend/url: (url -> response) -> request
;; like send/suspend except the continuation is encoded in the url
(define (send/suspend/url page-maker)
  (send/suspend
   (lambda (k)
     (page-maker
      (stuff-url k
                 (request-uri (execution-context-request (current-execution-context))))))))

(define-closure embed/url (proc) (k)
  (stuff-url (kont-append-fun k proc)
             (request-uri (execution-context-request (current-execution-context)))))
(define (send/suspend/dispatch response-generator)
  (send/suspend
   (lambda (k)
     (response-generator (make-embed/url (lambda () k))))))

;; request->continuation: req -> continuation
;; decode the continuation from the hidden field of a request
(define (request->continuation req)
  (or
   ; Look in url for c=<k>
   (let ([req-url (request-uri req)])
     (and (stuffed-url? req-url)
          (unstuff-url
           req-url)))
   ; Look in query for kont=<k>
   (match (bindings-assq #"kont" (request-bindings/raw req))
     [(struct binding:form (id kont))
      (deserialize (read (open-input-bytes kont)))]
     [_ #f])))

(provide/contract
 [redirect/get (-> request?)])  

(define (redirect/get)
  (send/suspend/url (lambda (k-url) (redirect-to (url->string k-url) temporarily))))
