(module web mzscheme
  (require (lib "serialize.ss")
           (lib "plt-match.ss")
           (lib "url.ss" "net")           
           "../private/request-structs.ss"
           "abort-resume.ss"
           "../private/session.ss"
           "stuff-url.ss"
           "../private/url-param.ss")
  
  (provide 
   ;; Server Interface
   initialize-servlet
   
   ;; Servlet Interface
   send/suspend/hidden
   send/suspend/url
   extract-proc/url
   embed-proc/url)
  
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
  ;; XXX incorporate stuffing in some way
  (define (send/suspend/hidden page-maker)
    (send/suspend
     (lambda (k)
       (let ([p-cont (serialize k)])
         (page-maker
          (session-url (current-session))
          `(input ([type "hidden"] [name "kont"] [value ,(format "~s" p-cont)])))))))
  
  ;; send/suspend/url: (url -> response) -> request
  ;; like send/suspend except the continuation is encoded in the url
  (define (send/suspend/url page-maker)
    (send/suspend
     (lambda (k)
       (page-maker
        (stuff-url k
                   (session-url (current-session)))))))
  
  ; XXX Don't use stuff-url, but use the other serialize thing
  (define embed-label "superkont")  
  (define (embed-proc/url k-url proc)
    (define superkont-url
      (stuff-url proc                                              
                 (session-url (current-session))))
    (define result-uri
      (insert-param k-url embed-label 
                    (url->string superkont-url)))
    (begin0 result-uri
            (when (> (string-length (url->string result-uri))
                     1024)
              (error "the url is too big: " (url->string result-uri)))))
  (define (extract-proc/url request)
    (define req-url (request-uri request))
    (define maybe-embedding (extract-param req-url embed-label))
    (if maybe-embedding
        (let ([proc (unstuff-url
                     (string->url maybe-embedding))])
          (proc request))
        (error 'send/suspend/dispatch "No ~a: ~S!" embed-label)))
  
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
       [_ #f]))))