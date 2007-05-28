(module web mzscheme
  (require (lib "serialize.ss")
           (lib "plt-match.ss")
           (lib "url.ss" "net")           
           (lib "request-structs.ss" "web-server")
           (rename "abort-resume.ss" send/suspend0 send/suspend)
           (all-except "abort-resume.ss" send/suspend)
           "session.ss"
           "stuff-url.ss")
  
  (provide send/suspend/hidden
           send/suspend/url
           extract-proc/url embed-proc/url
           start-servlet)
  
  ;; start-servlet: -> request
  ;; set the initial interaction point for the servlet
  (define (start-servlet)
    #;(printf "start-session~n")
    (start-session dispatch)
    #;(printf "start-interaction~n")
    (start-interaction
     (lambda (req)
       (or (request->continuation req)
           (lambda (req) (dispatch-start req))))))
  
  ;; send/suspend/hidden: (url input-field -> response) -> request
  ;; like send/suspend except the continuation is encoded in a hidden field
  (define (send/suspend/hidden page-maker)
    (send/suspend0
     (lambda (k)
       (let ([p-cont (serialize k)])
         (page-maker
          (session-url (current-session))
          `(input ([type "hidden"] [name "kont"] [value ,(format "~s" p-cont)])))))))
  
  ;; send/suspend/url: (url -> response) -> request
  ;; like send/suspend except the continuation is encoded in the url
  (define (send/suspend/url page-maker)
    (let ([ses (current-session)])
      (send/suspend0
       (lambda (k)
         (page-maker
          (stuff-url (serialize k)
                     (session-url ses)
                     (session-mod-path ses)))))))
  
  (define embed-label 'superkont)  
  (define (embed-proc/url k-url proc)
    (define ses (current-session))
    (define superkont-url
      (stuff-url (serialize proc)                                              
                 (session-url ses)
                 (session-mod-path ses)))
    (define result-uri
      (extend-url-query k-url embed-label 
                        (url->string superkont-url)))
    (begin0 result-uri
            (when (> (string-length (url->string result-uri))
                     1024)
              (error "the url is too big: " (url->string result-uri)))))
  (define (extract-proc/url request)
    (define req-url (request-uri request))
    (define binds (url-query req-url))
    (define maybe-embedding (assq embed-label binds))
    (if maybe-embedding
        (let* ([ses (current-session)]
               [superkont-url (string->url (cdr maybe-embedding))]
               [proc (deserialize 
                      (unstuff-url
                       superkont-url (session-url ses)
                       (session-mod-path ses)))])
          (proc request))
        (error 'send/suspend/dispatch "No ~a: ~S!" embed-label binds)))
  
  ;; request->continuation: req -> continuation
  ;; decode the continuation from the hidden field of a request
  (define (request->continuation req)
    (or
     ; Look in url for c=<k>
     (let* ([ses (current-session)]
            [req-url (request-uri req)]
            [qry (url-query req-url)]
            [l-code (find-binding 'c qry)])
       (and l-code
            (deserialize
             (unstuff-url
              req-url (session-url ses)
              (session-mod-path ses)))))
     ; Look in query for kont=<k>
     (match (bindings-assq #"kont" (request-bindings/raw req))
       [(struct binding:form (id kont))
        (deserialize (read (open-input-bytes kont)))]
       [_ #f]))))