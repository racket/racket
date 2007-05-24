(module persistent-web-interaction mzscheme
  (require (rename "persistent-expander.ss" send/suspend0 send/suspend)
           (all-except "persistent-expander.ss" send/suspend)
           "session.ss"
           "stuff-url.ss"
           (lib "servlet-helpers.ss" "web-server" "private")
           (lib "serialize.ss")
           (lib "url.ss" "net"))
  
  (provide (all-from-except mzscheme #%module-begin)
           (rename lang-module-begin #%module-begin)
           send/suspend/hidden
           send/suspend/url
           send/suspend/dispatch
           extract-proc/url embed-proc/url
           redirect/get
           start-servlet)
  
  ;; start-servlet: -> request
  ;; set the initial interaction point for the servlet
  (define (start-servlet)
    (printf "start-session~n")
    (start-session dispatch)
    (printf "start-interaction~n")
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
    (if (exists-binding? embed-label binds)
        (let* ([ses (current-session)]
               [superkont-url (string->url (extract-binding/single embed-label binds))]
               [proc (deserialize 
                      (unstuff-url
                       superkont-url (session-url ses)
                       (session-mod-path ses)))])
          (proc request))
        (error 'send/suspend/dispatch "No ~a: ~S!" embed-label binds)))
  
  (define-syntax send/suspend/dispatch
    (syntax-rules ()
      [(_ response-generator)
       (extract-proc/url
        (send/suspend/url
         (lambda (k-url)
           (response-generator
            (lambda (proc)
              (embed-proc/url k-url proc))))))]))  
  
  (define (redirect/get)
    (send/suspend/url (lambda (k-url) (redirect-to (url->string k-url) temporarily))))
  
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
     (let ([bdgs (request-bindings req)])
       (and (exists-binding? 'kont bdgs)
            (deserialize
             (read
              (open-input-string
               (extract-binding/single 'kont bdgs)))))))))