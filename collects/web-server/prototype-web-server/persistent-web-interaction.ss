(module persistent-web-interaction mzscheme
  (require (rename "persistent-expander.ss" send/suspend0 send/suspend)
           (all-except "persistent-expander.ss" send/suspend)
           "session.ss"
           "stuff-url.ss"
           (lib "servlet-helpers.ss" "web-server")
           (lib "serialize.ss")
           (lib "url.ss" "net")
           )
  
  (provide (all-from-except mzscheme #%module-begin)
           (rename lang-module-begin #%module-begin)
           send/suspend/hidden
           send/suspend/url
           start-servlet)
  
  ;; start-servlet: -> request
  ;; set the initial interaction point for the servlet
  (define (start-servlet)
    (start-session dispatch)
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
          `(input ([type "hidden"][name "kont"][value ,(format "~s" p-cont)])))))))
  
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
  
  ;; request->continuation: req -> continuation
  ;; decode the continuation from the hidden field of a request
  (define (request->continuation req)
    (or
     (let* ([ses (current-session)]
            [req-url (request-uri req)]
            [qry (url-query req-url)]
            [l-code (find-binding 'c qry)])
       (and l-code
            (deserialize
             (unstuff-url
              req-url (session-url ses)
              (session-mod-path ses)))))
     (let ([bdgs (request-bindings req)])
       (and (exists-binding? 'kont bdgs)
            (deserialize
             (read
              (open-input-string
               (extract-binding/single 'kont bdgs))))))))
  )