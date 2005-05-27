(module backend-servlet-testing mzscheme
  (require (lib "connection-manager.ss" "web-server")
           (lib "servlet-tables.ss" "web-server")
           (lib "request-parsing.ss" "web-server")
           "backend.ss"
           (lib "url.ss" "net")
           (lib "xml.ss" "xml")
           (lib "match.ss")
           )

  (provide run-servlet simple-start-servlet simple-resume-servlet)

  ;; run-servlet: bindings (listof bindings) (-> response) -> boolean
  ;; feed a bunch of requests to a servlet
  (define (run-servlet bdg0 bdgs svt)
    (let ((resp (simple-start-servlet
                  (new-request/url
                    (embed-url-bindings bdg0 (string->url "")))
                  svt)))
      (or (null? bdgs)
          (resume-test bdgs (form->k-url resp)))))

  ;; resume-test: (listof (listof (cons symbol string))) url -> boolean
  ;; having a gotten a response, feed a bunch of requests to a servlet
  (define (resume-test bdgs a-url)
    (or (null? bdgs)
        (let* ([resp (simple-resume-servlet
                       (new-request/url (embed-url-bindings (car bdgs) a-url))
                       a-url)]
               [next-url (form->k-url resp)])
          (resume-test (cdr bdgs) next-url))))


  ;; Produce the k-url for the form on this page
  ;; TODO: bad cut and paste
  (define (form->k-url an-xexpr)
    (string->url
     (string-append
      "http://nowhere.com"
      (let recur ([sub-xexpr an-xexpr])
        (match sub-xexpr
          ((`form ((attrs rhss) ...) . rest)
           (ormap (lambda (attr rhs) (and (eqv? attr 'action) rhs))
                  attrs rhss))
          ((tag (attrs ...) body ...) (ormap recur body))
          (else #f))))))

  ;; ****************************************

  (define the-instance-table (make-hash-table))
  (define the-instance-timeout 10)

  ;; simple-start-servlet: request (-> response) -> response
  ;; run the servlet until it produces a response
  (define (simple-start-servlet req svt)
    (let* ([i-port (open-input-string "")]
           [o-port (open-output-string)]
           [conn (new-connection 86400 i-port o-port (make-custodian) #t)])
      (start-servlet conn req the-instance-table the-instance-timeout
                     (lambda (adjust-timeout! initial-request)
                       (svt)))
      (let ([i-p (open-input-string (get-output-string o-port))])
        (purify-port i-p)
        (xml->xexpr (read-xml/element i-p)))))

  ;; simple-resume-servlet: request url -> response
  ;; resume the servlet continuation until it produces a response
  (define (simple-resume-servlet req url)
    (let* ([k-ref (continuation-url? url)]
           [i-port (open-input-string "")]
           [o-port (open-output-string)]
           [conn (new-connection 86400 i-port o-port (make-custodian) #t)])
      (resume-servlet conn req k-ref the-instance-table)
      (let ([i-p (open-input-string (get-output-string o-port))])
        (purify-port i-p)
        (xml->xexpr (read-xml/element i-p)))))

  ;; embed-url-bindings: (listof (cons symbol string)) url -> url
  ;; encode bindings in a url
  (define (embed-url-bindings env in-url)
    (let* ((query (url-query in-url))
           (old-env (or query '())))
      (make-url
       (url-scheme in-url)
       (url-user in-url)
       (url-host in-url)
       (url-port in-url)
       (url-path in-url)
       (append env old-env)
       (url-fragment in-url))))

  ;; Produce a new request, with an url
  (define (new-request/url new-url)
    (make-request
      'get new-url '() (url-query new-url) "a-host-ip" "a-client-ip"))

  )
