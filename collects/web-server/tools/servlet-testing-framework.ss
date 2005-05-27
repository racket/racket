;; The idea is to mimick the entire Web server as part of the framework for
;; this testing infrastructure. Copy most of this stuff from v299. The v299 Web
;; server was written with the assumption that continuations exist across
;; threads; this is not the case in the exp Web server. As a result, only one
;; thread should be used at a time.

;; Since the real send/* are used, with their full continuation table, one can
;; use this to fully pretend to be a Web browser, including back buttons and
;; cloning Web pages.
(module servlet-testing-framework mzscheme
  (require (lib "match.ss")
           (lib "list.ss")
           (lib "url.ss" "net")
           (lib "uri-codec.ss" "net")
           (lib "xml.ss" "xml")

           (lib "servlet.ss" "web-server")
           (lib "servlet-tables.ss" "web-server")
           (lib "connection-manager.ss" "web-server")
           (lib "timer.ss" "web-server")
           (all-except (lib "request-parsing.ss" "web-server")
                       request-bindings)
           )

  (provide start-servlet resume-servlet resume-servlet/headers)

  ;; Start the servlet
  (define (start-servlet svt)
    (run-servlet (new-request) svt))

  (define the-instance
    (make-servlet-instance 'id0 (make-hash-table) 0 (make-semaphore 0)))

  ;; new-servlet-context: request o-port (-> void) -> servlet-context
  (define (new-servlet-context req op suspend )
    (make-servlet-context
      the-instance
      (let ((cust (make-custodian)))
        (make-connection
          (start-timer 15 (lambda () (custodian-shutdown-all cust)))
          (open-input-string "foo") op cust #t))
      req
      suspend))

   ;; run-servlet: request string -> s-expression
   ;; Run a servlet and return its next response. Note that the servlet may be a
   ;; continuation.
   (define (run-servlet req svt)
     (let* ((cust (make-custodian))
            (result-channel (make-channel))
            (op (open-output-string))
            (sc (new-servlet-context
                  req op
                  (make-suspender result-channel op cust))))
       (parameterize ((current-custodian cust))
         (thread
           (lambda ()
             (thread-cell-set! current-servlet-context sc)
             (svt))))
       (channel-get result-channel)))

   ;; make-suspender: channel o-port custodian -> (-> void)
   (define (make-suspender result-channel op cust)
     (lambda ()
       (channel-put
         result-channel
         (let ((ip (open-input-string (get-output-string op))))
           (purify-port ip)
           (xml->xexpr (read-xml/element ip))))))

  (define (resume-servlet/headers prev-url input headers)
    (with-handlers
      ((exn:fail:contract?
         (lambda (e)
           `(html (head (title "Timeout"))
                  (body
                    (p "The transaction referred to by this url is no longer "
                       "active.  Please "
                       (a ((href ,(servlet-instance-k-table the-instance)))
                          "restart")
                       " the transaction."))))))
    (let ((u (string->url prev-url)))
      (cond
        ((continuation-url? u)
         => (lambda (res)
              (let ((k (hash-table-get (servlet-instance-k-table the-instance)
                                       (cadr res)))
                    (new-req (new-request/url+headers
                               (embed-url-bindings input u) headers)))
                (run-servlet new-req (lambda () (k new-req))))))
        (else (error "url doesn't encode a servlet continuation"))))))

  ;; Resume the servlet
  (define (resume-servlet prev-url input)
    (resume-servlet/headers prev-url input '()))

  ;; embed-url-bindings: (listof (cons string string)) url -> url
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

  (define (remove-query an-url)
    (make-url
      (url-scheme an-url)
      (url-user an-url)
      (url-host an-url)
      (url-port an-url)
      (url-path an-url)
      '()
      (url-fragment an-url)))

  ;; Produce a new request
  (define (new-request)
    (new-request/bindings '()))

  ;; Produce a new request, with an url
  (define (new-request/url new-url)
    (new-request/url+headers new-url '()))

  ;; Produce a new request, with an url and headers
  (define (new-request/url+headers new-url headers)
    (make-request 'get (remove-query new-url) headers (url-query new-url)
                  "a-host-ip" "a-client-ip"))

  ;; Produce a new request, with bindings
  (define (new-request/bindings bs)
    (make-request 'get (string->url "http://www.example.com/") '() bs
                  "a-host-ip" "a-client-ip"))

  )
