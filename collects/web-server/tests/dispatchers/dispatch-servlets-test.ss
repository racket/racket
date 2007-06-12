(module dispatch-servlets-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (only (planet "ssax.ss" ("lizorkin" "ssax.plt" 1 3))
                 ssax:xml->sxml)
           (planet "sxml.ss" ("lizorkin" "sxml.plt" 1 4))
           (lib "etc.ss")
           (lib "url.ss" "net")
           (lib "list.ss")
           (lib "pretty.ss")
           (lib "request-structs.ss" "web-server" "private")
           (lib "cache-table.ss" "web-server" "private")
           (lib "web-server-structs.ss" "web-server" "private")
           (lib "namespace.ss" "web-server" "configuration")
           (prefix servlets: (lib "dispatch-servlets.ss" "web-server" "dispatchers"))
           "../util.ss")
  (provide dispatch-servlets-tests)
  
  (current-server-custodian (current-custodian))
  
  (define (mkd p)
    (define-values (! d)
      (servlets:make #f (box (make-cache-table)) (make-make-servlet-namespace)
                     #:url->path (lambda _ (values p url0s))
                     #:responders-servlet-loading
                     (lambda (u exn)
                       (raise exn))
                     #:responders-servlet
                     (lambda (u exn)
                       (raise exn))))
    d)
  (define url0 "http://test.com/servlets/example.ss")
  (define url0s (list (build-path "servlets") (build-path "example.ss")))
  (define (call d u bs)
    (htxml (collect d (make-request 'get (string->url u) empty bs #"" "127.0.0.1" 80 "127.0.0.1"))))
  (define (htxml bs)
    (define sx (ssax:xml->sxml (open-input-bytes (second (regexp-match #"^.+\r\n\r\n(.+)$" bs))) empty))
    (pretty-print sx)
    sx)
  
  (define test-servlets (build-path (collection-path "web-server") "tests" "servlets"))
  (define example-servlets (build-path (collection-path "web-server") "default-web-root" "servlets" "examples/"))
  
  (define dispatch-servlets-tests
    (test-suite
     "Servlets"
     
     ; XXX test update cache
     
     (test-pred "configure.ss"
                string?
                (let* ([d (mkd (build-path example-servlets 'up "configure.ss"))]
                       [k0 (first ((sxpath "//form/@action/text()") (call d url0 empty)))])
                  k0))
     
     (test-suite
      "Examples"
      (test-equal? "hello.ss"
                   (let* ([d (mkd (build-path example-servlets "hello.ss"))]
                          [t0 (first ((sxpath "//p/text()") (call d url0 empty)))])
                     t0)
                   "Hello, Web!")
      (test-equal? "add.ss"
                   (let* ([d (mkd (build-path example-servlets "add.ss"))]
                          [k0 (first ((sxpath "//form/@action/text()") (call d url0 empty)))]
                          [k1 (first ((sxpath "//form/@action/text()") (call d k0 (list (make-binding:form #"number" #"23")))))]
                          [n (first ((sxpath "//p/text()") (call d k1 (list (make-binding:form #"number" #"12")))))])
                     n)
                   "The sum is 35")
      (test-equal? "count.ss"
                   (let* ([d (mkd (build-path example-servlets "count.ss"))]
                          [ext (lambda (c)
                                 (rest (regexp-match #rx"This servlet was called (.+) times and (.+) times since loaded on" c)))]
                          [c1 (ext (first ((sxpath "//p/text()") (call d url0 empty))))]
                          [c2 (ext (first ((sxpath "//p/text()") (call d url0 empty))))])
                     (list c1 c2))
                   (list (list "1" "1")
                         (list "2" "1")))
      (test-equal? "dir.ss"
                   (let* ([d (mkd (build-path example-servlets "dir.ss"))]
                          [t0 (first ((sxpath "//p/em/text()") (call d url0 empty)))])
                     t0)
                   (path->string example-servlets))
      (test-pred "quiz.ss"
                 string?
                 (let* ([d (mkd (build-path example-servlets "quiz.ss"))])
                   (foldl (lambda (_ k)
                            (first ((sxpath "//form/@action/text()") (call d k (list (make-binding:form #"answer" #"0"))))))
                          url0
                          (build-list 7 (lambda (i) i))))))
     
     (test-suite
      "servlet/web.ss"
      ; XXX test web.ss
      )
     
     )))