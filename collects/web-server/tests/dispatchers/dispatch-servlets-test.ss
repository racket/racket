(module dispatch-servlets-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "sxml.ss" ("lizorkin" "sxml.plt" 1 4))
           (lib "etc.ss")
           (lib "list.ss")
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
      (servlets:make (box (make-cache-table))
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
  
  (define (test-add-two-numbers t p)
    (let* ([x (random 500)]
           [xs (string->bytes/utf-8 (number->string x))]
           [y (random 500)]
           [ys (string->bytes/utf-8 (number->string y))])
      (test-equal? 
       t
       (let* ([d (mkd p)]
              [k0 (first ((sxpath "//form/@action/text()") (call d url0 empty)))]
              [k1 (first ((sxpath "//form/@action/text()") (call d (format "~a?number=~a" k0 xs)
                                                                 (list (make-binding:form #"number" xs)))))]
              [n (first ((sxpath "//p/text()") (call d (format "~a?number=~a" k1 ys)
                                                     (list (make-binding:form #"number" ys)))))])
         n)
       (format "The sum is ~a" (+ x y)))))
  
  (define test-servlets (build-path (collection-path "web-server") "tests" "servlets"))
  (define example-servlets (build-path (collection-path "web-server") "default-web-root" "servlets" "examples/"))
  
  (define dispatch-servlets-tests
    (test-suite
     "Servlets"
     
     ; XXX test update cache
     ; XXX redirect/get
     ; XXX web-cells
     
     (test-pred "configure.ss"
                string?
                (let* ([d (mkd (build-path example-servlets 'up "configure.ss"))]
                       [k0 (first ((sxpath "//form/@action/text()") (call d url0 empty)))])
                  k0))
     
     (test-suite
      "Examples"
      (test-equal? "hello.ss - loading"
                   (let* ([d (mkd (build-path example-servlets "hello.ss"))]
                          [t0 (first ((sxpath "//p/text()") (call d url0 empty)))])
                     t0)
                   "Hello, Web!")
      (test-add-two-numbers "add.ss - send/suspend"
                            (build-path example-servlets "add.ss"))
      (test-add-two-numbers "add-v2.ss - send/suspend, version 2"
                            (build-path example-servlets "add-v2.ss"))
      (test-add-two-numbers "add-ssd.ss - send/suspend/dispatch"
                            (build-path example-servlets "add-ssd.ss"))
      (test-equal? "count.ss - state"
                   (let* ([d (mkd (build-path example-servlets "count.ss"))]
                          [ext (lambda (c)
                                 (rest (regexp-match #rx"This servlet was called (.+) times and (.+) times since loaded on" c)))]
                          [c1 (ext (first ((sxpath "//p/text()") (call d url0 empty))))]
                          [c2 (ext (first ((sxpath "//p/text()") (call d url0 empty))))])
                     (list c1 c2))
                   (list (list "1" "1")
                         (list "2" "1")))
      (test-equal? "dir.ss - current-directory"
                   (let* ([d (mkd (build-path example-servlets "dir.ss"))]
                          [t0 (first ((sxpath "//p/em/text()") (call d url0 empty)))])
                     t0)
                   (path->string example-servlets))
      (test-pred "quiz.ss - send/suspend"
                 string?
                 (let* ([d (mkd (build-path example-servlets "quiz.ss"))])
                   (foldl (lambda (_ k)
                            (first ((sxpath "//form/@action/text()") (call d k (list (make-binding:form #"answer" #"0"))))))
                          url0
                          (build-list 7 (lambda (i) i)))))
      (test-equal? "cut.ss - current-url-transform"
                   (let* ([d (mkd (build-path example-servlets "cut.ss"))]
                          [k0 (first ((sxpath "//a/@href/text()") (call d url0 empty)))])
                     k0)
                   "#")
      (test-equal? "clear.ss - current-servlet-continuation-expiration-handler, clear-continuation-table!, send/finish, send/forward"
                   (let* ([d (mkd (build-path example-servlets "clear.ss"))]
                          [k0 (first ((sxpath "//a/@href/text()") (call d url0 empty)))]
                          [k1 (first ((sxpath "//a/@href/text()") (call d k0 empty)))]
                          [k0-expired (first ((sxpath "//body/text()") (call d k0 empty)))]
                          [done (first ((sxpath "//body/text()") (call d k1 empty)))]
                          [k1-expired (first ((sxpath "//body/text()") (call d k1 empty)))])
                     (list k0-expired
                           done
                           k1-expired))
                   (list "Expired"
                         "Done."
                         "Expired"))
      ; XXX Broken
      #;(test-equal? "adjust.ss - adjust-timeout!"
                   (let* ([d (mkd (build-path example-servlets "adjust.ss"))]
                          [k0 (first ((sxpath "//a/@href/text()") (call d url0 empty)))])
                     (sleep 3)
                     (call d k0 empty))
                   "#")))))