#lang scheme/base
(require schemeunit
         (planet "sxml.ss" ("lizorkin" "sxml.plt" 2 0))
         mzlib/etc
         mzlib/list
         web-server/http
         web-server/private/cache-table
         web-server/private/web-server-structs
         web-server/configuration/namespace
         web-server/servlet/setup
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets)
         "servlet-test-util.ss"
         "../util.ss")
(provide dispatch-servlets-tests)

(current-server-custodian (current-custodian))

(define (mkd p)
  (define-values (! u->s)
    (servlets:make-cached-url->servlet
     (lambda _ (values p url0s))
     (make-default-path->servlet)))
  (define d
    (servlets:make u->s
                   #:responders-servlet-loading
                   (lambda (u exn)
                     (raise exn))
                   #:responders-servlet
                   (lambda (u exn)
                     (raise exn))))
  d)

(define example-servlets 
  (build-path (collection-path "web-server") "default-web-root" "htdocs" "servlets" "examples/"))

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
    (test-add-two-numbers mkd "add.ss - send/suspend"
                          (build-path example-servlets "add.ss"))
    (test-add-two-numbers mkd "add-v2.ss - send/suspend, version 2"
                          (build-path example-servlets "add-v2.ss"))
    (test-add-two-numbers mkd "add-ssd.ss - send/suspend/dispatch"
                          (build-path example-servlets "add-ssd.ss"))
    (test-add-two-numbers mkd "add-formlets.ss - send/formlet"
                          (build-path example-servlets "add-formlets.ss"))
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
    
    (test-double-counters
     mkd
     "wc-fake.ss - no cells"
     (build-path example-servlets "wc-fake.ss"))
    
    (test-double-counters
     mkd
     "wc.ss - make-web-cell web-cell-ref web-cell-shadow"
     (build-path example-servlets "wc.ss"))
    
    ; XXX Broken
    #;(test-equal? "adjust.ss - adjust-timeout!"
                   (let* ([d (mkd (build-path example-servlets "adjust.ss"))]
                          [k0 (first ((sxpath "//a/@href/text()") (call d url0 empty)))])
                     (sleep 3)
                     (call d k0 empty))
                   "#"))))