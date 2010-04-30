#lang racket/base
(require racunit
         mzlib/etc
         mzlib/list
         web-server/http
         web-server/private/cache-table
         web-server/private/web-server-structs
         web-server/configuration/namespace
         web-server/servlet/setup
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets)
         "servlet-test-util.rkt"
         "../util.rkt")
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
   
   (test-pred "configure.rkt"
              string?
              (let* ([d (mkd (build-path example-servlets 'up "configure.rkt"))]
                     [k0 (simple-xpath* '(form #:action) (call d url0 empty))])
                k0))
   
   (test-suite
    "Examples"
    (test-equal? "hello.rkt - loading"
                 (let* ([d (mkd (build-path example-servlets "hello.rkt"))]
                        [t0 (simple-xpath* '(p) (call d url0 empty))])
                   t0)
                 "Hello, Web!")
    (test-add-two-numbers mkd "add.rkt - send/suspend"
                          (build-path example-servlets "add.rkt"))
    (test-add-two-numbers mkd "add-v2.rkt - send/suspend, version 2"
                          (build-path example-servlets "add-v2.rkt"))
    (test-add-two-numbers mkd "add-ssd.rkt - send/suspend/dispatch"
                          (build-path example-servlets "add-ssd.rkt"))
    (test-add-two-numbers mkd "add-formlets.rkt - send/formlet"
                          (build-path example-servlets "add-formlets.rkt"))
    (test-equal? "count.rkt - state"
                 (let* ([d (mkd (build-path example-servlets "count.rkt"))]
                        [ext (lambda (c)
                               (rest (regexp-match #rx"This servlet was called (.+) times and (.+) times since loaded on" c)))]
                        [c1 (ext (simple-xpath* '(p) (call d url0 empty)))]
                        [c2 (ext (simple-xpath* '(p) (call d url0 empty)))])
                   (list c1 c2))
                 (list (list "1" "1")
                       (list "2" "1")))
    (test-equal? "dir.rkt - current-directory"
                 (let* ([d (mkd (build-path example-servlets "dir.rkt"))]
                        [t0 (simple-xpath* '(p em) (call d url0 empty))])
                   t0)
                 (path->string example-servlets))
    (test-pred "quiz.rkt - send/suspend"
               string?
               (let* ([d (mkd (build-path example-servlets "quiz.rkt"))])
                 (foldl (lambda (_ k)
                          (simple-xpath* '(form #:action) (call d k (list (make-binding:form #"answer" #"0")))))
                        url0
                        (build-list 7 (lambda (i) i)))))
    (test-equal? "clear.rkt - current-servlet-continuation-expiration-handler, clear-continuation-table!, send/finish, send/forward"
                 (let* ([d (mkd (build-path example-servlets "clear.rkt"))]
                        [k0 (simple-xpath* '(a #:href) (call d url0 empty))]
                        [k1 (simple-xpath* '(a #:href) (call d k0 empty))]
                        [k0-expired (simple-xpath* '(body) (call d k0 empty))]
                        [done (simple-xpath* '(body) (call d k1 empty))]
                        [k1-expired (simple-xpath* '(body) (call d k1 empty))])
                   (list k0-expired
                         done
                         k1-expired))
                 (list "Expired"
                       "Done."
                       "Expired"))
    
    (test-double-counters
     mkd
     "wc-fake.rkt - no cells"
     (build-path example-servlets "wc-fake.rkt"))
    
    (test-double-counters
     mkd
     "wc.rkt - make-web-cell web-cell-ref web-cell-shadow"
     (build-path example-servlets "wc.rkt"))
    
    ; XXX Broken
    #;(test-equal? "adjust.rkt - adjust-timeout!"
                   (let* ([d (mkd (build-path example-servlets "adjust.rkt"))]
                          [k0 (first ((sxpath "//a/@href/text()") (call d url0 empty)))])
                     (sleep 3)
                     (call d k0 empty))
                   "#"))))
