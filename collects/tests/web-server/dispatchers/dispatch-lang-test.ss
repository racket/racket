#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
         (planet "sxml.ss" ("lizorkin" "sxml.plt" 2 0))
         mzlib/etc
         mzlib/list
         web-server/dispatchers/dispatch
         web-server/private/request-structs
         web-server/configuration/namespace
         (prefix-in lang: web-server/dispatchers/dispatch-lang)
         "servlet-test-util.ss"
         "../util.ss")
(provide dispatch-lang-tests)

(define (mkd p)
  (lang:make #:url->path (lambda _ (values p (list p)))
             #:make-servlet-namespace
             (make-make-servlet-namespace)
             #:responders-servlet-loading
             (lambda (u exn)
               ((error-display-handler) (exn-message exn) exn)
               (raise exn))
             #:responders-servlet
             (lambda (u exn)
               ((error-display-handler) (exn-message exn) exn)
               (raise exn))))

(define example-servlets (build-path (collection-path "web-server") "default-web-root" "htdocs" "lang-servlets/"))

(define dispatch-lang-tests
  (test-suite
   "Web Language"
   
   (test-exn
    "add-param.ss - Parameters, s/s/u (should fail)"
    exn:dispatcher?
    (lambda ()
      (let* ([xs #"10"]
             [ys #"17"]
             [d (mkd (build-path example-servlets "add-param.ss"))]
             [k0 (first ((sxpath "//form/@action/text()") (call d url0 empty)))]
             [k1 (first ((sxpath "//form/@action/text()") (call d (format "~a?number=~a" k0 xs)
                                                                (list (make-binding:form #"number" xs)))))]
             [n (first ((sxpath "//p/text()") (call d (format "~a?number=~a" k1 ys)
                                                    (list (make-binding:form #"number" ys)))))])
        n)))
   
   (test-add-two-numbers
    mkd
    "add-simple.ss - Web Parameters, s/s/u"
    (build-path example-servlets "add-simple.ss"))
   
   (test-add-two-numbers
    mkd
    "add.ss - s/s/u"
    (build-path example-servlets "add.ss"))
   
   (let* ([x (random 500)]
          [xs (string->bytes/utf-8 (number->string x))]
          [y (random 500)]
          [ys (string->bytes/utf-8 (number->string y))])
     (test-equal? 
      "add01.ss - no s/s, uri"
      (let* ([d (mkd (build-path example-servlets "add01.ss"))]
             [k0 (first ((sxpath "//form/@action/text()") (call d url0 empty)))]
             [k1 (first ((sxpath "//form/@action/text()") (call d (format "~a?first=~a" url0 xs) (list (make-binding:form #"first" xs)))))]
             [n (first ((sxpath "//p/text()") (call d (format "~a?first=~a&second=~a" url0 xs ys)
                                                    (list (make-binding:form #"first" xs)
                                                          (make-binding:form #"second" ys)))))])
        n)
      (format "The answer is: ~a" (+ x y))))
   
   (test-add-two-numbers
    mkd
    "add02.ss - s/s/u, uri"
    (build-path example-servlets "add02.ss"))
   
   ; XXX Use kont
   #;(test-add-two-numbers
      mkd
      "add03.ss - s/s/h"
      (build-path example-servlets "add03.ss"))
   
   (test-add-two-numbers
    mkd
    "add04.ss - s/s/u"
    (build-path example-servlets "add04.ss"))  
   
   (test-add-two-numbers
    mkd
    "add06.ss - send/suspend/dispatch"
    (build-path example-servlets "add06.ss"))
   
   ; XXX test something is not d-c
   (test-double-counters
    mkd
    "wc-fake.ss - no cells"
    (build-path example-servlets "wc-fake.ss"))
   
   (test-double-counters
    mkd
    "wc.ss - make-web-cell web-cell-ref web-cell-shadow"
    (build-path example-servlets "wc.ss"))
   
   (test-double-counters
    mkd
    "wc-comp.ss - make-web-cell web-cell-ref web-cell-shadow web-cell-component"
    (build-path example-servlets "wc-comp.ss"))
   
   (test-equal? "check-dir.ss"
                (let* ([d (mkd (build-path example-servlets "check-dir.ss"))]
                       [t0 (first ((sxpath "//h2/text()") (call d url0 empty)))])
                  t0)
                (format "The current directory: ~a" (path->string example-servlets)))
   
   ; XXX Use kont
   #;(test-equal? "quiz01.ss"
                  (let* ([d (mkd (build-path example-servlets "quiz01.ss"))]
                         [last
                          (foldl (lambda (_ k)
                                   (first ((sxpath "//form/@action/text()") 
                                           (call d k (list (make-binding:form #"answer" #"0"))))))
                                 url0
                                 (build-list 7 (lambda (i) i)))])
                    (first ((sxpath "//h1/text()") (call d last (list (make-binding:form #"answer" #"0"))))))
                  "Quiz Results")
   ; XXX Use kont
   #;(test-equal? "quiz02.ss"
                  (let* ([d (mkd (build-path example-servlets "quiz02.ss"))]
                         [last
                          (foldl (lambda (_ k)
                                   (first ((sxpath "//form/@action/text()")
                                           (call d k (list (make-binding:form #"answer" #"0"))))))
                                 url0
                                 (build-list 7 (lambda (i) i)))])
                    (first ((sxpath "//h1/text()") (call d last (list (make-binding:form #"answer" #"0"))))))
                  "Quiz Results")
   
   ; XXX test web-extras.ss - redirect/get
   ))
