(module dispatch-lang-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "sxml.ss" ("lizorkin" "sxml.plt" 1 4))
           (lib "etc.ss")
           (lib "list.ss")
           (lib "request-structs.ss" "web-server" "private")
           (lib "namespace.ss" "web-server" "configuration")
           (prefix lang: (lib "dispatch-lang.ss" "web-server" "dispatchers"))
           "../util.ss")
  (provide dispatch-lang-tests)
  
  (define (mkd p)
    (lang:make #:url->path (lambda _ (values p url0s))
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
  (define url0 "http://test.com/servlets/example.ss")
  (define url0s (list (build-path "servlets") (build-path "example.ss")))
  
  (define example-servlets (build-path (collection-path "web-server") "default-web-root" "htdocs" "lang-servlets/"))
  
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
       (format "The answer is ~a" (+ x y)))))
  
  (define (test-double-counters t p)
    (define d (mkd p))
    (define (invoke u)
      (define sx (call d u empty))
      (define ks ((sxpath "//div/div/a/@href/text()") sx))
      (values ((sxpath "//div/div/h3/text()") sx)
              (first ks)
              (second ks)))
    (test-equal? t
                 (let*-values ([(v0.0 0.0+1 0.0+2) (invoke url0)]
                               ; One add
                               [(v1.0 1.0+1 1.0+2) (invoke 0.0+1)]
                               [(v0.1 0.1+1 0.1+2) (invoke 0.0+2)]
                               ; Two adds
                               [(v2.0 2.0+1 2.0+2) (invoke 1.0+1)]
                               [(v1.1 1.1+1 1.1+2) (invoke 0.1+1)]
                               [(_v1.1 _1.1+1 _1.1+2) (invoke 1.0+2)]
                               [(v0.2 0.2+1 0.2+2) (invoke 0.1+2)])
                   (list v0.0
                         v1.0 v0.1
                         v2.0 v1.1 _v1.1 v0.2))
                 (list (list "0" "0")
                       (list "1" "0") (list "0" "1")
                       (list "2" "0") (list "1" "1") (list "1" "1") (list "0" "2"))))
  
  (define dispatch-lang-tests
    (test-suite
     "Web Language"
          
     (test-add-two-numbers
      "add-param.ss - Parameters, s/s/u (should fail)"
      (build-path example-servlets "add-param.ss"))
     
     (test-add-two-numbers
      "add-simple.ss - Web Parameters, s/s/u"
      (build-path example-servlets "add-simple.ss"))
     
     (test-add-two-numbers
      "add.ss - s/s/u"
      (build-path example-servlets "add.ss"))
     
     ; XXX
     (let* ([x (random 500)]
            [xs (string->bytes/utf-8 (number->string x))]
            [y (random 500)]
            [ys (string->bytes/utf-8 (number->string y))])
       (test-equal? 
        "add01.ss - no s/s, uri"
        (let* ([d (mkd (build-path example-servlets "add01.ss"))]
               [k0 (first ((sxpath "//form/@action/text()") (call d url0 empty)))]
               [k1 (first ((sxpath "//form/@action/text()") (call d (format "~a?first=~a" k0 xs) (list (make-binding:form #"first" xs)))))]
               [n (first ((sxpath "//p/text()") (call d (format "~a?first=~a&second=~a" k1 xs ys)
                                                      (list (make-binding:form #"first" xs)
                                                            (make-binding:form #"second" ys)))))])
          n)
        (format "The answer is ~a" (+ x y))))
     
     (test-add-two-numbers
      "add02.ss - s/s/u, uri"
      (build-path example-servlets "add02.ss"))
     
     ; XXX
     (test-add-two-numbers
      "add03.ss - s/s/h"
      (build-path example-servlets "add03.ss"))
     
     (test-add-two-numbers
      "add04.ss - s/s/u"
      (build-path example-servlets "add04.ss"))
     
     (test-add-two-numbers
      "add05.ss - extract-proc/url and embed-proc/url"
      (build-path example-servlets "add05.ss"))
     
     (test-add-two-numbers
      "add06.ss - send/suspend/dispatch"
      (build-path example-servlets "add06.ss"))
     
     (test-double-counters
      "wc-fake.ss - no cells"
      (build-path example-servlets "wc-fake.ss"))
     
     (test-double-counters
      "wc.ss - make-web-cell web-cell-ref web-cell-shadow"
      (build-path example-servlets "wc.ss"))
     
     (test-double-counters
      "wc-comp.ss - make-web-cell web-cell-ref web-cell-shadow web-cell-component"
      (build-path example-servlets "wc-comp.ss"))
     
     ; XXX test web-extras.ss - redirect/get
     ; XXX test web-cells.ss - web-cell?
     ; XXX test check-dir.ss quiz-lib.ss quiz01.ss quiz02.ss
     )))