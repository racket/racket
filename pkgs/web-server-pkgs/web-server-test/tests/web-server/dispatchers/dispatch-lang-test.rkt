#lang racket/base
(require rackunit
         mzlib/etc
         mzlib/list
         racket/path
         racket/runtime-path
         web-server/dispatchers/dispatch
         web-server/http
         web-server/configuration/namespace
         web-server/servlet/setup
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets)
         "servlet-test-util.rkt"
         "../util.rkt")
(provide dispatch-lang-tests)

#;(define (mkd p)
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

(define default-web-root
  (path-only 
   (collection-file-path "default-web-root/configuration-table.rkt" "web-server")))

(define example-servlets 
  (build-path default-web-root "htdocs" "lang-servlets/"))

(define dispatch-lang-tests
  (test-suite
   "Web Language"
   
   (test-exn
    "add-param.rkt - Parameters, s/s/u (should fail)"
    exn:fail?
    (lambda ()
      (let* ([xs #"10"]
             [ys #"17"]
             [d (mkd (build-path example-servlets "add-param.rkt"))]
             [k0 (simple-xpath* '(form #:action) (call d url0 empty))]
             [k1 (simple-xpath* '(form #:action) (call d (format "~a?number=~a" k0 xs)
                                                       (list (make-binding:form #"number" xs))))]
             [n (simple-xpath* '(p) (call d (format "~a?number=~a" k1 ys)
                                                    (list (make-binding:form #"number" ys))))])
        n)))
   
   (test-add-two-numbers
    mkd
    "add-simple.rkt - Web Parameters, s/s/u"
    (build-path example-servlets "add-simple.rkt"))
   
   (test-add-two-numbers
    mkd
    "add.rkt - s/s/u"
    (build-path example-servlets "add.rkt"))
   
   (let* ([x (random 500)]
          [xs (string->bytes/utf-8 (number->string x))]
          [y (random 500)]
          [ys (string->bytes/utf-8 (number->string y))])
     (test-equal? 
      "add01.rkt - no s/s, uri"
      (let* ([d (mkd (build-path example-servlets "add01.rkt"))]
             [k0 (simple-xpath* '(form #:action) (call d url0 empty))]
             [k1 (simple-xpath* '(form #:action) (call d (format "~a?first=~a" url0 xs) (list (make-binding:form #"first" xs))))]
             [n (simple-xpath* '(p) (call d (format "~a?first=~a&second=~a" url0 xs ys)
                                                    (list (make-binding:form #"first" xs)
                                                          (make-binding:form #"second" ys))))])
        n)
      (format "The answer is: ~a" (+ x y))))
   
   (test-add-two-numbers
    mkd
    "add02.rkt - s/s/u, uri"
    (build-path example-servlets "add02.rkt"))

   (test-add-two-numbers
    mkd
    "add02-base.rkt - s/s/u, uri"
    (build-path example-servlets "add02-base.rkt"))

   
   ; XXX Use kont
   #;(test-add-two-numbers
      mkd
      "add03.rkt - s/s/h"
      (build-path example-servlets "add03.rkt"))
   
   (test-add-two-numbers
    mkd
    "add04.rkt - s/s/u"
    (build-path example-servlets "add04.rkt"))  
   
   (test-add-two-numbers
    mkd
    "add06.rkt - send/suspend/dispatch"
    (build-path example-servlets "add06.rkt"))
   
   (test-add-two-numbers
    mkd
    "add-native.rkt - native continuation parts"
    (build-path example-servlets "add-native.rkt"))
   
   (test-add-two-numbers
    mkd
    "add-soft.rkt - soft state"
    (build-path example-servlets "add-soft.rkt"))
   
   ; XXX test something is not d-c
   (test-double-counters
    mkd
    "wc-fake.rkt - no cells"
    (build-path example-servlets "wc-fake.rkt"))
   
   (test-double-counters
    mkd
    "wc.rkt - make-web-cell web-cell-ref web-cell-shadow"
    (build-path example-servlets "wc.rkt"))
   
   (test-double-counters
    mkd
    "wc-comp.rkt - make-web-cell web-cell-ref web-cell-shadow web-cell-component"
    (build-path example-servlets "wc-comp.rkt"))
   
   (test-equal? "check-dir.rkt"
                (let* ([d (mkd (build-path example-servlets "check-dir.rkt"))]
                       [t0 (simple-xpath* '(h2) (call d url0 empty))])
                  t0)
                (format "The current directory: ~a/" 
                        (path->string
                         (normalize-path example-servlets))))
   
   ; XXX Use kont
   #;(test-equal? "quiz01.rkt"
                  (let* ([d (mkd (build-path example-servlets "quiz01.rkt"))]
                         [last
                          (foldl (lambda (_ k)
                                   (first ((sxpath "//form/@action/text()") 
                                           (call d k (list (make-binding:form #"answer" #"0"))))))
                                 url0
                                 (build-list 7 (lambda (i) i)))])
                    (first ((sxpath "//h1/text()") (call d last (list (make-binding:form #"answer" #"0"))))))
                  "Quiz Results")
   ; XXX Use kont
   #;(test-equal? "quiz02.rkt"
                  (let* ([d (mkd (build-path example-servlets "quiz02.rkt"))]
                         [last
                          (foldl (lambda (_ k)
                                   (first ((sxpath "//form/@action/text()")
                                           (call d k (list (make-binding:form #"answer" #"0"))))))
                                 url0
                                 (build-list 7 (lambda (i) i)))])
                    (first ((sxpath "//h1/text()") (call d last (list (make-binding:form #"answer" #"0"))))))
                  "Quiz Results")
   
   ; XXX test web-extras.rkt - redirect/get
   
   (let* ([x (random 500)]
          [xs (string->bytes/utf-8 (number->string x))]
          [y (random 500)]
          [ys (string->bytes/utf-8 (number->string y))])
     (test-equal? 
      "redirectget.rkt"
      (let* ([d (mkd (build-path example-servlets "redirectget.rkt"))]
             [k0 (simple-xpath* '(form #:action) (call d url0 empty))]
             [k1 (call d k0 empty)])
        k1)
      ""))
   
   
   ))
