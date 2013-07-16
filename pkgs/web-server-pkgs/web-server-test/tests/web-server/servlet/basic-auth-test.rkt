#lang racket
(require rackunit
         web-server/http
         net/url)
(provide basic-auth-tests)

(define (make-req hs)
  (make-request 
   #"GET" (string->url "http://test.com/foo")
   hs
   (delay empty) #f
   "host" 80 "client"))

(define (header->cons h)
  (cons (header-field h) (header-value h)))

(define basic-auth-tests
  (test-suite
   "Basic Authentication"
   
   (test-case
    "make-basic-auth-header"
    (check-equal? (header->cons (make-basic-auth-header "realm"))
                  (cons #"WWW-Authenticate"
                        #"Basic realm=\"realm\"")))
   
   (test-case
    "Simple"
    (check-equal? (request->basic-credentials
                   (make-req (list (make-header #"Authorization" #"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="))))
                  (cons #"Aladdin" #"open sesame")))
   
   (test-case
    "Value error"
    (check-false (request->basic-credentials (make-req (list (make-header #"Authorization" #"Basic adfadQWxhZGRpb124134jpvcGVu="))))))
   
   (test-case
    "No header"
    (check-false (request->basic-credentials (make-req (list)))))
   
   (test-case
    "Case"
    (check-equal? (request->basic-credentials (make-req (list (make-header #"AuthoRIZation" #"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="))))
                  (cons #"Aladdin" #"open sesame")))))
