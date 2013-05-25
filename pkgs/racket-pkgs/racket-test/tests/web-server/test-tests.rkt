#lang racket/base
(require rackunit
         racket/list
         racket/match
         xml/path
         web-server/test
         web-server/servlet-dispatch
         net/url
         racket/promise
         web-server/http)

(define (test-add-two-numbers -s>)
  (define x (random 500))
  (define xs (string->bytes/utf-8 (number->string x)))
  (define y (random 500))
  (define ys (string->bytes/utf-8 (number->string y)))

  (match-define (cons h r0) (-s> #:headers? #t))
  (define k0 (se-path* '(form #:action) r0))
  (define i0 (se-path* '(form input #:name) r0))
  (define r1 
    (-s> (format "~a?~a=~a" k0 i0 xs)
         (list (make-binding:form (string->bytes/utf-8 i0) xs))))
  (define k1 (se-path* '(form #:action) r1))
  (define i1 (se-path* '(form input #:name) r1))
  (define r2 
    (-s> (format "~a?~a=~a" k1 i1 ys)
         (list (make-binding:form (string->bytes/utf-8 i1) ys))))
  (define n (se-path* '(p) r2))
  (check-equal? n
                (format "The answer is ~a" (+ x y)))
  
  (let ()
    (define r2 
      (-s> (format "~a?~a=~a" k1 i1 ys)
           (list (make-binding:form (string->bytes/utf-8 i1) ys))))
    (check-equal? (se-path* '(html body p) r2)
                  (format "The answer is ~a" (+ x y))))
  
  (let ()    
    (define r2 
      (-s> 
       (make-request #"GET" (string->url (format "~a?~a=~a" k1 i1 ys)) empty
                     (delay (list (make-binding:form (string->bytes/utf-8 i1) ys)))
                     #"" "127.0.0.1" 80 "127.0.0.1")))
    (define n (se-path* '(p) r2))
    (check-equal? n
                  (format "The answer is ~a" (+ x y)))))

(require (prefix-in ex:add1: web-server/default-web-root/htdocs/servlets/examples/add)
         (prefix-in ex:add2: web-server/default-web-root/htdocs/servlets/examples/add-v2)
         (prefix-in ex:lang:add2: web-server/default-web-root/htdocs/lang-servlets/add02))
(require (prefix-in ex:double: web-server/default-web-root/htdocs/servlets/examples/wc))

(define (test-double-counters -s>)
  (define (invoke u)
    (define sx (-s> u))
    (define ks (se-path*/list '(div div a #:href) sx))
    (values (se-path*/list '(div div h3) sx)
            (first ks)
            (second ks)))
  ; One add
  (define-values (v0.0 0.0+1 0.0+2) (invoke ""))
  (check-equal? v0.0 (list "0" "0"))
  (define-values (v1.0 1.0+1 1.0+2) (invoke 0.0+1))
  (check-equal? v1.0 (list "1" "0")) ; XXX infinite loop after this
  (define-values (v0.1 0.1+1 0.1+2) (invoke 0.0+2))
  (check-equal? v0.1 (list "0" "1"))
  ; Two adds
  (define-values (v2.0 2.0+1 2.0+2) (invoke 1.0+1))
  (check-equal? v2.0 (list "2" "0"))
  (define-values (v1.1 1.1+1 1.1+2) (invoke 0.1+1))
  (check-equal? v1.1 (list "1" "1"))
  (define-values (_v1.1 _1.1+1 _1.1+2) (invoke 1.0+2))
  (check-equal? _v1.1 (list "1" "1"))
  (define-values (v0.2 0.2+1 0.2+2) (invoke 0.1+2))
  (check-equal? v0.2 (list "0" "2")))

(define test-tests
  (test-suite "Servlet testing tests"
              (test-case "add1"
                         (test-add-two-numbers 
                          (make-servlet-tester ex:add1:start)))
              (test-case "add2"
                         (test-add-two-numbers 
                          (make-servlet-tester ex:add2:start)))
              (test-case "lang add2"
                         (test-add-two-numbers 
                          (make-dispatcher-tester 
                           (dispatch/servlet ex:lang:add2:start #:stateless? #t))))
              (test-case "double-counters"
                         (test-double-counters
                          (make-servlet-tester ex:double:start)))))
(provide test-tests)

(module+ test
  (require rackunit/text-ui)
  (run-tests test-tests))


