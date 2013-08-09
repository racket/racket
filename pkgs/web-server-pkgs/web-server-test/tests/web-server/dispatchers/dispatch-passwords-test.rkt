#lang racket/base
(require rackunit
         (only-in mzlib/file
                  make-temporary-file)
         net/url
         racket/promise
         racket/runtime-path
         racket/list
         racket/path
         racket/serialize
         web-server/http
         web-server/dispatchers/dispatch
         (prefix-in passwords: web-server/dispatchers/dispatch-passwords)
         "../util.rkt")
(provide dispatch-passwords-tests)

(require/expose web-server/dispatchers/dispatch-passwords
                (read-passwords))

(define default-web-root
  (path-only 
   (collection-file-path "default-web-root/configuration-table.rkt" "web-server")))

(define default-passwords (build-path default-web-root "passwords"))
(define test-passwords (make-temporary-file))
(define (write-test-passwords!)
  (with-output-to-file test-passwords
    (lambda ()
      (write ''(("secret stuff" "/secret(/.*)?" (bubba "bbq") (|Billy| "BoB")
                                (aladdin "open sesame")))))
    #:exists 'truncate/replace))

(write-test-passwords!)

(define (compat #:password-file pf
                #:authentication-responder ar)
  (let-values ([(update-password-cache! password-check)
                (passwords:password-file->authorized? pf)])
    (values update-password-cache!
            (passwords:make
             (passwords:make-basic-denied?/path
              password-check)
             #:authentication-responder ar))))

(define (runt applies? authorized?)
  (let/ec esc
    (define-values (_ d) (compat #:password-file test-passwords
                                 #:authentication-responder 
                                 (lambda (u h) (esc h))))
    (define-values (c i o) (make-mock-connection #""))
    (d c (make-request #"GET"
                       (if applies? 
                           (string->url "http://host/secret/something")
                           (string->url "http://host/not-secret"))
                       (if authorized?
                           (list (make-header #"Authorization" #"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="))
                           empty)
                       (delay empty) #"" "host" 80 "client"))))

(define dispatch-passwords-tests
  (test-suite
   "Passwords"
   
   (test-suite
    "Default configuration"
    
    (test-not-false
     "Distribution file parses"
     (read-passwords default-passwords))
    
    (test-exn
     "False not allowed as password-file"
     exn?
     (lambda ()
       (passwords:password-file->authorized? #f)))
    
    (test-exn "authorized"
              exn:dispatcher?
              (lambda () (runt #t #t)))
    (test-equal? "not authorized"
                 (let ([v (runt #t #f)])
                   (list (header-field v) (header-value v)))
                 (list #"WWW-Authenticate" #"Basic realm=\"secret stuff\""))
    (test-exn "does not apply"
              exn:dispatcher?
              (lambda ()
                (runt #f #f)))
    (test-exn "does not apply (authd)"
              exn:dispatcher?
              (lambda ()
                (runt #f #t))))
   
   ; XXX test refresh cache
   
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests dispatch-passwords-tests))
