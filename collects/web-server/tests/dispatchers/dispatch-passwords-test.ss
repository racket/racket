(module dispatch-passwords-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "util.ss" ("schematics" "schemeunit.plt" 2))
           (lib "file.ss")
           (lib "url.ss" "net")
           (lib "list.ss")
           (lib "request-structs.ss" "web-server" "private")
           (lib "dispatch.ss" "web-server" "dispatchers")
           (prefix passwords: (lib "dispatch-passwords.ss" "web-server" "dispatchers"))
           "../util.ss")
  (provide dispatch-passwords-tests)
  
  (require/expose (lib "dispatch-passwords.ss" "web-server" "dispatchers")
                  (read-passwords))
  
  (define default-passwords (build-path (collection-path "web-server") "default-web-root" "passwords"))
  (define test-passwords (make-temporary-file))
  (define (write-test-passwords!)
    (with-output-to-file test-passwords
      (lambda ()
        (with-input-from-file default-passwords
          (lambda ()
            (write (read)))))
      'truncate/replace))
  
  (write-test-passwords!)
  
  (define (runt applies? authorized?)
    (let/ec esc
      (define-values (_ d) (passwords:make #:password-file test-passwords
                                           #:authentication-responder 
                                           (lambda (u h) (esc h))))
      (define-values (c i o) (make-mock-connection #""))
      (d c (make-request 'get 
                         (if applies? 
                             (string->url "http://host/secret/something")
                             (string->url "http://host/not-secret"))
                         (if authorized?
                             (list (make-header #"Authorization" #"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="))
                             empty)
                         empty #"" "host" 80 "client"))))
  
  (define dispatch-passwords-tests
    (test-suite
     "Passwords"
     
     (test-not-false
      "Distribution file parses"
      (read-passwords default-passwords))
     
     (test-exn "authorized"
               exn:dispatcher?
               (lambda () (runt #t #t)))
     (test-equal? "not authorized"
                  (runt #t #f)
                  `(WWW-Authenticate . " Basic realm=\"secret stuff\""))
     (test-exn "does not apply"
               exn:dispatcher?
               (lambda ()
                 (runt #f #f)))
     (test-exn "does not apply (authd)"
               exn:dispatcher?
               (lambda ()
                 (runt #f #t)))
     
     ; XXX test refresh cache
     
     )))