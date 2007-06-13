(module helpers-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "response-structs.ss" "web-server" "private")
           (lib "request-structs.ss" "web-server" "private")
           (lib "helpers.ss" "web-server" "servlet"))
  (provide helpers-tests)
  
  (define (dehead hs)
    (map (lambda (h)
           (list (header-field h)
                 (header-value h)))
         hs))
  
  (define helpers-tests
    (test-suite
     "Helpers"
     
     (test-suite
      "with-errors-to-browser"
      (test-case
       "Basic"
       (check-pred list? (with-errors-to-browser (lambda (x) x) (lambda () (error 'error "Hey!")))))
      (test-case
       "Basic (succ)"
       (check-true (with-errors-to-browser (lambda (x) x) (lambda () #t)))))
     
     (test-suite
      "redirect-to"
      (test-equal? "Code (temp)"  
                   (response/basic-code (redirect-to "http://test.com/foo"))
                   302)
      (test-equal? "Message (temp)" 
                   (response/basic-message (redirect-to "http://test.com/foo"))
                   "Moved Temporarily")
      (test-equal? "Code" 
                   (response/basic-code (redirect-to "http://test.com/foo" permanently))
                   301)
      (test-equal? "Message" 
                   (response/basic-message (redirect-to "http://test.com/foo" permanently))
                   "Moved Permanently")
      (test-equal? "URL"
                   (dehead (response/basic-headers (redirect-to "http://test.com/foo")))
                   (list (list #"Location" #"http://test.com/foo")))
      (test-equal? "Headers"
                   (dehead (response/basic-headers (redirect-to "http://test.com/foo" #:headers (list (make-header #"Header" #"Value")))))
                   (list (list #"Location" #"http://test.com/foo")
                         (list #"Header" #"Value"))))
     
     (test-suite
      "redirection-status?"
      (test-case "permanently" (check-true (redirection-status? permanently)))
      (test-case "temporarily" (check-true (redirection-status? temporarily)))
      (test-case "see-other" (check-true (redirection-status? see-other)))))))