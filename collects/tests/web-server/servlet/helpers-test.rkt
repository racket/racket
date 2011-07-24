#lang racket/base
(require rackunit
         web-server/servlet)
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
     (check-pred response? (let/ec esc (with-errors-to-browser esc (lambda () (error 'error "Hey!"))))))
    (test-case
     "Basic (succ)"
     (check-true (let/ec esc (with-errors-to-browser esc (lambda () #t))))))
   
   (test-suite
    "redirect-to"
    (test-exn "Empty"
              exn:fail:contract?
              (lambda () (redirect-to "")))
    (test-equal? "Code (temp)"  
                 (response-code (redirect-to "http://test.com/foo"))
                 302)
    (test-equal? "Message (temp)" 
                 (response-message (redirect-to "http://test.com/foo"))
                 #"Moved Temporarily")
    (test-equal? "Code" 
                 (response-code (redirect-to "http://test.com/foo" permanently))
                 301)
    (test-equal? "Message" 
                 (response-message (redirect-to "http://test.com/foo" permanently))
                 #"Moved Permanently")
    (test-equal? "URL"
                 (dehead (response-headers (redirect-to "http://test.com/foo")))
                 (list (list #"Location" #"http://test.com/foo")))
    (test-equal? "Headers"
                 (dehead (response-headers (redirect-to "http://test.com/foo" #:headers (list (make-header #"Header" #"Value")))))
                 (list (list #"Location" #"http://test.com/foo")
                       (list #"Header" #"Value"))))
   
   (test-suite
    "redirection-status?"
    (test-case "permanently" (check-true (redirection-status? permanently)))
    (test-case "temporarily" (check-true (redirection-status? temporarily)))
    (test-case "see-other" (check-true (redirection-status? see-other))))))
