#lang racket/base
(require xml/path
         tests/eli-tester)

(define some-page
  '(html (body (p ([class "awesome"]) "Hey") (p "Bar"))))
(test (se-path*/list '(p) some-page) 
      => '("Hey" "Bar")
      (se-path* '(p) some-page)
      => "Hey"
      (se-path* '(p #:class) some-page)
      => "awesome"
      (se-path*/list '(body) some-page)
      => '((p ((class "awesome")) "Hey") (p "Bar"))
      
      (se-path*/list '() '(p ((class "awesome")) "Hey"))
      => '((p ((class "awesome")) "Hey")
           "Hey")
      
      (se-path*/list '() some-page)
      => '((html (body (p ((class "awesome")) "Hey") (p "Bar"))) 
           (body (p ((class "awesome")) "Hey") (p "Bar"))
           (p ((class "awesome")) "Hey")
           "Hey"
           (p "Bar")
           "Bar")
      
      (se-path*/list '(p) '(html (body (p "Hey") (p "Bar")))) 
      => (list "Hey" "Bar")
      (se-path* '(p) '(html (body (p "Hey"))))
      => "Hey"
      (se-path* '(p #:bar) '(html (body (p ([bar "Zog"]) "Hey"))))
      => "Zog")
