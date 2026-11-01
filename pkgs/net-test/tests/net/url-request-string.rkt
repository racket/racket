#lang racket/base
(require net/url-string
         rackunit)

;; Not request strings:
(check-equal? (url->string (url "http" #f "example.com" #f #f null null #f))
              "http://example.com")
(check-equal? (url->string (url #f #f #f #f #f null null #f))
              "")
(check-equal? (url->string (url #f #f #f #f #f null null "x"))
              "#x")
(check-equal? (url->string (url #f #f #f #f #f null (list (cons 'q "1")) #f))
              "?q=1")

;; Request strings:
(check-equal? (url->request-path-string (url "http" #f "example.com" #f #f null null #f))
              "/")
(check-equal? (url->request-path-string (url #f #f #f #f #f null null #f))
              "/")
(check-equal? (url->request-path-string (url #f #f #f #f #f null (list (cons 'q "1")) #f))
              "/?q=1")
(check-equal? (url->request-path-string (url #f #f #f #f #f null null "x"))
              "/")
(check-equal? (url->request-path-string (url #f #f #f #f #f null (list (cons 'q "1")) "x"))
              "/?q=1")

;; Request string with proxy:
(check-equal? (url->request-path-string (url "http" #f "example.com" #f #f null null #f) #t)
              "http://example.com")
(check-equal? (url->request-path-string (url "https" #f "example.com" #f #f null null #f) #t)
              "https://example.com")
(check-equal? (url->request-path-string (url "http" #f "example.com" #f #f null null "x") #t)
              "http://example.com")
(check-equal? (url->request-path-string (url "http" #f "example.com" #f #f null (list (cons 'q "1")) "x") #t)
              "http://example.com?q=1")

(check-equal? (url->request-path-string (url "https" #f "example.com" #f #f null null #f) #f)
              "/")
(check-equal? (url->request-path-string (url "https" #f "example.com" #f #f null null "x") #f)
              "/")
(check-equal? (url->request-path-string (url "https" #f "example.com" #f #t null null "x") #f)
              "/")
(check-equal? (url->request-path-string (url "https" #f "example.com" #f #f null (list (cons 'q "1")) "x") #f)
              "/?q=1")

(check-equal? (url->request-path-string (url #f #f #f 80 #f null null #f) #t)
              "//:80")
(check-equal? (url->request-path-string (url "http" #f #f #f #f null null #f) #t)
              "http:")
(check-equal? (url->request-path-string (url "http" "user" #f #f #f null null #f) #t)
              "http://user@")
(check-equal? (url->request-path-string (url "http" #f #f 80 #f null null #f) #t)
              "http://:80")
