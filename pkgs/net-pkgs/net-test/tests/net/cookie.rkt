#lang scheme
(require net/cookie tests/eli-tester)

;; cookie tests --- JBM, 2006-12-01

(provide tests)
(module+ main (tests))
(define (tests)
  ;; cookie-test : (cookie -> cookie) string -> test
  (define (cookie-test fn expected)
    (test (print-cookie (fn (set-cookie "a" "b"))) => expected))
  ;; RC = "reverse curry"
  (define (RC f arg2) (λ (arg1) (f arg1 arg2)))
  ;; o = compose
  (define-syntax o
    (syntax-rules ()
      [(o f) f]
      [(o f g h ...) (λ (x) (o/* x f g h ...))]))
  (define-syntax o/*
    (syntax-rules ()
      [(o/* x) x]
      [(o/* x f g ...) (f (o/* x g ...))]))
  
  (define (tests)
    
    ;; test the most basic functionality
    (cookie-test (λ (x) x) "a=b")
    
    ;; test each modifier individually
    (cookie-test (RC cookie:add-comment "set+a+to+b")
                 "a=b; Comment=set+a+to+b")
    (cookie-test (RC cookie:add-comment "a comment with spaces")
                 "a=b; Comment=\"a comment with spaces\"")
    (cookie-test (RC cookie:add-comment "the \"risks\" involved in waking")
                 "a=b; Comment=\"the \\\"risks\\\" involved in waking\"")
    (cookie-test (RC cookie:add-comment "\"already formatted\"")
                 "a=b; Comment=\"already formatted\"")
    (cookie-test (RC cookie:add-comment "\"problematic \" internal quote\"")
                 "a=b; Comment=\"\\\"problematic \\\" internal quote\\\"\"")
    (cookie-test (RC cookie:add-comment "contains;semicolon")
                 "a=b; Comment=\"contains;semicolon\"")
    (cookie-test (RC cookie:add-domain ".example.net")
                 "a=b; Domain=.example.net")
    (cookie-test (RC cookie:add-max-age 100)
                 "a=b; Max-Age=100")
    (cookie-test (RC cookie:add-path "/whatever/wherever/")
                 "a=b; Path=/whatever/wherever/")
    (cookie-test (RC cookie:add-path "a+path")
                 "a=b; Path=a+path")
    (cookie-test (RC cookie:add-path "\"/already/quoted/\"")
                 "a=b; Path=\"/already/quoted/\"")
    (cookie-test (RC cookie:secure #t)
                 "a=b; Secure")
    (cookie-test (RC cookie:secure #f)
                 "a=b")
    (cookie-test (RC cookie:version 12)
                 "a=b; Version=12")
    
    ;; test combinations
    (cookie-test (o (RC cookie:add-comment "set+a+to+b")
                    (RC cookie:add-domain ".example.net"))
                 "a=b; Comment=set+a+to+b; Domain=.example.net")
    (cookie-test (o (RC cookie:add-max-age 300)
                    (RC cookie:secure #t))
                 "a=b; Max-Age=300; Secure")
    (cookie-test (o (RC cookie:add-path "/whatever/wherever/")
                    (RC cookie:version 10)
                    (RC cookie:add-max-age 20))
                 "a=b; Max-Age=20; Path=/whatever/wherever/; Version=10")
    
    ;; test error cases
    (let ()
      (define-syntax cookie-error-test
        (syntax-rules ()
          [(cookie-error-test e)
           (test (e (set-cookie "a" "b")) =error> cookie-error?)]))
      (cookie-error-test (RC cookie:add-comment "illegal character #\000"))
      (cookie-error-test (RC cookie:add-max-age -10))
      (cookie-error-test (RC cookie:add-domain "doesntstartwithadot.example.com"))
      (cookie-error-test (RC cookie:add-domain "bad domain.com"))
      (cookie-error-test (RC cookie:add-domain ".bad-domain;com")))
    
    ; cookie value
    (test
     (cookie-value? "value")
     (cookie-value? "(")
     (cookie-value? "!")
     (cookie-value? ")")
     (cookie-value? ")!")
     (cookie-value? "(!")
     (cookie-value? "(!)")
     (cookie-value? "!)"))

    
    
    )
  
  (test do (tests)))

(module+ test (require (submod ".." main))) ; for raco test & drdr
