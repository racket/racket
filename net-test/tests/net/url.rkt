#lang racket
(require net/url tests/eli-tester)

(provide tests)
(module+ main (test do (tests)))
(define (tests)
  (test
   ;; Test the current-proxy-servers parameter can be set
   (parameterize ([current-proxy-servers '(("http" "proxy.com" 3128))])
     (current-proxy-servers))
   => '(("http" "proxy.com" 3128))))

(module+ test (require (submod ".." main))) ; for raco test & drdr
