#lang racket/base
(require web-server/lang/stuff-url
         web-server/stuffers
         rackunit
         net/url
         mzlib/serialize
         "../util.rkt")
(provide stuff-url-tests)

(define uri0 (string->url "www.google.com")) 

(define test-stuffer serialize-stuffer)

(define (stuff-unstuff svl uri)
  (let ([result-uri (stuff-url test-stuffer uri svl)])
    (unstuff-url test-stuffer result-uri)))
(define (cidentity v)
  (deserialize 
   (stuff-unstuff (serialize v) uri0)))

(define the-dispatch
  `(lambda (k*v)
     (lambda (k*v)
       ((car k*v) k*v))))

(define m00 'web-server/default-web-root/htdocs/lang-servlets/mm00) 
(define m01 'web-server/default-web-root/htdocs/lang-servlets/mm01) 

(define stuff-url-tests
  (test-suite
   "Stuff URL"
   
   (test-suite
    "(compose unstuff-url stuff-url) is identity"
    (test-case "Integers" (check-equal? (cidentity 3) 3))
    (test-case "Symbols" (check-equal? (cidentity 'foo) 'foo))
    (test-case "Strings" (check-equal? (cidentity "Bar") "Bar"))
    (test-case "Vectors" (check-equal? (cidentity (vector 3 1 4)) (vector 3 1 4))))
   
   (test-suite
    "stuffed-url? works"
    (test-case "Not stuffed URL" (check-false (stuffed-url? uri0)))
    (test-case "Integers" (check-true (stuffed-url? (stuff-url test-stuffer uri0 (serialize 3)))))
    (test-case "Symbols" (check-true (stuffed-url? (stuff-url test-stuffer uri0 (serialize 'foo)))))
    (test-case "Strings" (check-true (stuffed-url? (stuff-url test-stuffer uri0 (serialize "Bar")))))
    (test-case "Vectors" (check-true (stuffed-url? (stuff-url test-stuffer uri0 (serialize (vector 3 1 4)))))))
   
   (test-case 
    "Using stuff-url with lang.rkt"
    (let-values ([(ev) (make-eval/mod-path m00)])
      (let* ([k0 (stuff-unstuff (ev '(serialize (dispatch-start start 'foo)))
                                uri0)]
             [k1 (stuff-unstuff (ev `(serialize (dispatch ,the-dispatch (list (deserialize ',k0) 1))))
                                uri0)]
             [k2 (stuff-unstuff (ev `(serialize (dispatch ,the-dispatch (list (deserialize ',k1) 2))))
                                uri0)])
        (check-true (= 6 (ev `(dispatch ,the-dispatch (list (deserialize ',k2) 3))))))))))
