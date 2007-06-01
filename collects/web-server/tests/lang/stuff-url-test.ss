(module stuff-url-test mzscheme
  (require (lib "stuff-url.ss" "web-server" "lang")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "url.ss" "net")
           (lib "serialize.ss")
           "../util.ss")  
  (provide stuff-url-tests)
  
  (define uri0 (string->url "www.google.com"))  
    
  (define (stuff-unstuff svl uri)
    (let ([result-uri (stuff-url svl uri)])
      (unstuff-url result-uri)))
  (define (cidentity v)
    (deserialize 
     (stuff-unstuff (serialize v) uri0)))
  
  (define the-dispatch
    `(lambda (k*v)
       (lambda (k*v)
         ((car k*v) k*v))))
  
  (define m00 '(lib "mm00.ss" "web-server" "default-web-root" "htdocs" "lang-servlets")) 
  (define m01 '(lib "mm01.ss" "web-server" "default-web-root" "htdocs" "lang-servlets")) 
  
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
      (test-case "Integers" (check-true (stuffed-url? (stuff-url (serialize 3) uri0))))
      (test-case "Symbols" (check-true (stuffed-url? (stuff-url (serialize 'foo) uri0))))
      (test-case "Strings" (check-true (stuffed-url? (stuff-url (serialize "Bar") uri0))))
      (test-case "Vectors" (check-true (stuffed-url? (stuff-url (serialize (vector 3 1 4)) uri0)))))
     
     (test-case 
      "Using stuff-url with lang.ss"
      (let-values ([(ev) (make-eval/mod-path m00)])
        (let* ([k0 (stuff-unstuff (ev '(serialize (dispatch-start start 'foo)))
                                  uri0)]
               [k1 (stuff-unstuff (ev `(serialize (dispatch ,the-dispatch (list (deserialize ',k0) 1))))
                                  uri0)]
               [k2 (stuff-unstuff (ev `(serialize (dispatch ,the-dispatch (list (deserialize ',k1) 2))))
                                  uri0)])
          (check-true (= 6 (ev `(dispatch ,the-dispatch (list (deserialize ',k2) 3)))))))))))