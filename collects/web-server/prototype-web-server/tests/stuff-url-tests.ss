(module stuff-url-tests mzscheme
  (require (lib "stuff-url.ss" "web-server" "prototype-web-server" "private")
           (lib "mod-map.ss" "web-server" "prototype-web-server" "private")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "url.ss" "net")
           "util.ss")
  
  (provide stuff-url-suite)
  
  (define uri0 (string->url "www.google.com"))  
  
  (define (simplify-unsimplify v)
    (decompress-serial
     (compress-serial
      v)))
  
  (define (stuff-unstuff svl uri)
    (let ([result-uri (stuff-url svl uri)])
      (unstuff-url result-uri)))
  
  (define the-dispatch
    `(lambda (k*v)
       (lambda (k*v)
         ((car k*v) k*v))))
  
  (define m00 '(lib "mm00.ss" "web-server" "prototype-web-server" "tests" "modules")) 
  (define m01 '(lib "mm01.ss" "web-server" "prototype-web-server" "tests" "modules")) 
  
  (define stuff-url-suite
    (test-suite
     "Tests for stuff-url.ss"     
     
     (test-case
      "compose url-parts and recover-serial (1)"
      (let-values ([(ev) (make-eval/mod-path m00)])
        (let* ([k0 (simplify-unsimplify (ev '(serialize (dispatch-start start 'foo))))]
               [k1 (simplify-unsimplify (ev `(serialize (dispatch ,the-dispatch (list (deserialize ',k0) 1)))))]
               [k2 (simplify-unsimplify (ev `(serialize (dispatch ,the-dispatch (list (deserialize ',k1) 2)))))])
          (check-true (= 6 (ev `(dispatch ,the-dispatch (list (deserialize ',k2) 3))))))))
     
     (test-case
      "compose url-parts and recover-serial (2)"
      (let-values ([(ev) (make-eval/mod-path m01)])
        (let* ([k0 (simplify-unsimplify (ev '(serialize (dispatch-start start 'foo))))])
          (check-true (= 7 (ev `(dispatch ,the-dispatch (list (deserialize ',k0) 7))))))))
     
     (test-case 
      "compose stuff-url and unstuff-url and recover the serial"
      (let-values ([(ev) (make-eval/mod-path m00)])
        (let* ([k0 (stuff-unstuff (ev '(serialize (dispatch-start start 'foo)))
                                  uri0)]
               [k1 (stuff-unstuff (ev `(serialize (dispatch ,the-dispatch (list (deserialize ',k0) 1))))
                                  uri0)]
               [k2 (stuff-unstuff (ev `(serialize (dispatch ,the-dispatch (list (deserialize ',k1) 2))))
                                  uri0)])
          (check-true (= 6 (ev `(dispatch ,the-dispatch (list (deserialize ',k2) 3)))))))))))