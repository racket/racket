(module stuff-url-tests mzscheme
  (require (lib "stuff-url.ss" "web-server" "prototype-web-server" "private")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "util.ss" ("schematics" "schemeunit.plt" 2))
           (lib "url.ss" "net")
           (lib "dirs.ss" "setup")
           (lib "file.ss")
           (lib "etc.ss")
           "util.ss")
  
  (require/expose (lib "stuff-url.ss" "web-server" "prototype-web-server" "private")
                  (same-module? url-parts recover-serial))
  
  (provide stuff-url-suite)
  
  (define uri0 (string->url "www.google.com"))
  
  (define (simplify-unsimplify svl pth)
    (let-values ([(l-code simple-mod-map graph fixups sv)
                  (url-parts pth svl)])
      (recover-serial
       pth
       l-code
       simple-mod-map graph fixups sv)))
  
  (define (stuff-unstuff svl uri mod-path)
    (let ([result-uri (stuff-url svl uri mod-path)])
      (unstuff-url result-uri uri mod-path)))
  
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
      "Test same-module?"
      
      (check-true
       (same-module? `(file ,(path->string (build-absolute-path (find-collects-dir) "web-server" "prototype-web-server" "private" "abort-resume.ss")))
                     '(lib "abort-resume.ss" "web-server" "prototype-web-server" "private")))
      
      (check-true
       (same-module? `(file ,(path->string (build-absolute-path (this-expression-source-directory) "../private/abort-resume.ss")))
                     '(lib "abort-resume.ss" "web-server" "prototype-web-server" "private")))
      
      (check-true
       (same-module?
        '(lib "abort-resume.ss" "web-server" "prototype-web-server" "private")
        '(lib "./abort-resume.ss" "web-server" "prototype-web-server" "private"))))
     
     (test-case
      "compose url-parts and recover-serial (1)"
      (let-values ([(go ev) (make-eval/mod-path m00)])
        (go the-dispatch)
        (let* ([k0 (simplify-unsimplify (ev '(serialize (dispatch-start 'foo)))
                                        m00)]
               [k1 (simplify-unsimplify (ev `(serialize (dispatch (list (deserialize ',k0) 1))))
                                        m00)]
               [k2 (simplify-unsimplify (ev `(serialize (dispatch (list (deserialize ',k1) 2))))
                                        m00)])
          (check-true (= 6 (ev `(dispatch (list (deserialize ',k2) 3))))))))
     
     (test-case
      "compose url-parts and recover-serial (2)"
      (let-values ([(go ev) (make-eval/mod-path m01)])
        (go the-dispatch)
        (let* ([k0 (simplify-unsimplify (ev '(serialize (dispatch-start 'foo)))
                                        m01)])
          (check-true (= 7 (ev `(dispatch (list (deserialize ',k0) 7))))))))
     
     (test-case 
      "compose stuff-url and unstuff-url and recover the serial"
      (let-values ([(go ev) (make-eval/mod-path m00)])
        (go the-dispatch)
        (let* ([k0 (stuff-unstuff (ev '(serialize (dispatch-start 'foo)))
                                  uri0 m00)]
               [k1 (stuff-unstuff (ev `(serialize (dispatch (list (deserialize ',k0) 1))))
                                  uri0 m00)]
               [k2 (stuff-unstuff (ev `(serialize (dispatch (list (deserialize ',k1) 2))))
                                  uri0 m00)])
          (check-true (= 6 (ev `(dispatch (list (deserialize ',k2) 3)))))))))))