(module stuff-url-tests mzscheme
  (require (lib "stuff-url.ss" "prototype-web-server")
           (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           (planet "util.ss" ("schematics" "schemeunit.plt" 1))
           (lib "url.ss" "net")
           (lib "file.ss")
           "language-tester.ss")
  
  (require/expose (lib "stuff-url.ss" "prototype-web-server")
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
  
  (define stuff-url-suite
    (make-test-suite
     "Tests for stuff-url.ss"
     
     (make-test-case
      "Test same-module?"
      
      (assert-true
       (same-module? (build-path "~/plt-exp/collects/prototype-web-server/abort-resume.ss")
                     '(lib "abort-resume.ss" "prototype-web-server")))
      
      (assert-true
       (same-module? (build-absolute-path (current-directory) "../abort-resume.ss")
                     '(lib "abort-resume.ss" "prototype-web-server")))
      
      (assert-true
       (same-module?
        '(lib "abort-resume.ss" "prototype-web-server")
        '(lib "./abort-resume.ss" "prototype-web-server"))))
     
     (make-test-case
      "compose url-parts and recover-serial (1)"
      (let* ([ev (make-eval/mod-path "modules/mm00.ss")]
             [k0 (simplify-unsimplify (ev '(serialize (dispatch-start 'foo))) "modules/mm00.ss")]
             [k1 (simplify-unsimplify (ev `(serialize (dispatch (list (deserialize ',k0) 1))))
                                      "modules/mm00.ss")]
             [k2 (simplify-unsimplify (ev `(serialize (dispatch (list (deserialize ',k1) 2))))
                                      "modules/mm00.ss")])
        (assert-true (= 6 (ev `(dispatch (list (deserialize ',k2) 3)))))))
     
     (make-test-case
      "compose url-parts and recover-serial (2)"
      (let* ([ev (make-eval/mod-path "modules/mm01.ss")]
             [k0 (simplify-unsimplify (ev '(serialize (dispatch-start 'foo))) "modules/mm01.ss")])
        (assert-true (= 7 (ev `(dispatch (list (deserialize ',k0) 7)))))))
     
     (make-test-case
      "compose stuff-url and unstuff-url and recover the serial"
      (let* ([ev (make-eval/mod-path "modules/mm00.ss")]
             [k0 (stuff-unstuff (ev '(serialize (dispatch-start 'foo))) uri0 "modules/mm00.ss")]
             [k1 (stuff-unstuff (ev `(serialize (dispatch (list (deserialize ',k0) 1))))
                                uri0 "modules/mm00.ss")]
             [k2 (stuff-unstuff (ev `(serialize (dispatch (list (deserialize ',k1) 2))))
                                uri0 "modules/mm00.ss")])
        (assert-true (= 6 (ev `(dispatch (list (deserialize ',k2) 3))))))))))