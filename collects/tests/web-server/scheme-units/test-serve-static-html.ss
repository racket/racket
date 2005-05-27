;; Mike Burns, July 28th, 2004, netgeek@speakeasy.net
;; Test whether a static, HTML file can be sent correctly.
(module test-serve-static-html mzscheme
  (require (lib "test.ss" "schemeunit")
           (lib "contract.ss")
           "assertions.ss"
           )

  (provide/contract
    (test-serve-static-html test-suite?))

  (define test-serve-static-html
    (make-test-suite
      "Test whether static HTML can be served"
      (make-test-case
        "Serve HTML explicitly"
        (assert-serve "/index.html"
                      (build-path web-root "htdocs" "index.html")
                      "text/html"))
      (make-test-case
        "Serve HTML implicitly"
        (assert-serve "/"
                      (build-path web-root "htdocs" "index.html")
                      "text/html"))))
  ;;; TODO test that additional indices work, too.
  )
