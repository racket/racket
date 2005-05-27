;; Mike Burns, July 28th, 2004, netgeek@speakeasy.net
;; Test serving JPEGs. This also tests PR #6302.
(module test-serve-static-jpeg mzscheme
  (require (lib "contract.ss")
           (lib "test.ss" "schemeunit")
           "assertions.ss")

  (provide/contract
    (test-serve-static-jpeg test-suite?))

  (define test-serve-static-jpeg
    (make-test-suite
      "Test whether static JPEGs can be served"
      (make-test-case
        "Serve JPEG"
        (assert-serve "/me.jpg"
                      (build-path web-root "htdocs" "me.jpg")
                      "image/jpeg"))
      (make-test-case
        "Serve JPEG with upper-case \"extension\""
        (assert-serve "/me2.JPG"
                      (build-path web-root "htdocs" "me2.JPG")
                      "image/jpeg"))))

  )
