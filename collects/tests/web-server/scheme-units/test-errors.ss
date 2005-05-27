;; Mike Burns, July 28th, 2004, netgeek@speakeasy.net
;; Test the error messages for both headers and content.
(module test-errors mzscheme
  (require (lib "test.ss" "schemeunit")
           (lib "url.ss" "net")
           "assertions.ss"
           )

  (provide test-errors)

  (define test-errors
    (make-test-suite
      "Test the error messages for both headers and content"

      (make-test-case
        "404 with an absolute filename"
        (assert-status-number "/does-not-exist" 404))

      (make-test-case
        "404 with a relative filename"
        (assert-status-number "does-not-exist" 404))

      (make-test-case
        "404 with an absolute directory name"
        (assert-status-number "/does-not-exist/" 404))

      (make-test-case
        "404 with an relative directory name"
        (assert-status-number "does-not-exist/" 404))

      (make-test-case
        "404 with an absolute servlet"
        (assert-status-number "/servlets/does-not-exist" 404))

      ; Not in suite.ss
      ;(make-test-case
      ; "Unit servlet not returning a response"
      ; ...)
      ;;; TODO check headers

      (make-test-case
        "Require failure"
        (assert-with-server
          "/servlets/bad-require.ss"
          (lambda (http-port)
            (purify-port http-port) ;; For the effect
            (input-port-equal? 
              http-port
              (open-input-string
                (format
                  (string-append
                    "Servlet didn't load.~ndefault-load-handler: cannot open "
                    "input file: \"~a\" (No such file or directory; "
                    "errno=2)")
                  (path->string
                    (build-path web-root
                                "servlets"
                                "I-do-not-exist.ss"))))))))

      (make-test-case
        "Exception raised"
        (assert-serve "/servlets/raise-exception.ss"
                      (path->string
                        (build-path web-root
                                    "htdocs"
                                    "servlet-output"
                                    "raise-exception.ss"))
                      "text/plain"))))

  )
