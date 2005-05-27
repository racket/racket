;; Mike Burns, July 28th, 2004, netgeek@speakeasy.net
;; Test servlets.
;; - Normal servlet call.
;; - Servlet call plus arguments on the URL
;; - Incremental servlets
;; - Various MIME formats
;; - URL paths
(module test-servlets mzscheme
  (require (lib "contract.ss")
           (lib "test.ss" "schemeunit")
           "assertions.ss"
           )

  (provide/contract
    (test-servlets test-suite?))

  (define test1-output "<html><head><title>Title</title></head></html>")

  (define test2-output
    (string-append
      "<html><head><title>Title</title></head><body><h1>Title</h1><p>Current "
      "path: " (path->string (build-path web-root "servlets"))
      "</p></body></html>"))

  (define test2-incremental-output
    (string-append
      "<html><head><title>Title</title></head><body><h1>Title</h1><p>Current "
      "path: " (path->string (build-path web-root "servlets"))
      "</p></body></html>"))

  (define test3-output "blah blah plain text")

  (define test4-output
    (string-append
      "<html><head><title>Title</title></head><body><h1>Title</h1><p>ab</p>"
      "<p>seed</p></body></html>"))

  (define test5-output
    (string-append
      "<html><head><title>Title</title></head><body><h1>Title</h1><p>ab</p>"
      "<p>seed</p><p>Current path: " (path->string
                                       (build-path web-root "servlets"))
      "</p></body></html>"))

  (define test6-output "abseed")

  (define test7-output (path->string (build-path web-root "servlets")))

  (define test8-output (string-append (path->string
                                        (build-path web-root "servlets"))
                                      "abseed"))

  (define test-servlets
    (make-test-suite

      ;; Non-incrementals
      (make-test-case
        (string-append
          "Non-incremental servlet with no arguments on the URL, "
          "in text/html, no URL path")
        (assert-serve/string "/servlets/test1.ss"
                      test1-output
                      "text/html"))

      (make-test-case
        (string-append
          "Non-incremental servlet with no arguments on the URL, "
          "in text/html, with URL path")
        (assert-serve/string "/servlets/test2.ss/home"
                      test2-output
                      "text/html"))

      (make-test-case
        (string-append
          "Non-incremental servlet with no arguments on the URL, "
          "in text/plain, no URL path")
        (assert-serve/string "/servlets/test3.ss"
                      test3-output
                      "text/plain"))

      (make-test-case
        (string-append
          "Non-incremental servlet with arguments on the URL, "
          "in text/html, no URL path")
        (assert-serve/string "/servlets/test4.ss?a=b&see=d"
                      test4-output
                      "text/html"))

      (make-test-case
        (string-append
          "Non-incremental servlet with arguments on the URL, "
          "in text/html, with URL path")
        (assert-serve/string "/servlets/test5.ss/home?a=b&see=d"
                      test5-output
                      "text/html"))

      (make-test-case
        (string-append
          "Non-incremental servlet with arguments on the URL, "
          "in text/plain, no URL path")
        (assert-serve/string "/servlets/test6.ss?a=b&see=d"
                      test6-output
                      "text/plain"))

      (make-test-case
        (string-append
          "Non-incremental servlet no arguments on the URL, "
          "in text/plain, with URL path")
        (assert-serve/string "/servlets/test7.ss/home"
                      test7-output
                      "text/plain"))

      (make-test-case
        (string-append
          "Non-incremental servlet with arguments on the URL, "
          "in text/plain, with URL path")
        (assert-serve/string "/servlets/test8.ss/home?a=b&see=d"
                      test8-output
                      "text/plain"))

      ;; Incrementals
      (make-test-case
        (string-append
          "Incremental servlet with no arguments on the URL, "
          "in text/html, no URL path")
        (assert-serve/string "/servlets/test1-incremental.ss"
                      test1-output
                      "text/html"))

      (make-test-case
        (string-append
          "Incremental servlet with no arguments on the URL, "
          "in text/html, with URL path")
        (assert-serve/string "/servlets/test2-incremental.ss/home"
                      test2-incremental-output
                      "text/html"))
      ;; Only the first two are tested incrementally.

      ;;; TODO
      ;;; - <form action="...?a=b;c=d" method="POST"> ... </form>

      ;; A servlet with an implicit send/back.
      (make-test-case
        "Implicit send/back"
        (let ((stop-server (start-server)))
          (let* ((p1 (get-pure-port
                       (string->url
                         (format "http://~a:~a/servlets/add.ss"
                                 THE-IP THE-PORT))))
                 (m1 (regexp-match #rx"action=\"([^\"]*)\"" p1))
                 (p2 (post-pure-port
                       (string->url
                         (format "http://~a:~a~a" THE-IP THE-PORT (cadr m1)))
                       #"number=1"
                       null))
                 (m2 (regexp-match #rx"action=\"([^\"]*)\"" p2))
                 (p3 (sync/timeout
                       5
                       (post-impure-port
                         (string->url
                           (format "http://~a:~a~a" THE-IP THE-PORT (cadr m2)))
                         #"number=2"
                         null))))
            (printf "p3 = ~s~n" p3)
            (if p3
              (begin0
                (begin
                  (purify-port p3)
                  (equal? (read-string 100 p3) add-output))
                (stop-server))
              (begin (stop-server) (fail))))))

      ))

  )
