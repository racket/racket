#lang racket
(require net/cgi (only-in net/uri-codec current-alist-separator-mode)
         tests/eli-tester)

(define (test-bindings mode query-string)
  (parameterize ([current-alist-separator-mode mode])
    (putenv "QUERY_STRING" query-string)
    (get-bindings)))

;; Runs `thunk` as a CGI script would and returns what it prints; an
;; error page from the library ends in `exit`, so that's caught here
(define (cgi-output query-string thunk)
  (putenv "QUERY_STRING" query-string)
  (with-output-to-string
    (lambda ()
      (let/ec k
        (parameterize ([exit-handler k])
          (thunk))))))

(provide tests)
(module+ main (tests))
(define (tests)
  (putenv "REQUEST_METHOD" "GET")
  (test (test-bindings 'amp-or-semi "key1=value1&key2=value2;key3=value3")
        => '([key1 . "value1"] [key2 . "value2"] [key3 . "value3"])
        (test-bindings 'amp "key1=value1&key2=value2")
        => '([key1 . "value1"] [key2 . "value2"])
        (test-bindings 'amp "key1=value1;key2=value2")
        => '([key1 . "value1;key2=value2"])
        (test-bindings 'semi "key1=value1;key2=value2")
        => '([key1 . "value1"] [key2 . "value2"])
        (test-bindings 'semi "key1=value1&key2=value2")
        => '([key1 . "value1&key2=value2"])
        ;; request data shown in an error page must be quoted as HTML
        (bindings-as-html '([<b> . "<i>&"] [key . "value"]))
        => '("<code>"
             "&lt;b&gt;&nbsp;--&gt;&nbsp;&lt;i&gt;&amp;<br>"
             "key&nbsp;--&gt;&nbsp;value<br>"
             "</code>")
        (regexp-match? #rx"<script>"
                       (cgi-output "key=%3Cscript%3E&key=value"
                                   (lambda ()
                                     (extract-binding/single 'key (get-bindings)))))
        => #f
        (regexp-match? #rx"No binding for `&lt;script&gt;' field[.]"
                       (cgi-output "key=value&<script>" get-bindings))
        => #t))

(module+ test (require (submod ".." main))) ; for raco test & drdr
