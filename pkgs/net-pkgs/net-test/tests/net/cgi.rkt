#lang racket
(require net/cgi (only-in net/uri-codec current-alist-separator-mode)
         tests/eli-tester)

(define (test-bindings mode query-string)
  (parameterize ([current-alist-separator-mode mode])
    (putenv "QUERY_STRING" query-string)
    (get-bindings)))

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
        => '([key1 . "value1&key2=value2"])))

(module+ test (require (submod ".." main))) ; for raco test & drdr
