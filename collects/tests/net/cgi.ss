#lang scheme
(require net/cgi
         net/uri-codec
         tests/eli-tester)

(define-syntax test-result
  (syntax-rules ()
                [(test-result expression expected)
                 (let ([result expression])
                   (if (equal? result expected)
                       (display (format "Ok: `~a' evaluated to `~a'.\n"
                                        'expression expected))
                     (display (format
                               "Error: `~a' evaluated to `~a', expected `~a'.\n"
                               'expression result expected))))]))

(void (putenv "REQUEST_METHOD" "GET"))

(define (test-bindings mode query-string)
  (parameterize ([current-alist-separator-mode mode])
    (putenv "QUERY_STRING" query-string)
    (get-bindings)))

(test (test-bindings 'amp-or-semi "key1=value1&key2=value2;key3=value3")
      => '([key1 . "value1"] [key2 . "value2"] [key3 . "value3"])
      (test-bindings 'amp "key1=value1&key2=value2")
      => '([key1 . "value1"] [key2 . "value2"])
      (test-bindings 'amp "key1=value1;key2=value2")
      => '([key1 . "value1;key2=value2"])
      (test-bindings 'semi "key1=value1;key2=value2")
      => '([key1 . "value1"] [key2 . "value2"])
      (test-bindings 'semi "key1=value1&key2=value2")
      => '([key1 . "value1&key2=value2"]))
