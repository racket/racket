#lang scheme
(require net/cgi
         net/uri-codec)

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

(putenv "REQUEST_METHOD" "GET")

(test-result (begin
              (current-alist-separator-mode 'amp-or-semi)
              (putenv "QUERY_STRING" "key1=value1&key2=value2;key3=value3")
              (get-bindings))
             '((key1 . "value1")
               (key2 . "value2")
               (key3 . "value3")))

(test-result (begin
              (current-alist-separator-mode 'amp)
              (putenv "QUERY_STRING" "key1=value1&key2=value2")
              (get-bindings))
             '((key1 . "value1")
               (key2 . "value2")))

(test-result (begin
              (current-alist-separator-mode 'amp)
              (putenv "QUERY_STRING" "key1=value1;key2=value2")
              (get-bindings))
             '((key1 . "value1;key2=value2")))

(test-result (begin
              (current-alist-separator-mode 'semi)
              (putenv "QUERY_STRING" "key1=value1;key2=value2")
              (get-bindings))
             '((key1 . "value1")
               (key2 . "value2")))

(test-result (begin
              (current-alist-separator-mode 'semi)
              (putenv "QUERY_STRING" "key1=value1&key2=value2")
              (get-bindings))
             '((key1 . "value1&key2=value2")))