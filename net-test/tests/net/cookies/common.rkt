#lang racket

(require rackunit net/cookies/common)

(module+ main
  (require rackunit/text-ui)
  (run-tests cookie-name-tests)
  (run-tests cookie-value-tests)
  (run-tests p/e-value-tests))

(define-syntax test-cookie-pred
  (syntax-rules (valid invalid)
    [(_ label ok? bytes-too? (valid v ...) (invalid inv ...))
     (test-begin
      (test-case (string-append "Valid " label)
                 (check-true (ok? v) v)
                 ...
                 (when bytes-too?
                   (check-true (ok? (string->bytes/utf-8 v)) v)
                   ...))
      (test-case (string-append "Invalid " label)
                 (check-false (ok? inv) inv)
                 ...
                 (when bytes-too?
                   (check-false (ok? (string->bytes/utf-8 inv)) inv)
                   ...)))]))

(define-test-suite cookie-name-tests
  (test-cookie-pred "cookie names" cookie-name? #t
    (valid "HI" "hi" "Hi" "modestlyLongCookieName"
           "somewhatTremendouslyOverlongCookieNameThatTakesAWhileToType")
    (invalid "(ugh)" "\"" "\"argh\"" "<tags>" "foo@bar"
             ",,,,,chameleon" "this;that" "this:that" "[bracketed]" "{braced}"
             "slashed/" "back\\slashed" "what?" "x=y" "spaced out" "\ttabbed")))

(define-test-suite cookie-value-tests
  (test-cookie-pred "cookie values" cookie-value? #t
    (valid "value" "(" "!" ")" ")!" "(!" "(!)" "!)" "\"hey!\"" "a=b=c")
    (invalid "a;b" "a,b" "a b" "a\tb" "a=\"foo\"")))

(define-test-suite p/e-value-tests
  (test-cookie-pred "path/extension values" path/extension-value? #f
    (valid "abc=123"
           "def=(define (forever x) (forever x))"
           "You're so \"cool\"")
    (invalid "x;y" "\000" (string #\rubout))))

(module+ test (require (submod ".." main))) ; for raco test & drdr
