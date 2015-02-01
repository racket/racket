#lang racket
(require net/cookies/server
         net/cookies/common
         rackunit
         )

;; Based on tests from original net/cookie (JBM, 2006-12-01)
;; with additional from JMJ & porting to rackunit, 2015-02-01 - 2015-03-27.

(define rfc1123:date-template "~a, ~d ~b ~Y ~H:~M:~S GMT")
(define rfc850:date-template "~A, ~d-~b-~y ~H:~M:~S GMT")
(define asctime:date-template "~a ~b ~e ~H:~M:~S ~Y")

; Date/time used by server.rkt for expiring cookies:
(define clear-cookie-expiration-seconds 1420070400)
(define clear-cookie-expdate-string "Thu, 01 Jan 2015 00:00:00 GMT")

(module+ main
  (require rackunit/text-ui)
  (run-tests cookie-making-tests)
  (run-tests set-cookie-header-tests)
  (run-tests cookie-header-parsing-tests)
  (run-tests contract-tests))

(module+ test (require (submod ".." main))) ; for raco test & drdr

;; ctest : string cookie string -> test
(define (ctest message c expected)
  (test-equal? message (cookie->string c) expected))

;; date for testing "Expires=" attribute:
(define exp-date (seconds->date 1424050886 #f))
(define exp-date-str "Mon, 16 Feb 2015 01:41:26 GMT")

(define-test-suite cookie-making-tests
  (ctest "simple cookie, no A/V pairs" (make-cookie "a" "b") "a=b")
  (ctest "test each modifier individually: expires"
         (make-cookie "a" "b" #:expires exp-date)
         (format "a=b; Expires=~a" exp-date-str))
  (ctest "test each modifier individually: domain"
         (make-cookie "x" "y" #:domain "example.net")
         "x=y; Domain=example.net")
  (ctest "test each modifier individually: long domain"
         (make-cookie "x" "y"
                      #:domain
                      (string-append "r" (make-string 250 #\e)
                                     "allylong.hostname.example.net"))
         (string-append "x=y; Domain=r" (make-string 250 #\e)
                        "allylong.hostname.example.net"))
  (ctest "test each modifier individually: max-age"
         (make-cookie "x" "y" #:max-age 100)
         "x=y; Max-Age=100")
  (ctest "test each modifier individually: path"
         (make-cookie "x" "y" #:path "/") "x=y; Path=/")
  (ctest "test each modifier individually: longer path"
         (make-cookie "x" "y" #:path "/whatever/wherever/")
         "x=y; Path=/whatever/wherever/")
  (ctest "test each modifier individually: path with a plus"
         (make-cookie "x" "y" #:path "a+path")
         "x=y; Path=a+path")
  (ctest "test each modifier individually: path with quotes"
         (make-cookie "x" "y" #:path "\"/already/quoted/\"")
         "x=y; Path=\"/already/quoted/\"")
  (ctest "test each modifier individually: secure?"
         (make-cookie "x" "y" #:secure? #t)
         "x=y; Secure")
  (ctest "test each modifier individually: secure? = #f"
         (make-cookie "x" "y" #:secure? #f)
         "x=y")
  (ctest "test each modifier individually: http-only? = #t"
         (make-cookie "x" "y" #:http-only? #t)
         "x=y; HttpOnly")
  (ctest "test each modifier individually: http-only? = #f"
         (make-cookie "x" "y" #:http-only? #f)
         "x=y")
  (ctest "test each modifier individually: extension"
         (make-cookie "a" "b" #:extension "Comment=set+a+to+b")
         "a=b; Comment=set+a+to+b")
  (ctest "test each modifier individually: extension with spaces"
         (make-cookie "a" "b" #:extension "Comment=a comment with spaces")
         "a=b; Comment=a comment with spaces")
  (ctest "test each modifier individually: extension with escaped dquotes"
         (make-cookie "a" "b"
                      #:extension
                      "Comment=the \"risks\" involved in waking")
         "a=b; Comment=the \"risks\" involved in waking")
  (ctest "test each modifier individually: extension"
         (make-cookie "x" "y" #:extension "Version=12")
         "x=y; Version=12")
  (ctest "test each modifier individually: extension"
         (make-cookie "x" "y" #:extension "Omega=Lx.(x x) Lx.(x x)")
         "x=y; Omega=Lx.(x x) Lx.(x x)")

  (ctest "test combinations: ext/domain"
         (make-cookie "m" "n"
                      #:extension "Comment=set+a+to+b"
                      #:domain "example.net")
         "m=n; Domain=example.net; Comment=set+a+to+b")
  (ctest "test combinations: max-age/secure"
         (make-cookie "m" "n"
                      #:max-age 300
                      #:secure? #t)
         "m=n; Max-Age=300; Secure")
  (ctest "test combinations: expires/max-age"
         (make-cookie "a" "b" #:expires exp-date #:max-age 86400)
         (format "a=b; Expires=~a; Max-Age=86400" exp-date-str))
  (ctest "test combinations: path/ext/max-age"
         (make-cookie "m" "n"
                      #:path "/whatever/wherever/"
                      #:extension "Version=10"
                      #:max-age 20)
         "m=n; Max-Age=20; Path=/whatever/wherever/; Version=10"))

(define-test-suite set-cookie-header-tests
  ;; There aren't a lot of tests to do here, since currently all
  ;; that cookie->set-cookie-header does is apply string->bytes/utf-8
  ;; to the output of cookie->string, which is tested in cookie-making-tests.
  (test-equal? "header for setting a cookie"
               (cookie->set-cookie-header
                (make-cookie "rememberUser" "bob" #:path "/main"))
               #"rememberUser=bob; Path=/main")
  (test-equal? "header for clearing a cookie"
               (clear-cookie-header "foo")
               (string->bytes/utf-8
                (string-append "foo=; Expires=" clear-cookie-expdate-string))))

;; Cookie header parsing, starting w/examples from RFC6265:
(define-test-suite cookie-header-parsing-tests
  (test-equal? "parse to alist: 2 cookies"
               (cookie-header->alist #"SID=31d4d96e407aad42; lang=en-US")
               '((#"SID" . #"31d4d96e407aad42")
                 (#"lang" . #"en-US")))
  (test-equal? "parse to alist: 2 cookies"
               (cookie-header->alist #"SID=31d4d96e407aad42; lang=en-US"
                              bytes->string/utf-8)
               '(("SID" . "31d4d96e407aad42")
                 ("lang" . "en-US")))
  (test-equal? "parse to alist: many empty cookies"
               (cookie-header->alist #"a=; b=; c=; d="
                              bytes->string/utf-8)
               '(("a" . "") ("b" . "") ("c" . "") ("d" . "")))
  (test-equal? "parse to alist: fancy cookies"
               (cookie-header->alist
                #"a=fn(x); b=foo[2]; c=foo=fn(x){x+3}"
                              bytes->string/utf-8)
               '(("a" . "fn(x)")
                 ("b" . "foo[2]")
                 ("c" . "foo=fn(x){x+3}")))
  (test-equal? "parse to alist: 3 cookies, 1 empty, 1 w/escaped dquotes"
               (cookie-header->alist #"seenIntro=; uname=bob; nick=\"FuzzyDucky\"")
               '((#"seenIntro" . #"")
                 (#"uname" . #"bob")
                 (#"nick" . #"\"FuzzyDucky\"")))
  ;; No valid cookies:
  (test-equal? "no valid cookies" (cookie-header->alist #"; ; ; ") '())
  (test-equal? "no valid cookies" (cookie-header->alist #"=; ;a=b ; =5; x=y z; ")
               '())
  (test-equal? "no valid cookies"
               (cookie-header->alist #"=55; a=x\\y; b=x,y; c=x\"y; d=x y z;")
               '())
  ;; Some valid cookies:
  (test-case "some valid cookies"
    (check-equal? (cookie-header->alist #"x=y; =23") '((#"x" . #"y")))
    (check-equal? (cookie-header->alist #"x=y; a=b=c; p=q=r=s=t")
                  '((#"x" . #"y") (#"a" . #"b=c") (#"p" . #"q=r=s=t")))
    (check-equal? (cookie-header->alist #"; ;x=y ; a=b; =; c=d")
                  '((#"a" . #"b") (#"c" . #"d")))))

;; test error cases
(define-syntax contract-test
  (syntax-rules ()
    [(_ e) (check-exn exn:fail:contract? (lambda () e))]))

(define-test-suite contract-tests
  (contract-test (make-cookie "a" "b" #:extension "Comment=contains;semicolon"))
  (contract-test (make-cookie "x" "y" #:extension "IllegalCharacter=#\000"))
  (contract-test (make-cookie "x" "y" #:max-age 0))
  (contract-test (make-cookie "x" "y" #:max-age -10))
  (contract-test (make-cookie "x" "y" #:domain ""))
  (contract-test (make-cookie "x" "y" #:domain ".com"))
  (contract-test (make-cookie "x" "y" #:domain ".example.net"))
  (contract-test (make-cookie "x" "y" #:domain "emptypart..example.com"))
  (contract-test (make-cookie "x" "y" #:domain "bad domain.com"))
  (contract-test (make-cookie "x" "y" #:domain ".bad-domain;com")))

