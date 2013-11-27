#lang racket
(require net/uri-codec tests/eli-tester)

(provide tests)
(module+ main (tests))
(define (tests)
  (define sepmode current-alist-separator-mode)
  (test (uri-decode "%Pq") => "%Pq"
        (uri-decode "%P")  => "%P"
        (uri-decode "λ")   => "λ"

        (uri-decode (string-append
                     "\u631\u633\u627\u646\u647\u200c\u647\u627\u6cc\u2d\u62e\u648\u62f"
                     "\u645\u627\u646\u6cc\u2f\u62d\u642\u648\u642\u2d\u628\u634\u631\u2d"
                     "\u645\u6cc\u62f\u6cc\u627\u2f\u31\u32\u35\u34\u36\u2d"))
        => (string-append
            "\u631\u633\u627\u646\u647\u200c\u647\u627\u6cc\u2d\u62e\u648\u62f"
            "\u645\u627\u646\u6cc\u2f\u62d\u642\u648\u642\u2d\u628\u634\u631\u2d"
            "\u645\u6cc\u62f\u6cc\u627\u2f\u31\u32\u35\u34\u36\u2d")

        (alist->form-urlencoded '([a . "hel+lo \u7238"]))
        => "a=hel%2Blo+%E7%88%B8"
        (form-urlencoded->alist
         (alist->form-urlencoded '([a . "hel+lo \u7238"])))
        => '([a . "hel+lo \u7238"])
        (alist->form-urlencoded '([a . "hel+lo"] [b . "good-bye"]))
        => "a=hel%2Blo&b=good-bye"

        do (let ([alist   '([a . "hel+lo"] [b . "good-bye"])]
                 [ampstr  "a=hel%2Blo&b=good-bye"]
                 [semistr "a=hel%2Blo;b=good-bye"])
             (define (alist<->str mode str)
               (parameterize ([sepmode (or mode (sepmode))])
                 (test (alist->form-urlencoded alist) => str
                       (form-urlencoded->alist str) => alist)))
             (alist<->str #f           ampstr) ; test the default
             (alist<->str 'amp         ampstr)
             (alist<->str 'amp-or-semi ampstr)
             (alist<->str 'semi        semistr)
             (alist<->str 'semi-or-amp semistr))

        (form-urlencoded->alist "x=foo&y=bar;z=baz")
        => '([x . "foo"] [y . "bar"] [z . "baz"])
        (parameterize ([sepmode 'semi])
          (form-urlencoded->alist
           (parameterize ([sepmode 'amp])
             (alist->form-urlencoded '([a . "hel+lo"] [b . "good-bye"])))))
        => '([a . "hel+lo&b=good-bye"])
        (parameterize ([sepmode 'amp])
          (form-urlencoded->alist
           (parameterize ([sepmode 'semi])
             (alist->form-urlencoded '([a . "hel+lo"] [b . "good-bye"])))))
        => '([a . "hel+lo;b=good-bye"])

        (alist->form-urlencoded '([aNt . "Hi"]))
        => "aNt=Hi"
        (form-urlencoded->alist (alist->form-urlencoded '([aNt . "Hi"])))
        => '([aNt . "Hi"])
        (alist->form-urlencoded (form-urlencoded->alist "aNt=Hi"))
        => "aNt=Hi"

        (current-alist-separator-mode) => 'amp-or-semi
        (current-alist-separator-mode 'bad) =error> "expected argument of type"

        ;; Test all ASCII chars
        do
        (let ([p (for/list ([n (in-range 128)])
                   (let ([s (string (char-downcase (integer->char n)))])
                     (cons (string->symbol s) s)))])
          (test (form-urlencoded->alist (alist->form-urlencoded p)) => p)
          (let ([l (apply string-append (map cdr p))])
            (test (uri-decode (uri-encode l)) => l)))

        do (noels-tests)

        (uri-userinfo-encode "hello")         => "hello"
        (uri-userinfo-encode "hello there")   => "hello%20there"
        (uri-userinfo-encode "hello:there")   => "hello:there"
        (uri-userinfo-decode "hello")         => "hello"
        (uri-userinfo-decode "hello%20there") => "hello there"
        (uri-userinfo-decode "hello:there")   => "hello:there"

        ;; tried to choose characters from each subset:
        (uri-encode "M~(@; ")              =>  "M~(%40%3B%20"
        (uri-path-segment-encode "M~(@; ") =>  "M~(@%3B%20"
        (uri-userinfo-encode "M~(@; ")     =>  "M~(%40;%20"
        (uri-unreserved-encode "M~(@; ")   =>  "M~%28%40%3B%20"
        (uri-path-segment-unreserved-encode "M~(@; ") =>  "M~%28@%3B%20"
        ;; matching decodes:
        (uri-decode "M~(%40%3B%20")              =>  "M~(@; "
        (uri-path-segment-decode "M~(@%3B%20")   =>  "M~(@; "
        (uri-userinfo-decode "M~(%40;%20")       =>  "M~(@; "
        (uri-unreserved-decode "M~%28%40%3B%20") =>  "M~(@; "
        (uri-path-segment-unreserved-decode "M~%28@%3B%20")   =>  "M~(@; "

        (uri-path-segment-decode "M~%28@%3B%20")   =>  "M~(@; "
        (uri-path-segment-unreserved-decode "M~(@%3B%20")   =>  "M~(@; "
        (uri-encode "æçè") => "%C3%A6%C3%A7%C3%A8"
        (uri-encode "Kidô senkan Nadeshiko") => "Kid%C3%B4%20senkan%20Nadeshiko"
        ))

;; tests adapted from Noel Welsh's original test suite
(define (noels-tests)
  (define (pad2 str)
    (if (= (string-length str) 1) (string-append "0" str) str))
  (define (%hex n)
    (string-append "%" (pad2 (string-downcase (number->string n 16)))))
  (define (%HEX n)
    (string-append "%" (pad2 (string-upcase (number->string n 16)))))
  (test

   (uri-encode "hello") => "hello"
   (uri-encode "hello there") => "hello%20there"

   do
   (for ([code (in-range 128)])
     (if (or (member code '(33 39 40 41 42 45 46 95 126))
             (<= 48 code 57)   ; 0-9
             (<= 65 code 90)   ; A-Z
             (<= 97 code 122)) ; a-z
       (test (uri-encode (string (integer->char code)))
             => (string (integer->char code)))
       (test (uri-encode (string (integer->char code)))
             => (%HEX code))))

   (alist->form-urlencoded '()) => ""
   (alist->form-urlencoded '([key . "hello there"]))
   => "key=hello+there"
   (alist->form-urlencoded '([key1 . "hi"] [key2 . "hello"]))
   => "key1=hi&key2=hello"
   (alist->form-urlencoded '([key1 . "hello there"]))
   => "key1=hello+there"
   (uri-decode "hello")
   => "hello"
   (uri-decode "hello%20there")
   => "hello there"

   ;; these were going from 0 to 255 in Noel's original test suite.
   ;; Those fail here, however.
   do (for ([code (in-range 128)])
        (test (uri-decode (%HEX code)) => (string (integer->char code))
              (uri-decode (%hex code)) => (string (integer->char code))
              (uri-decode (string (integer->char code)))
              => (string (integer->char code))))

   ;; form-urlencoded->alist
   (form-urlencoded->alist "") => '()
   (form-urlencoded->alist "key=value")
   => '([key . "value"])
   (form-urlencoded->alist "key=hello+there")
   => '([key . "hello there"])
   (form-urlencoded->alist "key=a%20value")
   => '([key . "a value"])
   (form-urlencoded->alist "key")
   => '([key . #f])
   (form-urlencoded->alist "key1=value+1&key2=value+2")
   => '([key1 . "value 1"] [key2 . "value 2"])

   ))

(module+ test (require (submod ".." main))) ; for raco test & drdr
