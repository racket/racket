#lang at-exp racket/base

(require json racket/string tests/eli-tester)

(define T string-append)

(define (pred-tests)
  (test (jsexpr? 0)
        (jsexpr? 1)
        (jsexpr? -1)
        (jsexpr? 12345)
        (jsexpr? 0.0)
        (jsexpr? 1.0)
        (not (jsexpr? 1/2))
        (not (jsexpr? +i))
        (not (jsexpr? 'foo))
        (not (jsexpr? 'true))
        (jsexpr? #t)
        (jsexpr? #f)
        (jsexpr? 'null)
        (jsexpr? "")
        (jsexpr? "abc")
        (jsexpr? "abc\n\\")
        (jsexpr? '())
        (jsexpr? '(1 2 3))
        (jsexpr? '(1 "2" (3) #t #f null))
        (jsexpr? '((((())))))
        (not (jsexpr? '(1 2 . 3)))
        (not (jsexpr? '#(1 2 3)))
        (jsexpr? '#hasheq())
        (jsexpr? '#hasheq([x . 1]))
        (jsexpr? '#hasheq([x . 1] [y . 2]))
        (jsexpr? '#hash([x . 1] [y . 2])) ; fine as a jsexpr too
        (jsexpr? '#hasheq([|x\y| . 1] [y . 2]))
        (not (jsexpr? '#hasheq([1 . 1])))
        (not (jsexpr? '#hasheq(["x" . 1])))
        (not (jsexpr? '#hasheq(['() . 1])))
        )
  ;; other `null' values
  (parameterize ([json-null #\null])
    (test (not (jsexpr? '(1 "2" (3) #t #f null)))
          (jsexpr? '(1 "2" (3) #t #f #\null))
          )))

(define (print-tests)
  (for ([x (list 0 1 -1 12345 0.0 1.0 #t #f (λ(n) n) "" "abc" "abc\n\\"
                 '() '(1 2 3) (λ(n) `(1 "2" (3) #t #f ,n)) '((((()))))
                 '#hasheq()
                 '#hasheq([x . 1])
                 '#hasheq([x . 1] [y . 2])
                 '#hasheq([|x\y| . 1] [y . 2])
                 ;; string escapes
                 "λ" "\U1D11E" ; goes as a plain character in normal encoding
                 "\0" "\1" "\2" "\3" "\37" "\177" ; encoded as json \u escapes
                 "\b" "\n" "\r" "\f" "\t"         ; same escapes in both
                 "\a" "\v" "\e"                   ; does not use racket escapes
                 )])
    (define (N x null) (if (procedure? x) (x null) x))
    (test
     ;; default
     (string->jsexpr (jsexpr->string (N x 'null)))
     => (N x 'null)
     ;; different null
     (string->jsexpr (jsexpr->string (N x #\null) #:null #\null) #:null #\null)
     => (N x #\null)
     ;; encode all non-ascii
     (string->jsexpr (jsexpr->string (N x 'null) #:encode 'all))
     => (N x 'null)))
  ;; also test some specific expected encodings
  (test (jsexpr->string "\0\1\2\3") => "\"\\u0000\\u0001\\u0002\\u0003\""
        (jsexpr->string "\b\n\r\f\t\\\"") => "\"\\b\\n\\r\\f\\t\\\\\\\"\""
        (jsexpr->string "\37\40\177") => "\"\\u001f \\u007f\""
        (jsexpr->string "λ∀𝄞") => "\"λ∀𝄞\""
        (jsexpr->string "λ∀𝄞" #:encode 'all)
                               => "\"\\u03bb\\u2200\\ud834\\udd1e\""))

(define (parse-tests)
  (test (string->jsexpr @T{  1   }) =>  1
        (string->jsexpr @T{ -1   }) => -1 ; note: `+' is forbidden
        (string->jsexpr @T{  1.0 }) =>  1.0
        (string->jsexpr @T{ -1.0 }) => -1.0
        (string->jsexpr @T{ true  }) => #t
        (string->jsexpr @T{ false }) => #f
        (string->jsexpr @T{ null  }) => 'null
        (string->jsexpr @T{ ""    }) => ""
        (string->jsexpr @T{ "abc" }) => "abc"
        (string->jsexpr @T{ [] }) => '()
        (string->jsexpr @T{ [1,[2],3] }) => '(1 (2) 3)
        (string->jsexpr @T{ [ 1 , [ 2 ] , 3 ] }) => '(1 (2) 3)
        (string->jsexpr @T{ [true, false, null] }) => '(#t #f null)
        (string->jsexpr @T{ {} }) => '#hasheq()
        (string->jsexpr @T{ {"x":1} }) => '#hasheq([x . 1])
        (string->jsexpr @T{ {"x":1,"y":2} }) => '#hasheq([x . 1] [y . 2])
        ;; string escapes
        (string->jsexpr @T{ " \b\n\r\f\t\\\"\/ " }) => " \b\n\r\f\t\\\"/ "
        (string->jsexpr @T{ "\uD834\uDD1E" }) => "\U1D11E"
        (string->jsexpr @T{ "\ud834\udd1e" }) => "\U1d11e"
        ;; EOF detection
        (for/list ([je (in-port read-json
                                (open-input-string
                                  @T{ 1 [2,3] "four" }))])
          je)
        =>
        '(1 (2 3) "four")
        ))

(test do (pred-tests)
      do (print-tests)
      do (parse-tests))
