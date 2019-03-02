#lang at-exp racket/base

;; Mathias, added test for contracts on read-json 

(require json racket/string tests/eli-tester)
(require racket/port)

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
        (not (jsexpr? (/ 1.0 0.0)))
        (not (jsexpr? (/ -1.0 0.0)))
        (not (jsexpr? (/ 0.0 0.0)))
        (not (jsexpr? +inf.0))
        (not (jsexpr? -inf.0))
        (not (jsexpr? +nan.0))
        (not (jsexpr? +inf.f))
        (not (jsexpr? -inf.f))
        (not (jsexpr? +nan.f))
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
          => "\"\\u03bb\\u2200\\ud834\\udd1e\""
        ;; and that the same holds for keys
        (jsexpr->string (string->jsexpr "{\"\U0010FFFF\":\"\U0010FFFF\"}"))
          => "{\"\U0010FFFF\":\"\U0010FFFF\"}"
        (jsexpr->string (string->jsexpr "{\"\U0010FFFF\":\"\U0010FFFF\"}")
                        #:encode 'all)
          => "{\"\\udbff\\udfff\":\"\\udbff\\udfff\"}"
        ))

(define (parse-tests)
  (test (string->jsexpr @T{  0   }) =>  0
        (string->jsexpr @T{  1   }) =>  1
        (string->jsexpr @T{ -1   }) => -1 ; note: `+' is forbidden
        (string->jsexpr @T{ -12  }) => -12
        (string->jsexpr @T{ -123 }) => -123
        (string->jsexpr @T{  1.0 }) =>  1.0
        (string->jsexpr @T{  1.3 }) =>  1.3
        (string->jsexpr @T{  1.34}) =>  1.34
        (string->jsexpr @T{ -1.0 }) => -1.0
        (string->jsexpr @T{ -1.3 }) => -1.3
        (string->jsexpr @T{-10.34}) => -10.34
        (string->jsexpr @T{-10.34e3}) => -10340.0
        (string->jsexpr @T{-10.34e+3}) => -10340.0
        (string->jsexpr @T{-10.34e-3}) => -1.034e-2
        (string->jsexpr @T{-10.34e+31}) => -1.034e32
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
        (string->jsexpr @T{ [{"x": 1}, {"y": 2}] }) => 
                        '(#hasheq([x . 1]) #hasheq([y . 2]))

        ;; string escapes
        (string->jsexpr @T{ " \b\n\r\f\t\\\"\/ " }) => " \b\n\r\f\t\\\"/ "
        (string->jsexpr @T{ "\uD834\uDD1E" }) => "\U1D11E"
        (string->jsexpr @T{ "\ud834\udd1e" }) => "\U1d11e"
	;; INPUT PORT is optional 
	(with-input-from-string "[]" read-json)
	=> (parameterize ((json-null '())) (json-null))
        ;; EOF detection
        (for/list ([je (in-port read-json
                                (open-input-string
                                  @T{ 1 [2,3] "four" }))])
          je)
        => '(1 (2 3) "four")
        (string->jsexpr "]") =error> "string->jsexpr:"
        (string->jsexpr "foo") =error> "string->jsexpr:"
        (string->jsexpr "") => eof
        (string->jsexpr " \t\r\n") => eof

        ;; Invalid UTF-8 input.
        (bytes->jsexpr #"\"\377\377\377\"") =error> exn:fail?

        ;; More string escapes:
        (string->jsexpr @T{ "hel\"lo" }) => "hel\"lo"
        (string->jsexpr @T{ "\\//\\\\//" }) => "\\//\\\\//"
        (string->jsexpr @T{ ["one", "t\\w\\o", 3] }) => '("one" "t\\w\\o" 3)
        (string->jsexpr @T{ "\u000A" }) => "\u000A"
        (string->jsexpr @T{ "/" }) => "/"
        (string->jsexpr @T{ "\/" }) => "/"
        ;; More error tests:
        (string->jsexpr @T{ -}) =error> #rx"string->jsexpr:.*-"
        (string->jsexpr @T{ -x}) =error> #rx"string->jsexpr:.*-x"
        (string->jsexpr @T{ 1.}) =error> #rx"string->jsexpr:.*1[.]"
        (string->jsexpr @T{ 1.x }) =error> #rx"string->jsexpr:.*1[.]x"
        (string->jsexpr @T{ -1.}) =error> #rx"string->jsexpr:.*-1[.]"
        (string->jsexpr @T{ -1.x }) =error> #rx"string->jsexpr:.*-1[.]x"
        (string->jsexpr @T{ -13e }) =error> #rx"string->jsexpr:.*-13e"
        (string->jsexpr @T{ -1.3e }) =error> #rx"string->jsexpr:.*-1[.]3e"
        (string->jsexpr @T{ -1.3ex}) =error> #rx"string->jsexpr:.*-1[.]3e"
        (string->jsexpr @T{ 10.3ex}) =error> #rx"string->jsexpr:.*10[.]3e"
        (string->jsexpr @T{ [00] }) =error> "string->jsexpr:"
        (string->jsexpr @T{ [1,2,,3] }) =error> "string->jsexpr:"
        (string->jsexpr @T{ [1,2,3,] }) =error> "string->jsexpr:"
        (string->jsexpr @T{ {42 : "bad-key!"} }) =error> "string->jsexpr:"
        (string->jsexpr @T{ {'no-colon' , ""} }) =error> "string->jsexpr:"
        (string->jsexpr @T{ {"x":1,,"y":2} }) =error> "string->jsexpr:"
        (string->jsexpr @T{ {x:1, y:2} }) =error> "string->jsexpr:"
        (string->jsexpr " {x:1, y:2] ") =error> "string->jsexpr:"
        (string->jsexpr " [\"x\",1, \"y\",2} ") =error> "string->jsexpr:"
        (string->jsexpr @T{ truelove }) =error> "string->jsexpr:"
        (string->jsexpr @T{ truebred }) =error> "string->jsexpr:"
        (string->jsexpr @T{ truea }) =error> "string->jsexpr:"
        (string->jsexpr @T{ falsehood }) =error> "string->jsexpr:"
        (string->jsexpr @T{ falsetto }) =error> "string->jsexpr:"
        (string->jsexpr @T{ nullity }) =error> "string->jsexpr:"
        (string->jsexpr @T{ nulliparous }) =error> "string->jsexpr:"
        ))

(test do (pred-tests)
      do (print-tests)
      do (parse-tests))
