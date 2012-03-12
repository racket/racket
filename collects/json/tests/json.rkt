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
        (jsexpr? #\null) ; TODO
        (jsexpr? "")
        (jsexpr? "abc")
        (jsexpr? "abc\n\\")
        (jsexpr? '())
        (jsexpr? '(1 2 3))
        (jsexpr? '(1 "2" (3) #t #f #\null))
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
        ))

(define (print-tests)
  (for ([x (list 0 1 -1 12345 0.0 1.0 #t #f #\null "" "abc" "abc\n\\"
                 '() '(1 2 3) '(1 "2" (3) #t #f #\null) '((((()))))
                 '#hasheq()
                 '#hasheq([x . 1])
                 '#hasheq([x . 1] [y . 2])
                 ;; '#hasheq([|x\y| . 1] [y . 2]) ; TODO
                 )])
    (test (json->jsexpr (jsexpr->json x)) => x)))

(define (parse-tests)
  (test (json->jsexpr @T{  1   }) =>  1
        ;; (json->jsexpr @T{ +1   }) =>  1 ; TODO
        (json->jsexpr @T{ -1   }) => -1
        (json->jsexpr @T{  1.0 }) =>  1.0
        ;; (json->jsexpr @T{ +1.0 }) => +1.0 ; TODO
        (json->jsexpr @T{ -1.0 }) => -1.0
        (json->jsexpr @T{ true  }) => #t
        (json->jsexpr @T{ false }) => #f
        (json->jsexpr @T{ null  }) => #\null ; TODO
        (json->jsexpr @T{ [] }) => '()
        (json->jsexpr @T{ [1,[2],3] }) => '(1 (2) 3)
        (json->jsexpr @T{ [ 1 , [ 2 ] , 3 ] }) => '(1 (2) 3)
        (json->jsexpr @T{ [true, false, null] }) => '(#t #f #\null)
        (json->jsexpr @T{ {} }) => '#hasheq()
        (json->jsexpr @T{ {"x":1} }) => '#hasheq([x . 1])
        (json->jsexpr @T{ {"x":1,"y":2} }) => '#hasheq([x . 1] [y . 2])
        ))

(test do (pred-tests)
      do (print-tests)
      do (parse-tests))
