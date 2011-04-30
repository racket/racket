#lang racket
(require racklog
         racket/stxparam
         tests/eli-tester)

(define-syntax-parameter Y 
  (λ (stx)
    (raise-syntax-error stx 'Y "not allowed outside test-%is")))
(define-syntax (test-%is stx)
  (syntax-case stx ()
    [(_ e)
     (with-syntax ([the-y #'y])
       #`(test #:failure-prefix (format "~a" 'e)
               (test
                (%which (x)
                        (syntax-parameterize
                         ([Y (λ (stx) #'1)])
                         (%is x e))) => `([x . 1])
                                     (%more) => #f)
               #:failure-prefix (format "~a (let)" 'e)
               (test
                (%which (x) 
                        (%let (the-y) 
                              (%and (%= the-y 1) 
                                    (syntax-parameterize
                                     ([Y (make-rename-transformer #'the-y)])
                                     (%is x e)))))
                => `([x . 1])
                (%more) => #f)))]))

(define top-z 1)

(test
 (test-%is Y)
 (let ([z 1]) (test-%is z))
 (test-%is ((λ (x) x) Y))
 (test-%is ((λ (x) Y) 2))
 (test-%is ((case-lambda [(x) x]) Y))
 (test-%is ((case-lambda [(x) Y]) 2))
 (test-%is (+ 0 Y))
 (test-%is (if #t Y 2))
 (test-%is (if #f 2 Y))
 (test-%is (begin Y))
 (test-%is (begin0 Y 2))
 (test-%is (let ([z Y]) z))
 (test-%is (let ([z 2]) Y))
 (test-%is (letrec ([z Y]) z))
 (test-%is (letrec ([z 2]) Y))
 (let ([z 2])
   (test-%is (begin (set! z Y) z)))
 (test-%is '1)
 (%which (x) (%let (y) (%and (%= y 1) (%is x 'y)))) => `([x . y])
 (%more) => #f
 (%which (x) (%let (y) (%and (%= y 1) (%is x #'1)))) 
 ;=> `([x . ,#'1])
 (%more) => #f
 (%which (x) (%let (y) (%and (%= y 1) (%is x #'y))))
 ;=> `([x . ,#'y])
 (%more) => #f
 (test-%is (with-continuation-mark 'k 'v Y))
 (test-%is (with-continuation-mark 'k Y
             (first
              (continuation-mark-set->list
               (current-continuation-marks)
               'k))))
 (test-%is (with-continuation-mark Y Y
             (first
              (continuation-mark-set->list
               (current-continuation-marks)
               Y))))
 (test-%is (#%top . top-z))
 
 #;(test
    (test-%is (#%variable-reference Y))
    (let ([z 1]) (test-%is (#%variable-reference z)))
    (test-%is (#%variable-reference (#%top . top-z)))
    (%which (x) (%let (y) (%and (%= y 1) (%is x (#%variable-reference))))) => `([x . ,(#%variable-reference)])
    (%more) => #f)
 
 )
