#lang racket

(require htdp/convert)

(define (Fahrenheit->Celsius x) (* (/ 100 180) (- x 32)))

(convert-file "convert-drracket-non-error.txt" Fahrenheit->Celsius "out.dat")

(with-handlers ((exn? (lambda (x)
                        (unless (cons? (regexp-match "DrRacket" (exn-message x)))
                          (error 'test "something went wrong with the test (1)")))))
  (convert-file "convert-drracket-error.txt" Fahrenheit->Celsius "out.dat")
  (raise `(test "something went wrong with the test (2)")))