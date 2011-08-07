#lang racket

(require htdp/convert)

(define (Fahrenheit->Celsius x) (* (/ 100 180) (- x 32)))

(with-handlers ((exn? (lambda (x)
                        (unless (cons? (regexp-match "DrRacket" (exn-message x)))
                          (error 'test "something went wrong with the test (1)")))))
  (convert-file "convert-drracket-error.txt" Fahrenheit->Celsius "out.dat")
  (error 'test "something went wrong with the test (2)"))