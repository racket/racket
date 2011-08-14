#lang racket

(require htdp/convert)

(define (Fahrenheit->Celsius x) (* (/ 100 180) (- x 32)))

(convert-file "convert-drracket-non-error.txt" Fahrenheit->Celsius "out.dat")

(with-handlers ((exn:fail:read? void))
  "The input file contains a bad header. The next line should raise an exn."
  (convert-file "convert-drracket-error.txt" Fahrenheit->Celsius "out.dat")
  (raise `(test "this test should have failed but didn't")))
