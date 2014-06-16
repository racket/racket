#lang racket
(require racket/runtime-path
         htdp/convert)

(define-runtime-path non-error-pth "convert-drracket-non-error.txt")
(define-runtime-path error-pth "convert-drracket-error.txt")

(define (Fahrenheit->Celsius x) (* (/ 100 180) (- x 32)))

(convert-file non-error-pth Fahrenheit->Celsius
              (build-path (find-system-path 'temp-dir) "out.dat"))

(with-handlers ((exn:fail:read? void))
  "The input file contains a bad header. The next line should raise an exn."
  (convert-file error-pth Fahrenheit->Celsius
                (build-path (find-system-path 'temp-dir) "out.dat"))
  (raise `(test "this test should have failed but didn't")))
