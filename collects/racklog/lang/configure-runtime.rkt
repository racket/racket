#lang racket/base

(define (configure data)
  ;; (printf "Configuring\n")
  (current-read-interaction even-read))
(provide configure)

(require datalog/parse
         racklog/lang/compiler)

; XXX This is almost certainly wrong.
(define (even-read src ip)
  (begin0
    (compile-statement
     (parameterize ([current-source-name src])
       (parse-statement ip)))
    (current-read-interaction odd-read)))
(define (odd-read src ip)
  (current-read-interaction even-read)
  eof)
