#lang scheme/base

(require syntax/module-reader)
(provide (rename-out [my-read read]
                     [my-read-syntax read-syntax]))

(define (my-read port modpath line col pos)
  (wrap-read-all 'scheme port read modpath (object-name port) line col pos))

(define (my-read-syntax src port modpath line col pos)
  (syntax-property
   (datum->syntax #f
                  (wrap-read-all 'scheme port (lambda (in) (read-syntax src in)) modpath src line col pos)
                  #f)
   'module-language
   '#(tests/racket/lang/getinfo get-info closure-data)))
