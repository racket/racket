#lang scheme
(require web-server/http
         "lib.ss")

(define (next-name i)
  (values (format "input_~a" i) (add1 i)))
(define (input i)
  (let-values ([(w i) (next-name i)])
    (values (list `(input ([name ,w])))
            (lambda (env) (bindings-assq (string->bytes/utf-8 w) env))
            i)))

(define input-string
  (cross 
   (pure (lambda (bf)
           (bytes->string/utf-8 (binding:form-value bf))))
   input))

(define input-int
  (cross
   (pure string->number)
   input-string))

(define input-symbol
  (cross
   (pure string->symbol)
   input-string))

(provide/contract
 [input-string (formlet/c string?)]
 [input-int (formlet/c integer?)]
 [input-symbol (formlet/c symbol?)])