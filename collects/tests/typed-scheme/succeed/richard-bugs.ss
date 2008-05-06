#lang typed-scheme

(: x : '())
(define x '())


(define-typed-struct even-struct ([x : (U #f odd-struct)]))
(define-typed-struct odd-struct ([x : (U #f even-struct)]))
