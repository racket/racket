#lang scheme/base

(require scheme/promise)

(provide output)
(define (output x [p (current-output-port)])
  (let loop ([x x])
    (cond [(or (void? x) (not x) (null? x)) (void)]
          [(pair? x) (loop (car x)) (loop (cdr x))]
          [(promise? x) (loop (force x))]
          [(keyword? x) (loop (keyword->string x))]
          [(and (procedure? x) (procedure-arity-includes? x 0)) (loop (x))]
          [(bytes? x)  (write-bytes x p)]
          [(string? x) (write-string x p)]
          [(char? x)   (write-char x p)]
          [(number? x) (write x p)]
          [(symbol? x) (display x p)]
          ;; generic fallback
          [else (error 'output "don't know how to render value: ~v" x)]))
  (void))
