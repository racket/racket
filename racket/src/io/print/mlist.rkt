#lang racket/base
(require "write-with-max.rkt"
         "mode.rkt"
         "parameter.rkt"
         "config.rkt")

(provide print-mlist)

(define (print-mlist p who v mode o max-length graph config)
  (define unquoted? (eq? mode PRINT-MODE/UNQUOTED))
  (define curly? (and (not unquoted?)
                      (config-get config print-mpair-curly-braces)))
  (let ([max-length
         (cond
           [unquoted? (write-string/max "(mcons " o max-length)]
           [else (write-string/max (if curly? "{" "(") o max-length)])])
    (let loop ([v v] [max-length max-length])
      (cond
        [(eq? max-length 'full) 'full]
        [(and (null? (mcdr v))
              (not unquoted?))
         (let ([max-length (p who (mcar v) mode o max-length graph config)])
           (write-string/max (if curly? "}" ")") o max-length))]
        [(and (mpair? (mcdr v))
              (or (not graph) (not (hash-ref graph (mcdr v) #f)))
              (not unquoted?))
         (let ([max-length (p who (mcar v) mode o max-length graph config)])
           (loop (mcdr v) (write-string/max " " o max-length)))]
        [else
         (let* ([max-length (p who (mcar v) mode o max-length graph config)]
                [max-length (if unquoted?
                                (write-string/max " " o max-length)
                                (write-string/max " . " o max-length))]
                [max-length (p who (mcdr v) mode o max-length graph config)])
           (write-string/max (if curly? "}" ")") o max-length))]))))
